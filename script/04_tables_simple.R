# 04_tables_simple.R
# ---------------------------------------------------------------------------
# Generates all frequency tables, balance scores, employment/formal-employment
# tables, hokim evaluation tables, Q7 problem tables, and income tables.
# Writes everything to a single multi-sheet Excel file.
#
# Requires: svy_design (from 03_weights_design.R),
#           q10_by_region_wide (from 04_q10_regex_tables.R)
# Key output:
#   tables/all_survey_data.xlsx — one sheet per table
# ---------------------------------------------------------------------------

library(dplyr)
library(srvyr)
library(tidyr)
library(readr)
library(writexl)
library(rlang)

stopifnot(exists("svy_design"))

# ===========================================================================
# Helpers
# ===========================================================================

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z / 10^digits * posneg
}

# --- Balance score helpers --------------------------------------------------

calculate_balance_score <- function(svy_design, question, pos_labels, group_var) {
  props <- svy_design %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(prop = survey_mean(na.rm = TRUE, vartype = NULL), .groups = "drop")

  props %>%
    group_by(!!sym(group_var)) %>%
    summarize(
      positive = sum(prop[!!sym(question) == pos_labels[1]], na.rm = TRUE),
      negative = sum(prop[!!sym(question) == pos_labels[2]], na.rm = TRUE),
      bs_score = (positive - negative) * 100 + 100,
      .groups = "drop"
    ) %>%
    select(!!sym(group_var), bs_score)
}

pos_labels_list <- list(
  q_1 = c("Яхшиланади", "Ёмонлашади"),
  q_2 = c("Ошди", "Пасайди"),
  q_3 = c("Кўпаяди", "Қисқаради"),
  q_4 = c("Кўпайди", "Камайди"),
  q_5 = c("Кўпаяди", "Камаяди"),
  q_6 = c("Ҳа", "Йўқ")
)

calculate_all_balance_scores <- function(svy_design, group_var) {
  all_scores <- lapply(names(pos_labels_list), function(q) {
    calculate_balance_score(svy_design, q, pos_labels_list[[q]], group_var) %>%
      rename(!!paste0(q, "_bs") := bs_score)
  })
  Reduce(function(x, y) merge(x, y, by = group_var, all = TRUE), all_scores)
}

calculate_final_scores <- function(balance_scores, group_var) {
  balance_scores %>%
    mutate(
      bs_score_cur = (q_2_bs + q_4_bs + q_6_bs) / 3,
      bs_score_fut = (q_1_bs + q_3_bs + q_5_bs) / 3,
      bs_gen       = (bs_score_cur + bs_score_fut) / 2
    ) %>%
    mutate(across(where(is.numeric), ~ round_half_up(., 0))) %>%
    arrange(desc(bs_gen)) %>%
    select(
      !!sym(group_var),
      "Умумий индекс" = bs_gen,
      "Жорий ҳолат"   = bs_score_cur,
      "Кутилмалар"    = bs_score_fut
    ) %>%
    as.data.frame(check.names = FALSE)
}

# --- Q1–Q6 frequency table with column ordering lookup ----------------------

col_order <- list(
  q_1 = c("Ёмонлашади", "Ўзгармайди", "Яхшиланади"),
  q_2 = c("Пасайди", "Ўзгармади", "Ошди"),
  q_3 = c("Қисқаради", "Ўзгармайди", "Кўпаяди"),
  q_4 = c("Камайди", "Ўзгармади", "Кўпайди"),
  q_5 = c("Камаяди", "Ўзгармайди", "Кўпаяди"),
  q_6 = c("Йўқ", "Билмайман", "Ҳа")
)

create_freq_table <- function(svy_design, question, group_var = "region") {
  result <- svy_design %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(!!sym(group_var), !!sym(question), percentage) %>%
    tidyr::pivot_wider(
      id_cols     = !!sym(group_var),
      names_from  = !!sym(question),
      values_from = percentage
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 0)))

  overall_label <- if (group_var == "district") {
    "Вилоят бўйича ўртача"
  } else {
    "Республика бўйича ўртача"
  }

  overall <- svy_design %>%
    group_by(!!sym(question)) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(group_var) := overall_label) %>%
    select(!!sym(group_var), !!sym(question), percentage) %>%
    tidyr::pivot_wider(
      id_cols     = !!sym(group_var),
      names_from  = !!sym(question),
      values_from = percentage
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 0)))

  out <- bind_rows(result, overall)

  # Safe column ordering via lookup
  if (question %in% names(col_order)) {
    out <- out %>% select(!!sym(group_var), all_of(col_order[[question]]))
  }

  out
}

# --- Employment table helper ------------------------------------------------

create_employment_table <- function(svy_design, group_var, filter_col, filter_val,
                                    status_col, round_digits = 0) {
  # Group-level percentages
  group_tbl <- svy_design %>%
    filter(.data[[filter_col]] != "Пенсиядаман" | !is.null(filter_val)) %>%
    { if (!is.null(filter_val)) filter(., .data[[filter_col]] == filter_val) else . } %>%
    group_by(.data[[group_var]], .data[[status_col]]) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(all_of(c(group_var, status_col, "percentage"))) %>%
    tidyr::pivot_wider(
      id_cols     = all_of(group_var),
      names_from  = all_of(status_col),
      values_from = percentage,
      values_fill = 0
    ) %>%
    mutate(across(any_of(c("Йўқ", "Ҳа")), ~ round(.x, round_digits)))

  # Overall row
  overall_label <- "Республика бўйича ўртача"
  overall_tbl <- svy_design %>%
    { if (!is.null(filter_val)) filter(., .data[[filter_col]] == filter_val)
      else filter(., .data[[filter_col]] != "Пенсиядаман") } %>%
    group_by(.data[[status_col]]) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(group_var) := overall_label) %>%
    select(all_of(c(group_var, status_col, "percentage"))) %>%
    tidyr::pivot_wider(
      id_cols     = all_of(group_var),
      names_from  = all_of(status_col),
      values_from = percentage,
      values_fill = 0
    ) %>%
    mutate(across(any_of(c("Йўқ", "Ҳа")), ~ round(.x, round_digits)))

  bind_rows(group_tbl, overall_tbl) %>%
    select(all_of(group_var), any_of(c("Йўқ", "Ҳа")))
}

# --- Hokim evaluation helper -----------------------------------------------

create_eval_table <- function(svy_design, score_col, group_var) {
  exclude_vals <- c("Бахолай олмайман", "Умуман танимайман")

  group_tbl <- svy_design %>%
    filter(!.data[[score_col]] %in% exclude_vals) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      mean_score = survey_mean(
        as.numeric(as.character(.data[[score_col]])),
        na.rm = TRUE, vartype = NULL
      ),
      .groups = "drop"
    ) %>%
    mutate(mean_score = round(mean_score, 1)) %>%
    arrange(desc(mean_score)) %>%
    rename("Ўртача баҳо" = mean_score)

  overall_tbl <- svy_design %>%
    filter(!.data[[score_col]] %in% exclude_vals) %>%
    summarize(
      mean_score = survey_mean(
        as.numeric(as.character(.data[[score_col]])),
        na.rm = TRUE, vartype = NULL
      )
    ) %>%
    mutate(
      mean_score = round(mean_score, 1),
      !!sym(group_var) := "Республика бўйича ўртача"
    ) %>%
    rename("Ўртача баҳо" = mean_score) %>%
    select(all_of(group_var), "Ўртача баҳо")

  bind_rows(group_tbl, overall_tbl)
}

# ===========================================================================
# Main logic
# ===========================================================================

# ----- Balance scores -------------------------------------------------------
region_balance_scores   <- calculate_all_balance_scores(svy_design, "region")
district_balance_scores <- calculate_all_balance_scores(svy_design, "district")
final_region_scores     <- calculate_final_scores(region_balance_scores, "region")
final_district_scores   <- calculate_final_scores(district_balance_scores, "district")

# Overall (republic) score
calculate_bs_for_question <- function(svy_design, question, pos_label, neg_label) {
  result <- svy_design %>%
    filter(!is.na(!!sym(question))) %>%
    group_by(!!sym(question)) %>%
    summarize(prop = survey_mean(na.rm = TRUE, vartype = NULL), .groups = "drop")
  positive <- result$prop[result[[as_name(ensym(question))]] == pos_label]
  negative <- result$prop[result[[as_name(ensym(question))]] == neg_label]
  (positive - negative) * 100 + 100
}

q1_bs <- calculate_bs_for_question(svy_design, "q_1", "Яхшиланади", "Ёмонлашади")
q2_bs <- calculate_bs_for_question(svy_design, "q_2", "Ошди", "Пасайди")
q3_bs <- calculate_bs_for_question(svy_design, "q_3", "Кўпаяди", "Қисқаради")
q4_bs <- calculate_bs_for_question(svy_design, "q_4", "Кўпайди", "Камайди")
q5_bs <- calculate_bs_for_question(svy_design, "q_5", "Кўпаяди", "Камаяди")
q6_bs <- calculate_bs_for_question(svy_design, "q_6", "Ҳа", "Йўқ")

bs_score_cur <- round_half_up((q2_bs + q4_bs + q6_bs) / 3, 0)
bs_score_fut <- round_half_up((q1_bs + q3_bs + q5_bs) / 3, 0)
bs_gen       <- round_half_up((bs_score_cur + bs_score_fut) / 2, 0)

overall_scores <- data.frame(
  region = "Республика бўйича",
  "Умумий индекс" = bs_gen,
  "Жорий ҳолат"   = bs_score_cur,
  "Кутилмалар"    = bs_score_fut,
  check.names = FALSE
)

final_region_scores_with_overall <- bind_rows(final_region_scores, overall_scores)

# ----- Q1–Q6 frequency tables -----------------------------------------------
regional_tables <- setNames(
  lapply(paste0("q_", 1:6), \(q) create_freq_table(svy_design, q, "region")),
  paste0("q_", 1:6, "_regional")
)

district_tables <- setNames(
  lapply(paste0("q_", 1:6), \(q) create_freq_table(svy_design, q, "district")),
  paste0("q_", 1:6, "_district")
)

# ----- Hokim evaluations (using helper) -------------------------------------
eval_region   <- create_eval_table(svy_design, "viloyat_hokimi", "region")
eval_district <- create_eval_table(svy_design, "tuman_hokimi", "district")

# ----- Employment (using helper) --------------------------------------------
employment_status <- create_employment_table(
  svy_design, group_var = "region",
  filter_col = "is_working", filter_val = NULL,
  status_col = "is_working"
)

employment_status_district <- svy_design %>%
  filter(is_working != "Пенсиядаман") %>%
  group_by(district, is_working) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  select(district, is_working, percentage) %>%
  tidyr::pivot_wider(
    id_cols     = district,
    names_from  = is_working,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(across(any_of(c("Йўқ", "Ҳа")), ~ round(.x, 0)))

# ----- Formal employment ---------------------------------------------------
formal_reg <- create_employment_table(
  svy_design, group_var = "region",
  filter_col = "is_working", filter_val = "Ҳа",
  status_col = "is_official"
)

formal_dis <- svy_design %>%
  filter(is_working == "Ҳа") %>%
  group_by(district, is_official) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  select(district, is_official, percentage) %>%
  tidyr::pivot_wider(
    id_cols     = district,
    names_from  = is_official,
    values_from = percentage
  ) %>%
  mutate(across(c("Йўқ", "Ҳа"), ~ round(., 0)))

# ----- Q7 frequency tables -------------------------------------------------
create_q7_frequency_dataframe <- function(svy_design, level = "district") {
  exclude_from_display <- c("Хаммаси кониктиради", "Муаммо йўқ", "muammo yoq", "muammo yo'q")
  overall_label <- if (level == "region") "Республика бўйича ўртача" else "Вилоят бўйича ўртача"

  group_freq <- svy_design %>%
    mutate(q_7 = trimws(q_7)) %>%
    filter(!q_7 %in% exclude_from_display) %>%
    group_by(!!sym(level), q_7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(!!sym(level), q_7, percentage) %>%
    tidyr::pivot_wider(
      id_cols     = !!sym(level),
      names_from  = q_7,
      values_from = percentage,
      values_fill = 0
    )

  overall <- svy_design %>%
    mutate(q_7 = trimws(q_7)) %>%
    filter(!q_7 %in% exclude_from_display) %>%
    group_by(q_7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(level) := overall_label) %>%
    select(!!sym(level), q_7, percentage) %>%
    tidyr::pivot_wider(
      id_cols     = !!sym(level),
      names_from  = q_7,
      values_from = percentage,
      values_fill = 0
    )

  bind_rows(group_freq, overall) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
}

q7_district_df <- create_q7_frequency_dataframe(svy_design, "district")
q7_regional_df <- create_q7_frequency_dataframe(svy_design, "region")

# ----- Income groups --------------------------------------------------------
categorize_income <- function(income) {
  income_numeric <- as.numeric(as.character(income))
  case_when(
    income_numeric == 0                          ~ "Даромади мавжуд эмас",
    income_numeric > 0 & income_numeric <= 1e6   ~ "1 млн сўмгача",
    income_numeric > 1e6 & income_numeric <= 3e6 ~ "1-3 млн",
    income_numeric > 3e6                         ~ "3 млн сўмдан баланд",
    TRUE                                         ~ NA_character_
  )
}

income_col_order <- c("Даромади мавжуд эмас", "1 млн сўмгача", "1-3 млн", "3 млн сўмдан баланд")

svy_income <- svy_design %>%
  mutate(
    income       = as.numeric(stringr::str_replace_all(income, " ", "")),
    income_group = categorize_income(income)
  ) %>%
  filter(!is.na(income_group))

overall_percentages <- svy_income %>%
  group_by(income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  mutate(region = "Республика бўйича ўртача") %>%
  select(region, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols     = region,
    names_from  = income_group,
    values_from = percentage
  ) %>%
  mutate(across(-region, ~ round(., 0))) %>%
  select(region, all_of(income_col_order))

inc_gen <- svy_income %>%
  group_by(region, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  select(region, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols     = region,
    names_from  = income_group,
    values_from = percentage
  ) %>%
  select(region, all_of(income_col_order)) %>%
  mutate(across(-region, ~ round(., 0))) %>%
  bind_rows(overall_percentages)

inc_dis <- svy_income %>%
  group_by(district, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  select(district, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols     = district,
    names_from  = income_group,
    values_from = percentage
  ) %>%
  select(district, all_of(income_col_order)) %>%
  mutate(across(-district, ~ round(., 0)))

# ===========================================================================
# Write all sheets
# ===========================================================================
out_list <- c(
  setNames(regional_tables, names(regional_tables)),
  setNames(district_tables, names(district_tables)),
  list(
    eval_region                = eval_region,
    eval_district              = eval_district,
    employment_status_regional = employment_status,
    employment_status_district = employment_status_district,
    formal_employment_regional = formal_reg,
    formal_employment_district = formal_dis,
    q7_regional                = q7_regional_df,
    q7_district                = q7_district_df,
    bs_regional                = final_region_scores_with_overall,
    bs_district                = final_district_scores,
    bs_district_scores         = district_balance_scores,
    income_regions             = inc_gen,
    income_tumanlar            = inc_dis,
    q10_by_region_percentages  = q10_by_region_wide
  )
)

dir.create(here::here("tables"), showWarnings = FALSE, recursive = TRUE)
writexl::write_xlsx(out_list, path = here::here("tables", "all_survey_data.xlsx"))
message(
  "✅ tables/all_survey_data.xlsx written with ",
  length(out_list), " sheets."
)
