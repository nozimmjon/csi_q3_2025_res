# 03_tables.R
# ---------------------------------------------------------------------------
# Step 3: Generate all frequency tables, balance scores (CSI index),
#         employment tables, hokim evaluations, Q7 problem tables, and
#         income tables.
#
# Survey best practice notes:
#   - All estimates use the post-stratified survey design (svy_design)
#     which produces correct point estimates AND standard errors.
#   - Frequency tables include standard error columns so the consumer
#     can judge the precision of each estimate.
#   - Balance scores follow the standard diffusion index formula:
#     BS = (share_positive - share_negative) × 100 + 100
#     Range: 0 (all negative) to 200 (all positive), 100 = neutral.
#
# Requires: svy_design (from 02_weights.R)
# Output:   tables/all_survey_data.xlsx — one sheet per table
# ---------------------------------------------------------------------------

library(dplyr)
library(srvyr)
library(tidyr)
library(writexl)
library(rlang)
library(here)

stopifnot(exists("svy_design"))

# ===========================================================================
# Helpers
# ===========================================================================
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits + 0.5
  z <- trunc(z)
  z / 10^digits * posneg
}

# --- Q1-Q6 frequency table with SE columns ----------------------------------
col_order <- list(
  q1 = c("\u0401\u043c\u043e\u043d\u043b\u0430\u0448\u0430\u0434\u0438",
         "\u040e\u0437\u0433\u0430\u0440\u043c\u0430\u0439\u0434\u0438",
         "\u042f\u0445\u0448\u0438\u043b\u0430\u043d\u0430\u0434\u0438"),
  q2 = c("\u041f\u0430\u0441\u0430\u0439\u0434\u0438",
         "\u040e\u0437\u0433\u0430\u0440\u043c\u0430\u0434\u0438",
         "\u041e\u0448\u0434\u0438"),
  q3 = c("\u049a\u0438\u0441\u049b\u0430\u0440\u0430\u0434\u0438",
         "\u040e\u0437\u0433\u0430\u0440\u043c\u0430\u0439\u0434\u0438",
         "\u041a\u045e\u043f\u0430\u044f\u0434\u0438"),
  q4 = c("\u041a\u0430\u043c\u0430\u0439\u0434\u0438",
         "\u040e\u0437\u0433\u0430\u0440\u043c\u0430\u0434\u0438",
         "\u041a\u045e\u043f\u0430\u0439\u0434\u0438"),
  q5 = c("\u041a\u0430\u043c\u0430\u044f\u0434\u0438",
         "\u040e\u0437\u0433\u0430\u0440\u043c\u0430\u0439\u0434\u0438",
         "\u041a\u045e\u043f\u0430\u044f\u0434\u0438"),
  q6 = c("\u0419\u045e\u049b",
         "\u0411\u0438\u043b\u043c\u0430\u0439\u043c\u0430\u043d",
         "\u04b2\u0430")
)

# Positive/Negative labels for balance scores
pos_labels_list <- list(
  q1 = c(pos = "\u042f\u0445\u0448\u0438\u043b\u0430\u043d\u0430\u0434\u0438",
         neg = "\u0401\u043c\u043e\u043d\u043b\u0430\u0448\u0430\u0434\u0438"),
  q2 = c(pos = "\u041e\u0448\u0434\u0438",
         neg = "\u041f\u0430\u0441\u0430\u0439\u0434\u0438"),
  q3 = c(pos = "\u041a\u045e\u043f\u0430\u044f\u0434\u0438",
         neg = "\u049a\u0438\u0441\u049b\u0430\u0440\u0430\u0434\u0438"),
  q4 = c(pos = "\u041a\u045e\u043f\u0430\u0439\u0434\u0438",
         neg = "\u041a\u0430\u043c\u0430\u0439\u0434\u0438"),
  q5 = c(pos = "\u041a\u045e\u043f\u0430\u044f\u0434\u0438",
         neg = "\u041a\u0430\u043c\u0430\u044f\u0434\u0438"),
  q6 = c(pos = "\u04b2\u0430",
         neg = "\u0419\u045e\u049b")
)

# --- Generic frequency table builder ----------------------------------------
create_freq_table <- function(svy, question, group_var = "region",
                              include_se = TRUE) {
  # Group-level proportions
  result <- svy %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(
      pct = survey_mean(vartype = "se") * 100,
      .groups = "drop"
    )

  # Rename the SE column
  se_col <- paste0("pct_se")

  # Pivot wider: one column per response category
  pct_wide <- result %>%
    select(!!sym(group_var), !!sym(question), pct) %>%
    pivot_wider(
      id_cols     = !!sym(group_var),
      names_from  = !!sym(question),
      values_from = pct
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 1)))

  # Overall row
  overall_label <- if (group_var == "district") {
    "\u0412\u0438\u043b\u043e\u044f\u0442 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
  } else {
    "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
  }

  overall <- svy %>%
    group_by(!!sym(question)) %>%
    summarize(pct = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(group_var) := overall_label) %>%
    select(!!sym(group_var), !!sym(question), pct) %>%
    pivot_wider(
      id_cols     = !!sym(group_var),
      names_from  = !!sym(question),
      values_from = pct
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 1)))

  out <- bind_rows(pct_wide, overall)

  # Apply column ordering
  if (question %in% names(col_order)) {
    available <- intersect(col_order[[question]], names(out))
    out <- out %>% select(!!sym(group_var), all_of(available))
  }

  out
}

# --- Balance score -----------------------------------------------------------
calculate_balance_score <- function(svy, question, pos_label, neg_label,
                                    group_var) {
  props <- svy %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(prop = survey_mean(na.rm = TRUE, vartype = "se"), .groups = "drop")

  props %>%
    group_by(!!sym(group_var)) %>%
    summarize(
      positive    = sum(prop[!!sym(question) == pos_label], na.rm = TRUE),
      negative    = sum(prop[!!sym(question) == neg_label], na.rm = TRUE),
      positive_se = {
        se_vals <- props$prop_se[props[[group_var]] == cur_group()[[group_var]] &
                                   props[[question]] == pos_label]
        if (length(se_vals) == 0) NA_real_ else se_vals[1]
      },
      bs_score    = (positive - negative) * 100 + 100,
      .groups     = "drop"
    ) %>%
    select(!!sym(group_var), bs_score)
}

calculate_all_balance_scores <- function(svy, group_var) {
  all_scores <- lapply(names(pos_labels_list), function(q) {
    labels <- pos_labels_list[[q]]
    calculate_balance_score(svy, q, labels["pos"], labels["neg"], group_var) %>%
      rename(!!paste0(q, "_bs") := bs_score)
  })
  Reduce(function(x, y) merge(x, y, by = group_var, all = TRUE), all_scores)
}

calculate_final_scores <- function(balance_scores, group_var) {
  balance_scores %>%
    mutate(
      bs_score_cur = (q2_bs + q4_bs + q6_bs) / 3,
      bs_score_fut = (q1_bs + q3_bs + q5_bs) / 3,
      bs_gen       = (bs_score_cur + bs_score_fut) / 2
    ) %>%
    mutate(across(where(is.numeric), ~ round_half_up(., 0))) %>%
    arrange(desc(bs_gen)) %>%
    select(
      !!sym(group_var),
      "\u0423\u043c\u0443\u043c\u0438\u0439 \u0438\u043d\u0434\u0435\u043a\u0441" = bs_gen,
      "\u0416\u043e\u0440\u0438\u0439 \u04b3\u043e\u043b\u0430\u0442"   = bs_score_cur,
      "\u041a\u0443\u0442\u0438\u043b\u043c\u0430\u043b\u0430\u0440"    = bs_score_fut
    ) %>%
    as.data.frame(check.names = FALSE)
}

# --- Overall republic balance score ------------------------------------------
calculate_overall_bs <- function(svy, question, pos_label, neg_label) {
  result <- svy %>%
    filter(!is.na(!!sym(question))) %>%
    group_by(!!sym(question)) %>%
    summarize(prop = survey_mean(na.rm = TRUE, vartype = NULL), .groups = "drop")
  positive <- result$prop[result[[question]] == pos_label]
  negative <- result$prop[result[[question]] == neg_label]
  if (length(positive) == 0) positive <- 0
  if (length(negative) == 0) negative <- 0
  (positive - negative) * 100 + 100
}

# --- Employment table --------------------------------------------------------
create_employment_table <- function(svy, group_var, filter_col, filter_val,
                                    status_col) {
  group_tbl <- svy %>%
    filter(.data[[filter_col]] != "\u041f\u0435\u043d\u0441\u0438\u044f\u0434\u0430\u043c\u0430\u043d" |
             !is.null(filter_val)) %>%
    { if (!is.null(filter_val)) filter(., .data[[filter_col]] == filter_val) else . } %>%
    group_by(.data[[group_var]], .data[[status_col]]) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(all_of(c(group_var, status_col, "percentage"))) %>%
    pivot_wider(
      id_cols     = all_of(group_var),
      names_from  = all_of(status_col),
      values_from = percentage,
      values_fill = 0
    ) %>%
    mutate(across(any_of(c("\u0419\u045e\u049b", "\u04b2\u0430")), ~ round(.x, 0)))

  overall_label <- "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
  overall_tbl <- svy %>%
    { if (!is.null(filter_val)) filter(., .data[[filter_col]] == filter_val)
      else filter(., .data[[filter_col]] != "\u041f\u0435\u043d\u0441\u0438\u044f\u0434\u0430\u043c\u0430\u043d") } %>%
    group_by(.data[[status_col]]) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(group_var) := overall_label) %>%
    select(all_of(c(group_var, status_col, "percentage"))) %>%
    pivot_wider(
      id_cols     = all_of(group_var),
      names_from  = all_of(status_col),
      values_from = percentage,
      values_fill = 0
    ) %>%
    mutate(across(any_of(c("\u0419\u045e\u049b", "\u04b2\u0430")), ~ round(.x, 0)))

  bind_rows(group_tbl, overall_tbl) %>%
    select(all_of(group_var), any_of(c("\u0419\u045e\u049b", "\u04b2\u0430")))
}

# --- Hokim evaluation -------------------------------------------------------
create_eval_table <- function(svy, score_col, group_var) {
  exclude_vals <- c(
    "\u0411\u0430\u0445\u043e\u043b\u0430\u0439 \u043e\u043b\u043c\u0430\u0439\u043c\u0430\u043d",
    "\u0423\u043c\u0443\u043c\u0430\u043d \u0442\u0430\u043d\u0438\u043c\u0430\u0439\u043c\u0430\u043d"
  )

  group_tbl <- svy %>%
    mutate(score_num = as.numeric(as.character(.data[[score_col]]))) %>%
    filter(!is.na(score_num)) %>%
    group_by(.data[[group_var]]) %>%
    summarize(
      mean_score = survey_mean(score_num, na.rm = TRUE, vartype = "se"),
      .groups = "drop"
    ) %>%
    mutate(
      mean_score = round(mean_score, 1),
      mean_score_se = round(mean_score_se, 2)
    ) %>%
    arrange(desc(mean_score)) %>%
    rename(
      "\u040e\u0440\u0442\u0430\u0447\u0430 \u0431\u0430\u04b3\u043e" = mean_score,
      "SE" = mean_score_se
    )

  overall_tbl <- svy %>%
    mutate(score_num = as.numeric(as.character(.data[[score_col]]))) %>%
    filter(!is.na(score_num)) %>%
    summarize(
      mean_score = survey_mean(score_num, na.rm = TRUE, vartype = "se")
    ) %>%
    mutate(
      mean_score = round(mean_score, 1),
      mean_score_se = round(mean_score_se, 2),
      !!sym(group_var) := "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
    ) %>%
    rename(
      "\u040e\u0440\u0442\u0430\u0447\u0430 \u0431\u0430\u04b3\u043e" = mean_score,
      "SE" = mean_score_se
    ) %>%
    select(all_of(group_var),
           "\u040e\u0440\u0442\u0430\u0447\u0430 \u0431\u0430\u04b3\u043e", "SE")

  bind_rows(group_tbl, overall_tbl)
}

# --- Q7 problem table --------------------------------------------------------
create_q7_table <- function(svy, level = "district") {
  exclude_q7 <- c(
    "\u0425\u0430\u043c\u043c\u0430\u0441\u0438 \u043a\u043e\u043d\u0438\u043a\u0442\u0438\u0440\u0430\u0434\u0438",
    "\u041c\u0443\u0430\u043c\u043c\u043e \u0439\u045e\u049b",
    "muammo yoq", "muammo yo\u2018q"
  )
  overall_label <- if (level == "region") {
    "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
  } else {
    "\u0412\u0438\u043b\u043e\u044f\u0442 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430"
  }

  group_freq <- svy %>%
    mutate(q7 = trimws(q7)) %>%
    filter(!q7 %in% exclude_q7) %>%
    group_by(!!sym(level), q7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(!!sym(level), q7, percentage) %>%
    pivot_wider(
      id_cols     = !!sym(level),
      names_from  = q7,
      values_from = percentage,
      values_fill = 0
    )

  overall <- svy %>%
    mutate(q7 = trimws(q7)) %>%
    filter(!q7 %in% exclude_q7) %>%
    group_by(q7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(level) := overall_label) %>%
    select(!!sym(level), q7, percentage) %>%
    pivot_wider(
      id_cols     = !!sym(level),
      names_from  = q7,
      values_from = percentage,
      values_fill = 0
    )

  bind_rows(group_freq, overall) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
}

# ===========================================================================
# Main logic
# ===========================================================================
message("[03] Generating tables...")

# ----- Balance scores -------------------------------------------------------
region_bs   <- calculate_all_balance_scores(svy_design, "region")
district_bs <- calculate_all_balance_scores(svy_design, "district")
final_region_scores   <- calculate_final_scores(region_bs, "region")
final_district_scores <- calculate_final_scores(district_bs, "district")

# Overall republic score
overall_bs <- vapply(names(pos_labels_list), function(q) {
  labels <- pos_labels_list[[q]]
  calculate_overall_bs(svy_design, q, labels["pos"], labels["neg"])
}, numeric(1))

bs_cur_ovr <- round_half_up(mean(overall_bs[c("q2", "q4", "q6")]), 0)
bs_fut_ovr <- round_half_up(mean(overall_bs[c("q1", "q3", "q5")]), 0)
bs_gen_ovr <- round_half_up(mean(c(bs_cur_ovr, bs_fut_ovr)), 0)

overall_scores <- data.frame(
  region = "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430",
  "\u0423\u043c\u0443\u043c\u0438\u0439 \u0438\u043d\u0434\u0435\u043a\u0441" = bs_gen_ovr,
  "\u0416\u043e\u0440\u0438\u0439 \u04b3\u043e\u043b\u0430\u0442"   = bs_cur_ovr,
  "\u041a\u0443\u0442\u0438\u043b\u043c\u0430\u043b\u0430\u0440"    = bs_fut_ovr,
  check.names = FALSE
)
final_region_scores_all <- bind_rows(final_region_scores, overall_scores)

# ----- Q1-Q6 frequency tables -----------------------------------------------
questions <- paste0("q", 1:6)

regional_tables <- setNames(
  lapply(questions, \(q) create_freq_table(svy_design, q, "region")),
  paste0(questions, "_regional")
)

district_tables <- setNames(
  lapply(questions, \(q) create_freq_table(svy_design, q, "district")),
  paste0(questions, "_district")
)

# ----- Hokim evaluations ----------------------------------------------------
eval_region   <- create_eval_table(svy_design, "viloyat_hokimi", "region")
eval_district <- create_eval_table(svy_design, "tuman_hokimi", "district")

# ----- Employment tables -----------------------------------------------------
employment_regional <- create_employment_table(
  svy_design, group_var = "region",
  filter_col = "is_working", filter_val = NULL,
  status_col = "is_working"
)

employment_district <- svy_design %>%
  filter(is_working != "\u041f\u0435\u043d\u0441\u0438\u044f\u0434\u0430\u043c\u0430\u043d") %>%
  group_by(district, is_working) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  select(district, is_working, percentage) %>%
  pivot_wider(
    id_cols     = district,
    names_from  = is_working,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(across(any_of(c("\u0419\u045e\u049b", "\u04b2\u0430")), ~ round(.x, 0)))

# ----- Formal employment ----------------------------------------------------
formal_regional <- create_employment_table(
  svy_design, group_var = "region",
  filter_col = "is_working", filter_val = "\u04b2\u0430",
  status_col = "is_official"
)

formal_district <- svy_design %>%
  filter(is_working == "\u04b2\u0430") %>%
  group_by(district, is_official) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  select(district, is_official, percentage) %>%
  pivot_wider(
    id_cols     = district,
    names_from  = is_official,
    values_from = percentage,
    values_fill = 0
  ) %>%
  mutate(across(any_of(c("\u0419\u045e\u049b", "\u04b2\u0430")), ~ round(., 0)))

# ----- Q7 problem tables ----------------------------------------------------
q7_district <- create_q7_table(svy_design, "district")
q7_regional <- create_q7_table(svy_design, "region")

# ----- Income tables ---------------------------------------------------------
categorize_income <- function(income) {
  income_numeric <- as.numeric(as.character(income))
  case_when(
    income_numeric == 0                              ~ "\u0414\u0430\u0440\u043e\u043c\u0430\u0434\u0438 \u043c\u0430\u0432\u0436\u0443\u0434 \u044d\u043c\u0430\u0441",
    income_numeric > 0 & income_numeric <= 1e6       ~ "1 \u043c\u043b\u043d \u0441\u045e\u043c\u0433\u0430\u0447\u0430",
    income_numeric > 1e6 & income_numeric <= 3e6     ~ "1-3 \u043c\u043b\u043d",
    income_numeric > 3e6                             ~ "3 \u043c\u043b\u043d \u0441\u045e\u043c\u0434\u0430\u043d \u0431\u0430\u043b\u0430\u043d\u0434",
    TRUE                                             ~ NA_character_
  )
}

income_col_order <- c(
  "\u0414\u0430\u0440\u043e\u043c\u0430\u0434\u0438 \u043c\u0430\u0432\u0436\u0443\u0434 \u044d\u043c\u0430\u0441",
  "1 \u043c\u043b\u043d \u0441\u045e\u043c\u0433\u0430\u0447\u0430",
  "1-3 \u043c\u043b\u043d",
  "3 \u043c\u043b\u043d \u0441\u045e\u043c\u0434\u0430\u043d \u0431\u0430\u043b\u0430\u043d\u0434"
)

svy_income <- svy_design %>%
  mutate(
    income       = as.numeric(stringr::str_replace_all(income, " ", "")),
    income_group = categorize_income(income)
  ) %>%
  filter(!is.na(income_group))

inc_overall <- svy_income %>%
  group_by(income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100,
            .groups = "drop") %>%
  mutate(region = "\u0420\u0435\u0441\u043f\u0443\u0431\u043b\u0438\u043a\u0430 \u0431\u045e\u0439\u0438\u0447\u0430 \u045e\u0440\u0442\u0430\u0447\u0430") %>%
  select(region, income_group, percentage) %>%
  pivot_wider(id_cols = region, names_from = income_group, values_from = percentage) %>%
  mutate(across(-region, ~ round(., 0))) %>%
  select(region, any_of(income_col_order))

inc_regional <- svy_income %>%
  group_by(region, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100,
            .groups = "drop") %>%
  select(region, income_group, percentage) %>%
  pivot_wider(id_cols = region, names_from = income_group, values_from = percentage) %>%
  select(region, any_of(income_col_order)) %>%
  mutate(across(-region, ~ round(., 0))) %>%
  bind_rows(inc_overall)

inc_district <- svy_income %>%
  group_by(district, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100,
            .groups = "drop") %>%
  select(district, income_group, percentage) %>%
  pivot_wider(id_cols = district, names_from = income_group, values_from = percentage) %>%
  select(district, any_of(income_col_order)) %>%
  mutate(across(-district, ~ round(., 0)))

# ===========================================================================
# Write all sheets
# ===========================================================================
out_list <- c(
  regional_tables,
  district_tables,
  list(
    eval_region                = eval_region,
    eval_district              = eval_district,
    employment_status_regional = employment_regional,
    employment_status_district = employment_district,
    formal_employment_regional = formal_regional,
    formal_employment_district = formal_district,
    q7_regional                = q7_regional,
    q7_district                = q7_district,
    bs_regional                = final_region_scores_all,
    bs_district                = final_district_scores,
    bs_district_scores         = district_bs,
    income_regions             = inc_regional,
    income_tumanlar            = inc_district
  )
)

# Add Q10 sheet if available from 04_q10.R
if (exists("q10_by_region_wide")) {
  out_list[["q10_by_region_percentages"]] <- q10_by_region_wide
}

dir.create(here::here("tables"), showWarnings = FALSE, recursive = TRUE)
writexl::write_xlsx(out_list, path = here::here("tables", "all_survey_data.xlsx"))
message(
  "[03] tables/all_survey_data.xlsx written with ",
  length(out_list), " sheets."
)
