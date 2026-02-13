
library(dplyr)
library(srvyr)
library(tidyr)
library(readr)
library(writexl)
library(rlang)

stopifnot(exists("svy_design"))

# --- helpers ---
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x); z <- abs(x) * 10^digits; z <- z + 0.5; z <- trunc(z); z / 10^digits * posneg
}

calculate_balance_score <- function(svy_design, question, pos_labels, group_var) {
  props <- svy_design %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(
      prop = survey_mean(na.rm = TRUE, vartype = NULL),
      .groups = "drop"
    )
  
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
  q_1 = c("Яхшиланади","Ёмонлашади"),
  q_2 = c("Ошди","Пасайди"),
  q_3 = c("Кўпаяди","Қисқаради"),
  q_4 = c("Кўпайди","Камайди"),
  q_5 = c("Кўпаяди","Камаяди"),
  q_6 = c("Ҳа","Йўқ")
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
      bs_gen = (bs_score_cur + bs_score_fut) / 2
    ) %>%
    mutate(across(where(is.numeric), ~ round_half_up(., 0))) %>%
    arrange(desc(bs_gen)) %>%
    select(!!sym(group_var),
           "Умумий индекс" = bs_gen,
           "Жорий ҳолат"   = bs_score_cur,
           "Кутилмалар"    = bs_score_fut) %>%
    as.data.frame(check.names = FALSE)
}

# --- frequency helper (q_1..q_6) ---
create_freq_table <- function(svy_design, question, group_var = "region") {
  # group %
  result <- svy_design %>%
    group_by(!!sym(group_var), !!sym(question)) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    select(!!sym(group_var), !!sym(question), percentage) %>%
    tidyr::pivot_wider(
      id_cols    = !!sym(group_var),
      names_from = !!sym(question),
      values_from= percentage
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 0)))
  
  # overall (Fix A: add ID first, explicit id_cols)
  overall <- svy_design %>%
    group_by(!!sym(question)) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
    mutate(!!sym(group_var) := "Республика бўйича ўртача") %>%
    select(!!sym(group_var), !!sym(question), percentage) %>%
    tidyr::pivot_wider(
      id_cols    = !!sym(group_var),
      names_from = !!sym(question),
      values_from= percentage
    ) %>%
    mutate(across(-!!sym(group_var), ~ round(., 0)))
  
  out <- bind_rows(result, overall)
  
  # Column ordering per question
  if (question == "q_1") out <- out %>% select(!!sym(group_var), "Ёмонлашади","Ўзгармайди","Яхшиланади")
  if (question == "q_2") out <- out %>% select(!!sym(group_var), "Пасайди","Ўзгармади","Ошди")
  if (question == "q_3") out <- out %>% select(!!sym(group_var), "Қисқаради","Ўзгармайди","Кўпаяди")
  if (question == "q_4") out <- out %>% select(!!sym(group_var), "Камайди","Ўзгармади","Кўпайди")
  if (question == "q_5") out <- out %>% select(!!sym(group_var), "Камаяди","Ўзгармайди","Кўпаяди")
  if (question == "q_6") out <- out %>% select(!!sym(group_var), "Йўқ","Билмайман","Ҳа")
  
  out
}

# ----- balance scores -----
region_balance_scores   <- calculate_all_balance_scores(svy_design, "region")
district_balance_scores <- calculate_all_balance_scores(svy_design, "district")
final_region_scores     <- calculate_final_scores(region_balance_scores, "region")
final_district_scores     <- calculate_final_scores(district_balance_scores, "district")


# overall (rep.) score (no SE)
calculate_bs_for_question <- function(svy_design, question, pos_label, neg_label) {
  result <- svy_design %>%
    filter(!is.na(!!sym(question))) %>%
    group_by(!!sym(question)) %>%
    summarize(prop = survey_mean(na.rm = TRUE, vartype = NULL), .groups = "drop")
  positive <- result$prop[result[[as_name(ensym(question))]] == pos_label]
  negative <- result$prop[result[[as_name(ensym(question))]] == neg_label]
  (positive - negative) * 100 + 100
}

q1_bs <- calculate_bs_for_question(svy_design, "q_1","Яхшиланади","Ёмонлашади")
q2_bs <- calculate_bs_for_question(svy_design, "q_2","Ошди","Пасайди")
q3_bs <- calculate_bs_for_question(svy_design, "q_3","Кўпаяди","Қисқаради")
q4_bs <- calculate_bs_for_question(svy_design, "q_4","Кўпайди","Камайди")
q5_bs <- calculate_bs_for_question(svy_design, "q_5","Кўпаяди","Камаяди")
q6_bs <- calculate_bs_for_question(svy_design, "q_6","Ҳа","Йўқ")

bs_score_cur <- round_half_up((q2_bs + q4_bs + q6_bs)/3, 0)
bs_score_fut <- round_half_up((q1_bs + q3_bs + q5_bs)/3, 0)
bs_gen       <- round_half_up((bs_score_cur + bs_score_fut)/2, 0)

overall_scores <- data.frame(
  region = "Республика бўйича",
  "Умумий индекс" = bs_gen,
  "Жорий ҳолат"   = bs_score_cur,
  "Кутилмалар"    = bs_score_fut,
  check.names = FALSE
)

final_region_scores_with_overall <- bind_rows(final_region_scores, overall_scores)

# ----- q1..q6 frequency tables -----
regional_tables <- setNames(
  lapply(paste0("q_",1:6), \(q) create_freq_table(svy_design, q, "region")),
  paste0("q_",1:6,"_regional")
)

district_tables <- setNames(
  lapply(paste0("q_",1:6), \(q) create_freq_table(svy_design, q, "district")),
  paste0("q_",1:6,"_district")
)

# ----- hokim evaluations (no SE kept) -----
eval_region <- svy_design %>%
  filter(viloyat_hokimi != "Бахолай олмайман", viloyat_hokimi != "Умуман танимайман") %>%
  group_by(region) %>%
  summarize(mean_score = survey_mean(as.numeric(as.character(viloyat_hokimi)), na.rm = TRUE, vartype = NULL),
            .groups="drop") %>%
  mutate(mean_score = round(mean_score, 1)) %>%
  arrange(desc(mean_score)) %>%
  rename("Ўртача баҳо" = mean_score) %>%
  select(region, "Ўртача баҳо")

overall_region <- svy_design %>%
  filter(viloyat_hokimi != "Бахолай олмайман", viloyat_hokimi != "Умуман танимайман") %>%
  summarize(mean_score = survey_mean(as.numeric(as.character(viloyat_hokimi)), na.rm = TRUE, vartype = NULL)) %>%
  mutate(mean_score = round(mean_score, 1),
         region = "Республика бўйича ўртача") %>%
  rename("Ўртача баҳо" = mean_score) %>%
  select(region, "Ўртача баҳо")
eval_region <- bind_rows(eval_region, overall_region)

eval_district <- svy_design %>%
  filter(tuman_hokimi != "Бахолай олмайман", tuman_hokimi != "Умуман танимайман") %>%
  group_by(district) %>%
  summarize(mean_score = survey_mean(as.numeric(as.character(tuman_hokimi)), na.rm = TRUE, vartype = NULL),
            .groups="drop") %>%
  mutate(mean_score = round(mean_score, 1)) %>%
  arrange(desc(mean_score)) %>%
  rename("Ўртача баҳо" = mean_score) %>%
  select(district, "Ўртача баҳо")

overall_district <- svy_design %>%
  filter(tuman_hokimi != "Бахолай олмайман", tuman_hokimi != "Умуман танимайман") %>%
  summarize(mean_score = survey_mean(as.numeric(as.character(tuman_hokimi)), na.rm = TRUE, vartype = NULL)) %>%
  mutate(mean_score = round(mean_score, 1),
         district = "Республика бўйича ўртача") %>%
  rename("Ўртача баҳо" = mean_score) %>%
  select(district, "Ўртача баҳо")
eval_district <- bind_rows(eval_district, overall_district)

# ----- Employment (regional + overall) -----
employment_status <- svy_design %>%
  filter(is_working != "Пенсиядаман") %>%
  group_by(region, is_working) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
  select(region, is_working, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = is_working,
    values_from= percentage
  ) %>%
  mutate(across(c("Йўқ","Ҳа"), ~ round(.,0))) %>%
  select(region, "Йўқ","Ҳа")

overall_employment <- svy_design %>%
  filter(is_working != "Пенсиядаман") %>%
  group_by(is_working) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
  mutate(region = "Республика бўйича ўртача") %>%
  select(region, is_working, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = is_working,
    values_from= percentage
  ) %>%
  mutate(across(c("Йўқ","Ҳа"), ~ round(.,0))) %>%
  select(region, "Йўқ","Ҳа")

employment_status <- bind_rows(employment_status, overall_employment)


employment_status_district <- svy_design %>%
  filter(is_working != "Пенсиядаман") %>%
  group_by(district, is_working) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  select(district, is_working, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = district,
    names_from = is_working,
    values_from= percentage,
    values_fill = 0
  ) %>%
  mutate(across(any_of(c("Йўқ","Ҳа")), ~ round(.x, 0)))

# ----- Formal employment (regional + overall + district) -----
formal_reg <- svy_design %>%
  filter(is_working == "Ҳа") %>%
  group_by(region, is_official) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
  select(region, is_official, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = is_official,
    values_from= percentage
  ) %>%
  mutate(across(c("Йўқ","Ҳа"), ~ round(.,0)))

overall_formal <- svy_design %>%
  filter(is_working == "Ҳа") %>%
  group_by(is_official) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups = "drop") %>%
  mutate(region = "Республика бўйича ўртача") %>%
  select(region, is_official, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = is_official,
    values_from= percentage,
    values_fill = 0                 # ensure both columns exist
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0))) %>%  # only numeric cols
  select(region, any_of(c("Йўқ", "Ҳа")))                 # safe even if one missing


formal_reg <- bind_rows(formal_reg, overall_formal)

formal_dis <- svy_design %>%
  filter(is_working == "Ҳа") %>%
  group_by(district, is_official) %>%
  summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
  select(district, is_official, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = district,
    names_from = is_official,
    values_from= percentage
  ) %>%
  mutate(across(c("Йўқ","Ҳа"), ~ round(.,0)))

# ----- Q7 frequency tables -----
create_q7_frequency_dataframe <- function(svy_design, level = "district") {
  exclude_from_display <- c("Хаммаси кониктиради","Муаммо йўқ","muammo yoq","muammo yo'q")
  
  # group
  group_freq <- svy_design %>%
    mutate(q_7 = trimws(q_7)) %>%
    filter(!q_7 %in% exclude_from_display) %>%
    group_by(!!sym(level), q_7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
    select(!!sym(level), q_7, percentage) %>%
    tidyr::pivot_wider(
      id_cols    = !!sym(level),
      names_from = q_7,
      values_from= percentage,
      values_fill= 0
    )
  
  # overall
  overall <- svy_design %>%
    mutate(q_7 = trimws(q_7)) %>%
    filter(!q_7 %in% exclude_from_display) %>%
    group_by(q_7) %>%
    summarize(percentage = survey_mean(vartype = NULL) * 100, .groups="drop") %>%
    mutate(!!sym(level) := if (level == "region") "Республика бўйича ўртача" else "Вилоят бўйича ўртача") %>%
    select(!!sym(level), q_7, percentage) %>%
    tidyr::pivot_wider(
      id_cols    = !!sym(level),
      names_from = q_7,
      values_from= percentage,
      values_fill= 0
    )
  
  bind_rows(group_freq, overall) %>%
    mutate(across(where(is.numeric), ~ round(.,1)))
}

q7_district_df <- create_q7_frequency_dataframe(svy_design, "district")
q7_regional_df <- create_q7_frequency_dataframe(svy_design, "region")

# ----- Income groups -----
categorize_income <- function(income) {
  income_numeric <- as.numeric(as.character(income))
  case_when(
    income_numeric == 0 ~ "Даромади мавжуд эмас",
    income_numeric > 0 & income_numeric <= 1000000 ~ "1 млн сўмгача",
    income_numeric > 1000000 & income_numeric <= 3000000 ~ "1-3 млн",
    income_numeric > 3000000 ~ "3 млн сўмдан баланд",
    TRUE ~ NA_character_
  )
}

svy_income <- svy_design %>%
  mutate(
    income = as.numeric(stringr::str_replace_all(income, " ", "")),
    income_group = categorize_income(income)
  ) %>%
  filter(!is.na(income_group))

overall_percentages <- svy_income %>%
  group_by(income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  mutate(region = "Республика бўйича ўртача") %>%
  select(region, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = income_group,
    values_from= percentage
  ) %>%
  mutate(across(-region, ~ round(., 0))) %>%
  select(region, "Даромади мавжуд эмас","1 млн сўмгача","1-3 млн","3 млн сўмдан баланд")

inc_gen <- svy_income %>%
  group_by(region, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  select(region, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = region,
    names_from = income_group,
    values_from= percentage
  ) %>%
  select(region, "Даромади мавжуд эмас","1 млн сўмгача","1-3 млн","3 млн сўмдан баланд") %>%
  mutate(across(-region, ~ round(.,0))) %>%
  bind_rows(overall_percentages)

inc_dis <- svy_income %>%
  group_by(district, income_group) %>%
  summarize(percentage = survey_mean(na.rm = TRUE, vartype = NULL) * 100, .groups = "drop") %>%
  select(district, income_group, percentage) %>%
  tidyr::pivot_wider(
    id_cols    = district,
    names_from = income_group,
    values_from= percentage
  ) %>%
  select(district, "Даромади мавжуд эмас","1 млн сўмгача","1-3 млн","3 млн сўмдан баланд") %>%
  mutate(across(-district, ~ round(.,0)))

# ----- write all sheets -----
out_list <- c(
  setNames(regional_tables, names(regional_tables)),
  setNames(district_tables, names(district_tables)),
  list(
    eval_region                 = eval_region,
    eval_district               = eval_district,
    employment_status_regional  = employment_status,
    employment_status_district  = employment_status_district,   # <- now a precomputed object
    formal_employment_regional  = formal_reg,
    formal_employment_district  = formal_dis,
    q7_regional                 = q7_regional_df,
    q7_district                 = q7_district_df,
    bs_regional                 = final_region_scores_with_overall,
    bs_district                 = final_district_scores ,
    bs_district_scores          = district_balance_scores,
    income_regions              = inc_gen,
    income_tumanlar             = inc_dis,
    q10_by_region_percentages   = q10_by_region_wide
   )
)

dir.create(here::here("tables"), showWarnings = FALSE, recursive = TRUE)
writexl::write_xlsx(out_list, path = here::here("tables", "all_survey_data.xlsx"))
message("✅ tables/all_survey_data.xlsx written.")
