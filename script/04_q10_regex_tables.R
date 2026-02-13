# 05_q10_regex_tables.R
# Purpose: Q10 regex buckets (overall + by region), using existing srvyr design `svy_design`.

library(dplyr)
library(stringr)
library(srvyr)
library(tidyr)
library(scales)
library(writexl)
library(rlang)

stopifnot(exists("svy_design"))

# --- 1) Light normalizer (do NOT change 'Хеч' vs 'Ҳеч') ---------------------
normalize_uzbek <- function(x) {
  x %>%
    str_replace_all('[“”"]', "") %>%
    str_replace_all("[’ʼ`′´’']", "’") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_replace_all("Маҳалл+ларни", "Маҳаллаларни") %>%
    str_replace_all("Асосий йўл сифати яхшиланди", "Асосий йўлларнинг сифати ошди") %>%
    str_replace_all("Фуқора", "Фуқаро") %>%
    str_replace_all("Фуқаро билмаслигни айтди", "Фуқаро билмаслигини айтди")
}

# --- 2) Regex buckets --------------------------------------------------------
cat_patterns <- list(
  "Ичимлик сув таъминоти яхшиланди" =
    "(Ичимлик\\s*сув(и)?\\s*(келди|яхшилан(ди)?))|(^Сув\\s*таъминот(и)?\\s*яхшилан(ди)?$)|Сув\\s*келди\\s*симёғоч\\s*ўрнатилди",
  "Электр энергия таъминоти яхшиланди" =
    "Электр(\\s*энергия)?\\s*таъминот(и)?\\s*яхшилан(ди)?",
  "Газ таъминоти яхшиланди" =
    "Газ\\s*таъминот(и)?\\s*яхшилан(ди)?",
  "Ички йўллар сифатининг ошиши" =
    "Ички\\s*йўллар\\s*сифат(и|ининг)\\s*ошиш(и)?",
  "Асосий йўлларнинг сифати ошди" =
    "Асосий\\s*йўл(лар(ининг)?|)\\s*сифат(и|ининг).*(ошд(и)?|яхшилан(ди)?)",
  "Уй-жой қурилиши кучайди" =
    "Уй-?жой\\s*қурилиш(и)?\\s*кучайд(и)?",
  "Аҳолини уй-жой билан таъминлаш ишлари" =
    "Аҳолини\\s*уй-жой\\s*билан\\s*таъми(н|но)лаш",
  "Кредит олиш осонлашди" =
    "Кредит\\s*олиш\\s*осонлашд(и)?",
  "Тадбиркорларни қўллаб-қувватлаш ишларини" =
    "Тадбиркорлар(ни)?\\s*қўллаб-қувватлаш",
  "Маҳаллаларни ободонлаштириш ишлари кучайди" =
    "Маҳаллаларни\\s*ободонлаштириш.*кучайд(и)?",
  "Жамоат транспорти яхшиланди" =
    "Жамоат\\s*транспорти\\s*яхшилан(ди)?",
  "Мактабда таълим сифатини ошириш борасидаги ишлар" =
    "Мактабда\\s*таълим\\s*сифатини\\s*ошириш",
  "Мактабгача таълим тизими билан қамров даражасини ошириш борасидаги ишлар" =
    "Мактабгача\\s*таълим",
  "Соғлиқни сақлаш хизматлари сифатини ошириш борасидаги ишлар" =
    "Соғлиқни\\s*сақлаш\\s*хизматлари\\s*сифатини\\s*ошириш",
  "Аҳолини иш билан таъминлаш борасидаги ишлар" =
    "Аҳолини\\s*иш\\s*билан\\s*таъми(н|но)лаш|Иш\\s*ўринлар(и)?\\s*кўпайд(и)?",
  "Ишлаб чиқариш яхшиланди" =
    "Ишлаб\\s*чиқариш\\s*(яхшилан(ган|ди)|кучайди|кўпайди|соҳаси\\s*яхшиланди)",
  "Қишлоқ хўжалиги яхшиланди" =
    "Қишлоқ\\s*х[ўо]жалиг(и|идаги)",
  "Чорвачилик ривожланди" =
    "Чорвачил(ик|к)",
  "Савдо-сотиқ соҳаси яхшиланди" =
    "Савдо-сотиқ(\\s*соҳаси)?\\s*яхшиланди",
  "Дўконлар кўпайди" =
    "Дўконлар\\s*(кўпайди|қурилди)",
  "Билмайди" =
    "Билмаслигини\\s*айтди|Билмайди|Аниқ\\s*жавоб\\s*бера\\s*олмади"
)

build_detector <- function(pat) regex(pat, ignore_case = FALSE)

# --- 3) Prep once (no re-wrapping) ------------------------------------------
# Use svy_design directly; just normalize and keep respondents with an answer
svy_q10 <- svy_design %>%
  mutate(
    q10_norm     = normalize_uzbek(q_10),
    has_answer   = !is.na(q10_norm) & q10_norm != "",
    region_clean = region %>% str_replace_all("[\\r\\n\\t]+", " ") %>% str_squish()
  ) %>%
  filter(has_answer, !is.na(region_clean), region_clean != "")

# Exact match for “no change”
no_change_label <- "Хеч кандай узгаришлар сезмадим"
svy_q10 <- svy_q10 %>%
  mutate(`Хеч кандай узгаришлар сезмадим` := as.integer(q10_norm == no_change_label))

# Add regex indicators
for (nm in names(cat_patterns)) {
  svy_q10 <- svy_q10 %>%
    mutate(!!nm := as.integer(str_detect(q10_norm, build_detector(cat_patterns[[nm]]))))
}

all_cats <- c("Хеч кандай узгаришлар сезмадим", names(cat_patterns))

# --- 4) Overall weighted table ----------------------------------------------
q10_overall <- svy_q10 %>%
  summarise(
    across(all_of(all_cats), ~ survey_mean(.x, vartype = NULL, na.rm = TRUE), .names = "{.col}"),
    respondents_wt    = survey_total(1),
    respondents_unwtd = unweighted(n())
  ) %>%
  pivot_longer(all_of(all_cats), names_to = "category", values_to = "pct_of_respondents") %>%
  arrange(desc(pct_of_respondents)) %>%
  mutate(
    pct_label      = percent(pct_of_respondents, accuracy = 0.1),
    wt_n_resp      = as.numeric(respondents_wt),
    weighted_count = pct_of_respondents * wt_n_resp
  )

# --- 5) Regional percentages (wide with explicit ID) -------------------------
q10_by_region_long <- svy_q10 %>%
  group_by(region_clean) %>%
  summarise(
    across(all_of(all_cats), ~ survey_mean(.x, vartype = NULL, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  pivot_longer(all_of(all_cats), names_to = "category", values_to = "pct_of_respondents") %>%
  arrange(region_clean, desc(pct_of_respondents)) %>%
  mutate(pct_label = percent(pct_of_respondents, accuracy = 0.1))

q10_by_region_wide <- q10_by_region_long %>%
  transmute(region = region_clean, category, pct = pct_label) %>%
  pivot_wider(id_cols = region, names_from = category, values_from = pct)

# # --- 6) Save -----------------------------------------------------------------
# dir.create(here::here("tables"), recursive = TRUE, showWarnings = FALSE)
# 
# # Main output (wide by region)
# write_xlsx(q10_by_region_wide, here::here("tables", "q10_by_region_percentages.xlsx"))

# # Extra: keep overall long + regional long in one file (optional but handy)
# writexl::write_xlsx(
#   list(
#     "Overall (long)"   = q10_overall,
#     "By region (wide)" = q10_by_region_wide,
#     "By region (long)" = q10_by_region_long
#   ),
#   path = here::here("tables", "q10_outputs.xlsx")
# )
# 
# message("✅ Q10 regex tables written: tables/q10_by_region_percentages.xlsx and tables/q10_outputs.xlsx")
