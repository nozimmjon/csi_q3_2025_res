# 01_standardize_reshape.R
# ---------------------------------------------------------------------------
# Renames raw columns to short English names, deduplicates by phone_number,
# normalizes text encoding inconsistencies (is_working / is_official),
# harmonizes district names, and drops excluded districts and invalid genders.
#
# Requires: df_all (from 00_ingest_merge.R)
# Key output:
#   d_01 — cleaned, reshaped data frame (assigned to .GlobalEnv)
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(writexl)

stopifnot(exists("df_all"))

# --- 1) Rename columns -----------------------------------------------------
new_names <- c(
  "phone_number", "gender", "region", "district", "age",
  "is_working", "is_official", "q_1", "q_2", "q_3", "income",
  "q_4", "q_5", "q_6", "q_7", "tuman_hokimi", "viloyat_hokimi"
)

# Validate that df_all has enough columns
if (ncol(df_all) < length(new_names)) {
  stop(
    "df_all has only ", ncol(df_all), " columns but ",
    length(new_names), " are expected. Check the source Excel files."
  )
}

names(df_all)[seq_along(new_names)] <- new_names

# Validate critical columns exist
required_cols <- c("phone_number", "gender", "region", "district", "age",
                   "is_working", "is_official")
missing <- setdiff(required_cols, names(df_all))
if (length(missing) > 0) {
  stop("Missing required columns after rename: ", paste(missing, collapse = ", "))
}

# --- 2) Clean and reshape --------------------------------------------------
d_01 <- df_all %>%
  group_by(phone_number) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(
    is_official = case_when(
      is_official %in% c("Йўқ", "Йуқ") ~ "Йўқ",
      TRUE ~ as.character(is_official)
    ),
    is_working = case_when(
      is_working %in% c("Йўқ", "Йуқ") ~ "Йўқ",
      TRUE ~ as.character(is_working)
    )
  ) %>%
  tidyr::drop_na(district) %>%
  relocate(income, .before = q_1) %>%
  mutate(
    district = case_match(
      district,
      "Бўз"           ~ "Бўстон",
      "Жиззах"        ~ "Шароф Рашидов",
      "Хаваст"        ~ "Ховос",
      "Чирчиқ шаҳар"  ~ "Чирчиқ ш.",
      "Чиноз тумани"  ~ "Чиноз",
      "Қибрай тумани"  ~ "Қибрай",
      "Янгийўл шаҳар" ~ "Янгийўл ш.",
      "Турткўл"       ~ "Тўрткўл",
      "Янги хаёт"     ~ "Янгиҳаёт",
      .default = district
    ),
    region = region |>
      as.character() |>
      str_replace_all("[\\r\\n\\t]+", " ") |>
      str_squish()
  ) %>%
  filter(!district %in% c("Давлатобод", "Ғозғон шаҳри")) %>%
  filter(gender %in% c("Эркак", "Аёл"))

assign("d_01", d_01, envir = .GlobalEnv)
message("d_01 ready: ", nrow(d_01), " rows x ", ncol(d_01), " cols")
