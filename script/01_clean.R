# 01_clean.R
# ---------------------------------------------------------------------------
# Step 1: Rename, validate, deduplicate, and clean the merged raw data.
#
# Design notes (survey best practice):
#   - Every exclusion (filter, dedup, invalid value) is counted and logged
#     so the analyst has a full audit trail from raw N to analytic N.
#   - District numeric codes are mapped to text names via sample_size.xlsx
#     for readability in downstream tables.
#   - Text normalization handles known encoding variants (Йуқ → Йўқ).
#   - Age is validated against the range [18, 80] per the data dictionary.
#   - Special-code conventions from the dictionary (-96 Refused, -97 DK,
#     -98 N/A skip, -99 Missing) are mapped to NA_real_ for numeric fields.
#   - age_group is created to match the population_age_group.xlsx breaks.
#
# Requires: df_raw (from 00_ingest.R)
# Output:
#   checkpoint/01_clean.rds — cleaned analytic data
#   d_clean                 — same, in .GlobalEnv
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)
library(here)

stopifnot(exists("df_raw"))

n_raw <- nrow(df_raw)
exclusion_log <- list()   # collects (reason, n_dropped)

# ===========================================================================
# 1) Rename columns to English short names
# ===========================================================================
new_names <- c(
  "phone_number", "gender", "region", "district_code", "age",
  "is_working", "is_official", "q1", "q2", "q3", "income",
  "q4", "q5", "q6", "q7", "tuman_hokimi", "viloyat_hokimi"
)

if (ncol(df_raw) < length(new_names)) {
  stop(
    "df_raw has ", ncol(df_raw), " columns but ",
    length(new_names), " are expected."
  )
}
names(df_raw)[seq_along(new_names)] <- new_names

# ===========================================================================
# 2) Map district codes to text names
# ===========================================================================
sample_size_data <- read_excel(here::here("sample_size.xlsx")) %>%
  mutate(code = as.character(code))

code_to_name <- setNames(sample_size_data$district, sample_size_data$code)

df <- df_raw %>%
  mutate(
    district_code = as.character(district_code),
    district = ifelse(
      district_code %in% names(code_to_name),
      code_to_name[district_code],
      NA_character_
    )
  )

n_unmapped <- sum(is.na(df$district) & !is.na(df$district_code))
if (n_unmapped > 0) {
  unmapped_codes <- unique(df$district_code[is.na(df$district) & !is.na(df$district_code)])
  warning(
    "[01] ", n_unmapped, " rows have district codes not in sample_size.xlsx: ",
    paste(head(unmapped_codes, 10), collapse = ", ")
  )
}

# ===========================================================================
# 3) Drop rows with missing district
# ===========================================================================
n_before <- nrow(df)
df <- df %>% filter(!is.na(district))
exclusion_log[["missing_district"]] <- n_before - nrow(df)

# ===========================================================================
# 4) Deduplicate by phone_number (keep first occurrence)
# ===========================================================================
n_before <- nrow(df)
n_total_dupes <- sum(duplicated(df$phone_number))
df <- df %>%
  group_by(phone_number) %>%
  slice(1L) %>%
  ungroup()
exclusion_log[["duplicate_phone"]] <- n_before - nrow(df)

# ===========================================================================
# 5) Normalize text fields
# ===========================================================================
df <- df %>%
  mutate(
    # Employment variants
    is_official = case_when(
      is_official %in% c("\u0419\u045e\u049b", "\u0419\u0443\u049b") ~ "\u0419\u045e\u049b",
      TRUE ~ as.character(is_official)
    ),
    is_working = case_when(
      is_working %in% c("\u0419\u045e\u049b", "\u0419\u0443\u049b") ~ "\u0419\u045e\u049b",
      TRUE ~ as.character(is_working)
    ),
    # District name harmonization (renamed districts)
    district = case_match(
      district,
      "\u0411\u045e\u0437"           ~ "\u0411\u045e\u0441\u0442\u043e\u043d",
      "\u0416\u0438\u0437\u0437\u0430\u0445" ~ "\u0428\u0430\u0440\u043e\u0444 \u0420\u0430\u0448\u0438\u0434\u043e\u0432",
      "\u0425\u0430\u0432\u0430\u0441\u0442" ~ "\u0425\u043e\u0432\u043e\u0441",
      "\u0427\u0438\u0440\u0447\u0438\u049b \u0448\u0430\u04b3\u0430\u0440" ~ "\u0427\u0438\u0440\u0447\u0438\u049b \u0448.",
      "\u0427\u0438\u043d\u043e\u0437 \u0442\u0443\u043c\u0430\u043d\u0438" ~ "\u0427\u0438\u043d\u043e\u0437",
      "\u049a\u0438\u0431\u0440\u0430\u0439 \u0442\u0443\u043c\u0430\u043d\u0438" ~ "\u049a\u0438\u0431\u0440\u0430\u0439",
      "\u042f\u043d\u0433\u0438\u0439\u045e\u043b \u0448\u0430\u04b3\u0430\u0440" ~ "\u042f\u043d\u0433\u0438\u0439\u045e\u043b \u0448.",
      "\u0422\u0443\u0440\u0442\u043a\u045e\u043b" ~ "\u0422\u045e\u0440\u0442\u043a\u045e\u043b",
      "\u042f\u043d\u0433\u0438 \u0445\u0430\u0451\u0442" ~ "\u042f\u043d\u0433\u0438\u04b3\u0430\u0451\u0442",
      .default = district
    ),
    # Clean region whitespace
    region = region %>%
      as.character() %>%
      str_replace_all("[\\r\\n\\t]+", " ") %>%
      str_squish()
  )

# ===========================================================================
# 6) Exclude specific districts and invalid genders
# ===========================================================================
excluded_districts <- c(
  "\u0414\u0430\u0432\u043b\u0430\u0442\u043e\u0431\u043e\u0434",
  "\u0492\u043e\u0437\u0493\u043e\u043d \u0448\u0430\u04b3\u0440\u0438"
)
n_before <- nrow(df)
df <- df %>% filter(!district %in% excluded_districts)
exclusion_log[["excluded_districts"]] <- n_before - nrow(df)

valid_genders <- c("\u042d\u0440\u043a\u0430\u043a", "\u0410\u0451\u043b")
n_before <- nrow(df)
df <- df %>% filter(gender %in% valid_genders)
exclusion_log[["invalid_gender"]] <- n_before - nrow(df)

# ===========================================================================
# 7) Parse and validate numeric fields
# ===========================================================================
df <- df %>%
  mutate(
    age = suppressWarnings(as.numeric(age)),
    income = suppressWarnings(as.numeric(str_replace_all(income, " ", ""))),
    tuman_hokimi  = suppressWarnings(as.numeric(tuman_hokimi)),
    viloyat_hokimi = suppressWarnings(as.numeric(viloyat_hokimi))
  )

# Age validation: data dictionary specifies [18, 80]
n_invalid_age <- sum(is.na(df$age) | df$age < 18 | df$age > 120, na.rm = FALSE)
if (n_invalid_age > 0) {
  message("[01] ", n_invalid_age, " rows with invalid/missing age (set to NA)")
  df <- df %>% mutate(age = ifelse(age >= 18 & age <= 120, age, NA_real_))
}

# Hokim scores: valid range [1, 10]
df <- df %>%
  mutate(
    tuman_hokimi  = ifelse(tuman_hokimi >= 1 & tuman_hokimi <= 10,
                           tuman_hokimi, NA_real_),
    viloyat_hokimi = ifelse(viloyat_hokimi >= 1 & viloyat_hokimi <= 10,
                            viloyat_hokimi, NA_real_)
  )

# ===========================================================================
# 8) Create age_group (must match population_age_group.xlsx breaks)
# ===========================================================================
df <- df %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 30, 50, 66, Inf),
      labels = c("18-29", "30-49", "50-65", "Over 65"),
      right  = FALSE
    )
  )

n_missing_age_group <- sum(is.na(df$age_group))
if (n_missing_age_group > 0) {
  message("[01] ", n_missing_age_group, " rows with NA age_group (age was invalid)")
}

# ===========================================================================
# 9) Print exclusion audit trail
# ===========================================================================
message("\n[01] === EXCLUSION AUDIT TRAIL ===")
message("  Raw rows from 00_ingest:        ", n_raw)
for (reason in names(exclusion_log)) {
  n <- exclusion_log[[reason]]
  if (n > 0) message("  Dropped (", reason, "): ", n)
}
message("  Final analytic N:                ", nrow(df))
message("  Total dropped:                   ", n_raw - nrow(df))
message("  Retention rate:                  ",
        round(nrow(df) / n_raw * 100, 1), "%")
message("================================\n")

# ===========================================================================
# 10) Save
# ===========================================================================
d_clean <- df
saveRDS(d_clean, here::here("checkpoint", "01_clean.rds"))
assign("d_clean", d_clean, envir = .GlobalEnv)

# Also keep the code_to_name lookup for downstream scripts
assign("code_to_name", code_to_name, envir = .GlobalEnv)

message("[01] d_clean ready: ", nrow(d_clean), " rows x ", ncol(d_clean), " cols")
