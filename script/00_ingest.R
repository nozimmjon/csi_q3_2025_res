# 00_ingest.R
# ---------------------------------------------------------------------------
# Step 0: Read all regional .xlsx files from data/, validate column structure,
#         align columns, and row-bind into a single raw data frame.
#
# Design notes:
#   - Raw data comes as 14 regional Excel files, one per viloyat.
#   - Some files have trailing empty columns (readxl names them "...N");
#     we keep only the first 17 meaningful columns.
#   - All values are cast to character at this stage to prevent type
#     coercion issues during the merge. Type conversion happens in 01_clean.R.
#
# Output:
#   checkpoint/00_raw.rds   — merged raw data frame
#   df_raw                  — same, assigned to .GlobalEnv for pipeline
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(here)

N_EXPECTED_COLS <- 17L   # core survey columns per data dictionary

# --- helper ------------------------------------------------------------------
standardize_header <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

# --- 1) Discover files -------------------------------------------------------
data_dir   <- here::here("data")
file_paths <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)
stopifnot(
  "No .xlsx files found in data/" = length(file_paths) > 0
)
message("[00] Found ", length(file_paths), " regional files in data/")

# --- 2) Read each file -------------------------------------------------------
df_list <- lapply(file_paths, function(path) {
  df <- read_excel(path, col_types = "text")           # everything as text
  names(df) <- standardize_header(names(df))

  # Drop trailing empty/placeholder columns
  keep <- !grepl("^\\.\\.\\.", names(df))
  n_real <- sum(keep)
  if (n_real < N_EXPECTED_COLS) {
    warning(
      basename(path), ": only ", n_real, " real columns ",
      "(expected ", N_EXPECTED_COLS, ")"
    )
  }
  df <- df[, keep, drop = FALSE]

  # Keep only first N_EXPECTED_COLS to ensure uniform structure
  if (ncol(df) > N_EXPECTED_COLS) {
    df <- df[, seq_len(N_EXPECTED_COLS), drop = FALSE]
  }

  df$source_file <- basename(path)                     # provenance
  df
})

# --- 3) Validate column counts -----------------------------------------------
col_counts <- vapply(df_list, ncol, integer(1))
if (length(unique(col_counts)) > 1) {
  tbl <- table(col_counts)
  warning(
    "Column count varies across files after trimming:\n",
    paste(capture.output(print(tbl)), collapse = "\n")
  )
}

# --- 4) Align and bind -------------------------------------------------------
all_cols <- Reduce(union, lapply(df_list, names))

df_list_aligned <- lapply(df_list, function(df) {
  miss <- setdiff(all_cols, names(df))
  for (m in miss) df[[m]] <- NA_character_
  df[all_cols]
})

df_raw <- bind_rows(df_list_aligned)

# --- 5) Save checkpoint ------------------------------------------------------
dir.create(here::here("checkpoint"), showWarnings = FALSE, recursive = TRUE)
saveRDS(df_raw, here::here("checkpoint", "00_raw.rds"))

assign("df_raw", df_raw, envir = .GlobalEnv)
message(
  "[00] df_raw ready: ", nrow(df_raw), " rows x ", ncol(df_raw), " cols ",
  "(", length(file_paths), " files merged)"
)
