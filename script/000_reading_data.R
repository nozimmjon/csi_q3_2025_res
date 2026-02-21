# 000_reading_data.R
# ---------------------------------------------------------------------------
# Standalone diagnostic tool for inspecting raw Excel files in data/.
# NOT sourced by the pipeline (99_run_all.R) — run interactively when you
# need to audit column counts, duplicate names, or empty/placeholder columns
# before a pipeline run.
#
# Key outputs (printed to console):
#   - Column-count frequency table across all files
#   - List of files with duplicate column names
#   - List of files with empty/placeholder columns (names starting with "...")
#   - Combined df_all (all files bound row-wise after column alignment)
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(here)
library(stringr)

# --- helper -----------------------------------------------------------------
standardize_name <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

# --- 1) List files ----------------------------------------------------------
file_paths <- list.files(
  path    = here::here("data"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)
message("Found ", length(file_paths), " Excel files in data/")

# --- 2) Read each file once -------------------------------------------------
df_list <- lapply(file_paths, function(path) {
  df <- read_excel(path)
  names(df) <- sapply(names(df), standardize_name)
  df
})
names(df_list) <- basename(file_paths)

# --- 3) Run all checks in-memory -------------------------------------------

# Column counts
col_counts <- sapply(df_list, ncol)
cat("Column counts:\n")
print(table(col_counts))

if (length(unique(col_counts)) > 1) {
  cat("\nFiles that differ from the mode:\n")
  mode_ncol <- as.numeric(names(sort(table(col_counts), decreasing = TRUE))[1])
  print(names(col_counts)[col_counts != mode_ncol])
}

# Duplicate column names
dup_flags <- sapply(df_list, function(df) any(duplicated(names(df))))
if (any(dup_flags)) {
  cat("\nFiles with duplicate column names:\n")
  for (nm in names(dup_flags)[dup_flags]) {
    dup_names <- names(df_list[[nm]])[duplicated(names(df_list[[nm]]))]
    cat("  ", nm, ":", paste(dup_names, collapse = ", "), "\n")
  }
} else {
  cat("\nNo files with duplicate column names detected.\n")
}

# Empty / placeholder columns (readxl names them "...1", "...2", etc.)
empty_flags <- sapply(df_list, function(df) any(grepl("^\\.\\.\\.", names(df))))
if (any(empty_flags)) {
  cat("\nFiles with empty/placeholder columns:\n")
  for (nm in names(empty_flags)[empty_flags]) {
    bad <- grep("^\\.\\.\\.", names(df_list[[nm]]), value = TRUE)
    cat("  ", nm, ":", paste(bad, collapse = ", "), "\n")
  }
} else {
  cat("\nNo files with empty/placeholder columns.\n")
}

# --- 4) Align columns and bind ---------------------------------------------
all_cols_union <- Reduce(union, lapply(df_list, names))

df_list_aligned <- lapply(df_list, function(df) {
  missing_cols <- setdiff(all_cols_union, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }
  df <- df[all_cols_union]
  df[] <- lapply(df, as.character)
  df
})

# Verify alignment before binding
if (all(sapply(df_list_aligned, function(x) identical(names(x), all_cols_union)))) {
  cat("\nColumns aligned successfully.\n")
} else {
  stop("Column alignment failed — check for encoding issues in column names.")
}

df_all <- bind_rows(df_list_aligned)
cat("Combined data frame:", nrow(df_all), "rows x", ncol(df_all), "cols.\n")
