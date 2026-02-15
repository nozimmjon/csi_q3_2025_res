# 00_ingest_merge.R
# ---------------------------------------------------------------------------
# Reads all regional .xlsx files from data/, standardizes column names,
# aligns columns across files, and row-binds into a single data frame.
#
# Key output:
#   df_all — character-typed data frame with all survey responses
#            (assigned to .GlobalEnv for downstream scripts)
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)

# --- helper -----------------------------------------------------------------
standardize_name <- function(x) {
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

# --- 1) Discover files ------------------------------------------------------
data_dir   <- here::here("data")
file_paths <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)
message("Found: ", length(file_paths), " files")

# --- 2) Read once, then run sanity checks on already-loaded data ------------
df_list <- lapply(file_paths, function(path) {
  df <- read_excel(path)
  names(df) <- sapply(names(df), standardize_name)
  df
})

col_counts <- sapply(df_list, ncol)
print(table(col_counts))

dup_cols <- sapply(df_list, \(df) any(duplicated(names(df))))
if (any(dup_cols)) {
  cat("Duplicate colnames in:\n")
  print(file_paths[dup_cols])
}

# --- 3) Align columns and bind ---------------------------------------------
all_cols <- Reduce(union, lapply(df_list, names))

df_list_aligned <- lapply(df_list, function(df) {
  miss <- setdiff(all_cols, names(df))
  for (m in miss) df[[m]] <- NA_character_
  df <- df[all_cols]
  df[] <- lapply(df, as.character)
  df
})

stopifnot(all(sapply(df_list_aligned, \(x) identical(names(x), all_cols))))
df_all <- bind_rows(df_list_aligned)

assign("df_all", df_all, envir = .GlobalEnv)
<<<<<<< HEAD
message("✅ df_all ready: ", nrow(df_all), " rows x ", ncol(df_all), " cols")


col_counts <- sapply(df_list, ncol)
file_paths[col_counts == 20]
=======
message("df_all ready: ", nrow(df_all), " rows x ", ncol(df_all), " cols")
>>>>>>> 38df743b61bea5c0fa55c932a2d3d725df98f984
