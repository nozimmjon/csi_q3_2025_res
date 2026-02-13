library(readxl)
library(dplyr)
library(here)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(janitor)
library(skimr)
library(here)
library(knitr)
library(writexl)
library(rlang)
library(openxlsx)
library(summarytools)
library(usethis)
library(pointblank)
library(survey)
library(srvyr)
library(webshot2)
library(gt)
library(ggrepel)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Step 1: List files
file_paths <- list.files(
  path = here::here("data"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Step 2: Check and identify problematic files
col_counts <- sapply(file_paths, function(path) length(names(read_excel(path))))
cat("📊 Column counts:\n"); print(table(col_counts))



files_with_dup_cols <- sapply(file_paths, function(path) {
  col_names <- names(read_excel(path))
  any(duplicated(col_names))
})

dup_files <- file_paths[files_with_dup_cols]

if (length(dup_files) > 0) {
  cat("🚨 Files with duplicate column names:\n")
  print(dup_files)
} else {
  cat("✅ No files with duplicate column names detected.\n")
}



files_with_empty_cols <- sapply(file_paths, function(path) {
  any(grepl("^\\.\\.\\.", names(read_excel(path))))
})
bad_files <- file_paths[files_with_empty_cols]
if (length(bad_files) > 0) {
  cat("🚨 Files with empty columns detected:\n"); print(bad_files)
}

# Step 3: Read Excel files
df_list <- lapply(file_paths, function(path) {
  df <- read_excel(path)
  names(df) <- trimws(names(df))
  df
})

standardize_name <- function(x) {
  # Lowercase, trim, remove double spaces
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}


df_list <- lapply(file_paths, function(path) {
  df <- read_excel(path)
  names(df) <- sapply(names(df), standardize_name)
  df
})


# Step 4: Union of all column names
all_cols_union <- Reduce(union, lapply(df_list, names))

# Step 5: Align columns & fix types explicitly
df_list_aligned <- lapply(df_list, function(df) {
  missing_cols <- setdiff(all_cols_union, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA_character_
  }
  df <- df[all_cols_union]
  df[] <- lapply(df, as.character)  # fix all to character for safe binding
  df
})

# Step 6: Verify consistency
if (all(sapply(df_list_aligned, function(x) identical(names(x), all_cols_union)))) {
  cat("✅ Columns aligned successfully.\n")
} else {
  stop("❌ Column alignment failed!")
}

# Step 7: Bind safely
df_all <- bind_rows(df_list_aligned)

cat("✅ Combined data frame:", nrow(df_all), "rows,", ncol(df_all), "cols.\n")
