# 00_ingest_merge.R
library(readxl); 
library(dplyr); 
library(stringr); 
library(purrr); 
library(janitor)



standardize_name <- function(x) {
  # Lowercase, trim, remove double spaces
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

data_dir <- here::here("data")
file_paths <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)
message("Found: ", length(file_paths), " files")

# quick sanity checks (console)
col_counts <- sapply(file_paths, \(p) length(names(read_excel(p))))
print(table(col_counts))
dup_cols <- sapply(file_paths, \(p){ any(duplicated(names(read_excel(p)))) })
if (any(dup_cols)) { cat("🚨 Duplicate colnames in:\n"); print(file_paths[dup_cols]) }

df_list <- lapply(file_paths, function(path){
  df <- read_excel(path)
  names(df) <- sapply(names(df), standardize_name)
  df
})

all_cols <- Reduce(union, lapply(df_list, names))

df_list_aligned <- lapply(df_list, function(df){
  miss <- setdiff(all_cols, names(df))
  for (m in miss) df[[m]] <- NA_character_
  df <- df[all_cols]
  df[] <- lapply(df, as.character)  # keep characters (no factors yet)
  df
})

stopifnot(all(sapply(df_list_aligned, \(x) identical(names(x), all_cols))))
df_all <- bind_rows(df_list_aligned) 

assign("df_all", df_all, envir = .GlobalEnv)
message("✅ df_all ready: ", nrow(df_all), " rows x ", ncol(df_all), " cols")
