# 01_standardize_reshape.R
library(dplyr); library(stringr); library(writexl)

stopifnot(exists("df_all"))

new_names <- c(
  "phone_number","gender","region","district","age",
  "is_working","is_official","q_1","q_2","q_3","income",
  "q_4","q_5","q_6","q_10","q_7","tuman_hokimi","viloyat_hokimi"
)

# Ensure we can set first N names, leave the rest unchanged
names(df_all)[seq_along(new_names)] <- new_names

# Keep characters; DO NOT convert to factors here
d_01 <- df_all %>%
  group_by(phone_number) %>% slice(1L) %>% ungroup() %>%
  mutate(is_official = case_when(
    is_official %in% c("Йўқ","Йуқ") ~ "Йўқ",
    TRUE ~ as.character(is_official)
  )) %>% 
  mutate(is_working = case_when(
    is_working %in% c("Йўқ","Йуқ") ~ "Йўқ",
    TRUE ~ as.character(is_working)
  )) %>% 
  tidyr::drop_na(district) %>%
  relocate(income, .before = q_1) %>%
  mutate(
    district = dplyr::recode(district,
                             # --- your full recode list (unchanged) ---
                             "Бўз" = "Бўстон",
                             "Жиззах" = "Шароф Рашидов",
                             "Хаваст" = "Ховос",
                             "Чирчиқ шаҳар" = "Чирчиқ ш.",
                             "Чиноз тумани" = "Чиноз", 
                             "Қибрай тумани" = "Қибрай", 
                             "Янгийўл шаҳар" = "Янгийўл ш.",
                             "Турткўл" = "Тўрткўл",
                             "Янги хаёт" = "Янгиҳаёт"
                             ),
    region = region |> as.character() |> str_replace_all("[\\r\\n\\t]+", " ") |> str_squish()
  ) %>%
  filter(!district %in% c("Давлатобод", "Ғозғон шаҳри")) %>%
  filter(gender %in% c("Эркак", "Аёл")) 

# write_xlsx(d_01, here::here("25_01_Cons_sent_jan.xlsx"))
# assign("d_01", d_01, envir = .GlobalEnv)
# message("✅ d_01 saved and ready.")
