library(readr)
library(dplyr)
library(purrr)

files <- fs::dir_ls(path = "./data/intermed", recurse = TRUE, glob = "*.csv")

base_salvador <- map(
  files,
  ~ read_csv(.x)
) %>%
  reduce(left_join) %>%
  mutate(
    across(c(nse, iie),
           ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
    across(reuniao_pais:mora_mae_pai,
           ~ round(.x * 100, 1))
  )

write_csv(base_salvador, "./data/processed/base_salvador/base_salvador_2013_2015_2017.csv")

haven::write_dta(base_salvador, "./data/processed/base_salvador/base_salvador_2013_2015_2017.dta", version = 13)
