library(readr)
library(dplyr)

read_greve <- function(nivel, ...) {
  path_base = glue::glue("./edu-covid-data/data/projeto_greve/raw/greve/greves_{nivel}_2000-2020.csv")
  read_csv(path_base) %>%
    filter(...)
}
