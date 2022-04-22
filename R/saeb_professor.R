library(dplyr)
library(stringr)
library(magrittr)
library(purrr)

source("./R/helpers_saeb.R")

clean_saeb_professor <- function(path) {
  ano = str_extract(path, "2011|2013|2015|2017") %>% as.double()
  read_saeb(path, 
            ID_DEPENDENCIA_ADM %in% c(2, 3), 
            ID_SERIE %in% c(5, 9), 
            ID_UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
    select_saeb_professor_cols(.ano = ano) %>%
    padr_saeb_professor()
}


saeb_professor <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_greve/raw/saeb", recurse = TRUE, regexp = "PROFESSOR.csv"),
  ~ clean_saeb_professor(.x)
)

readr::write_csv(saeb_professor, "./edu-covid-data/data/projeto_greve/intermed/saeb/professor/professor_11131517.csv")
