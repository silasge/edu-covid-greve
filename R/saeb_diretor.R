library(dplyr)
library(stringr)
library(magrittr)
library(purrr)

source("./R/helpers_saeb.R")

clean_saeb_diretor <- function(path) {
  ano = str_extract(path, "2011|2013|2015|2017") %>% as.double()
  read_saeb(path, 
            ID_DEPENDENCIA_ADM %in% c(2, 3), 
            ID_UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
    select_saeb_diretor_cols(.ano = ano) %>%
    padr_saeb_diretor()
}


saeb_diretor <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_greve/raw/saeb", recurse = TRUE, regexp = "DIRETOR.csv"),
  ~ clean_saeb_diretor(.x)
)

readr::write_csv(saeb_diretor, "./edu-covid-data/data/projeto_greve/intermed/saeb/diretor/diretor_11131517.csv")
