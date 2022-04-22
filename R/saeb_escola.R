library(dplyr)
library(stringr)
library(magrittr)
library(purrr)
library(tidyr)

source("./R/helpers_saeb.R")

clean_saeb_escola <- function(path) {
  if (str_detect(path, "2011")) {
    escola <- read_saeb(path, 
                        ID_DEPENDENCIA_ADM %in% c(2, 3), 
                        ID_SERIE %in% c(5, 9), 
                        ID_UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
      select_saeb_escola_cols_2011(.path_escola_quest_2011 = "./edu-covid-data/data/projeto_greve/raw/saeb/2011/Dados/TS_QUEST_ESCOLA.csv")
  } else if (str_detect(path, "2013|2015|2017")) {
    ano <- str_extract(path, "2013|2015|2017")
    print(glue::glue("extraindo {ano}"))
    path_aluno <- fs::dir_ls(path = glue::glue("./edu-covid-data/data/projeto_greve/raw/saeb/{ano}/DADOS/"), regexp = "ALUNO_(5EF|9EF).csv")
    escola <- read_saeb(path, 
                        ID_DEPENDENCIA_ADM %in% c(2, 3), 
                        ID_UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29)) %>%
      select_saeb_escola_cols()
  }
  escola <- escola %>%
    padr_saeb_escola()
  print(glue::glue("{path} concluído"))
  return(escola)
}


saeb_escola <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_greve/raw/saeb", recurse = TRUE, regexp = "(TS_RESULTADO_ESCOLA|TS_ESCOLA).csv"),
  ~ clean_saeb_escola(.x)
)

readr::write_csv(saeb_escola, "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/escola_11131517.csv")
