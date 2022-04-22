library(dplyr)
library(stringr)
library(magrittr)
library(purrr)

source("./R/helpers_saeb.R")


clean_saeb_aluno <- function(path) {
  aluno <- read_saeb(path, 
                     ID_DEPENDENCIA_ADM %in% c(2, 3), 
                     ID_SERIE %in% c(5, 9), 
                     ID_UF %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29))
  if (str_detect(path, "2011")) {
    aluno <- aluno %>%
      select_saeb_aluno_cols_2011()
  } else if (str_detect(path, "2013|2015|2017")) {
    aluno <- aluno %>%
      select_saeb_aluno_cols()
  }
  aluno <- aluno %>%
    padr_saeb_aluno()
  print(glue::glue("{path} concluído"))
  return(aluno)
}

saeb_aluno <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_greve/raw/saeb/2013", recurse = TRUE, regexp = "(QUEST_ALUNO|ALUNO(_5EF|_9EF)).csv"),
  ~ clean_saeb_aluno(.x)
)

readr::write_csv(saeb_aluno, "./edu-covid-data/data/projeto_greve/intermed/saeb/aluno/aluno_13.csv")