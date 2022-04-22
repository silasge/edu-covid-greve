library(ltm)
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)

source("./R/helpers_TRI.R")

# ---- aluno ----

tri_aluno <- function(path_aluno, indicador = "nse") {
  aluno <- readr::read_csv(path_aluno) %>%
    select(-feeezer_junto_nse) # excluir quando rodar toda base de alunos de novo
  print("aluno carregado")
  aluno_tri <- tri(aluno, indicador = indicador)
  readr::write_csv(aluno_tri, "./data/intermed/saeb/aluno/nse_11131517.csv")
  print("nse_11131517.csv salvo")
  return(aluno_tri)
}

aluno_nse <- tri_aluno("./data/intermed/saeb/aluno/aluno_11131517.csv")


# ---- escola ----

tri_escola <- function(path_escola, indicador = c("ice", "ise", "iie", "ipe")) {
  escola <- readr::read_csv(path_escola)
  print("escola carregado")
  escola_tri <- map(
    indicador,
    ~ tri(escola, .x) %>%
      readr::write_csv(glue::glue("./data/intermed/saeb/escola/{.x}_11131517.csv"))
  )
  return(escola_tri)
}

escola_tri <- tri_escola("./data/intermed/saeb/escola/escola_11131517.csv")
