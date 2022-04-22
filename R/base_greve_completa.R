library(readr)
library(dplyr)


# ---- saeb aluno ----

saeb_aluno_group <- function(.escola_municipal, .id_serie, ...) {
  aluno <- "./edu-covid-data/data/projeto_greve/intermed/saeb/aluno/aluno_11131517.csv"
  nse <- "./edu-covid-data/data/projeto_greve/intermed/saeb/aluno/nse_11131517.csv"
  read_csv(aluno) %>%
    select(!contains("_nse")) %>%
    mutate(
      ano5 = if_else(ID_SERIE == 5, 1, 0),
      ano9 = if_else(ID_SERIE == 9, 1, 0)
    ) %>%
    filter(ESCOLA_MUNICIPAL %in% .escola_municipal, ID_SERIE %in% .id_serie) %>%
    group_by(...) %>%
    summarise(across(mulher:atraso, ~ mean(.x, na.rm = TRUE)), ano5 = max(ano5), ano9 = max(ano9)) %>%
    inner_join(
      read_csv(nse) %>%
        select(!contains("_nse")) %>%
        group_by(ANO, ID_ESCOLA) %>%
        summarise(nse = mean(nse, na.rm = TRUE))
    ) %>%
    filter(ID_MUNICIPIO < 6000000 | ID_ESCOLA < 6000000) # Removendo as mascaras
}

# ---- saeb escolas ----

saeb_escolas_group <- function(.escola_municipal, .id_serie) {
  escola <- "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/escola_11131517.csv"
  ice <- "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/ice_11131517.csv"
  iie <- "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/iie_11131517.csv"
  ipe <- "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/ipe_11131517.csv"
  ise <- "./edu-covid-data/data/projeto_greve/intermed/saeb/escola/ise_11131517.csv"
  read_csv(escola) %>%
    select(!c(contains("ice"), contains("iie"), contains("ipe"), contains("ise"))) %>%
    filter(ESCOLA_MUNICIPAL %in% .escola_municipal, ID_SERIE %in% .id_serie) %>%
    mutate(
      MEDIA_LP_PADR = (MEDIA_LP - mean(MEDIA_LP, na.rm = TRUE)) / (sd(MEDIA_LP, na.rm = TRUE)),
      MEDIA_MT_PADR = (MEDIA_MT - mean(MEDIA_MT, na.rm = TRUE)) / (sd(MEDIA_MT, na.rm = TRUE))
    ) %>%
    inner_join(
      read_csv(ice) %>% 
        select(!contains("_ice")) %>%
        group_by(ANO, ID_ESCOLA) %>%
        summarise(ice = mean(ice, na.rm = TRUE))) %>%
    inner_join(
      read_csv(iie) %>% 
        select(!contains("_iie")) %>%
        group_by(ANO, ID_ESCOLA) %>%
        summarise(iie = mean(iie, na.rm = TRUE))) %>%
    inner_join(
      read_csv(ipe) %>% 
        select(!contains("_ipe")) %>%
        group_by(ANO, ID_ESCOLA) %>%
        summarise(ipe = mean(ipe, na.rm = TRUE))) %>%
    inner_join(
      read_csv(ise) %>% 
        select(!contains("_ise")) %>%
        group_by(ANO, ID_ESCOLA) %>%
        summarise(ise = mean(ise, na.rm = TRUE)))
}

# ---- professor ----

saeb_professor_group <- function(.escola_municipal, .id_serie, ...) {
  professor <- "./edu-covid-data/data/projeto_greve/intermed/saeb/professor/professor_11131517.csv"
  read_csv(professor) %>%
    filter(ESCOLA_MUNICIPAL %in% .escola_municipal, ID_SERIE %in% .id_serie) %>%
    group_by(...) %>%
    summarise(across(prof_mulher:aprendizagem_falta_interesse, ~ mean(.x, na.rm = TRUE)))
    
}

# ---- diretor ----

saeb_diretor_group <- function(.escola_municipal) {
  diretor <- "./edu-covid-data/data/projeto_greve/intermed/saeb/diretor/diretor_11131517.csv"
  read_csv(diretor) %>%
    filter(ESCOLA_MUNICIPAL == .escola_municipal)
}


# ---- tamanho da turma ----

tamanho_da_turma <- function(.escola_municipal, .id_serie, .ano) {
  path_tam_turma <- "./edu-covid-data/data/projeto_greve/intermed/indicadores_inep/atu_ba_1113151719.csv"
  tam_turma <- read_csv(path_tam_turma) %>%
    tidyr::pivot_longer(tam_turma_5ano:tam_turma_9ano, names_to = "ID_SERIE", values_to = "tam_turma") %>%
    mutate(ID_SERIE = str_extract(ID_SERIE, "5|9") %>% as.double()) %>%
    tidyr::drop_na(tam_turma) %>%
    filter(ESCOLA_MUNICIPAL %in% .escola_municipal, ID_SERIE %in% .id_serie, ANO %in% .ano)
}

# ---- greve ----

read_greve <- function(.nivel, ...) {
  path_base = glue::glue("./edu-covid-data/data/projeto_greve/raw/greves/greves_{.nivel}_2000-2020.csv")
  read_csv(path_base) %>%
    filter(...)
}

# ---- base completa ----
base_completa <- function(.escola_municipal, .id_serie, ...) {
  saeb_aluno_group(.escola_municipal = .escola_municipal, .id_serie = .id_serie, ...) %>%
    inner_join(saeb_escolas_group(.escola_municipal = .escola_municipal, .id_serie = .id_serie)) %>%
    inner_join(saeb_professor_group(.escola_municipal = .escola_municipal, .id_serie = .id_serie, ...)) %>%
    inner_join(saeb_diretor_group(.escola_municipal = .escola_municipal)) %>%
    inner_join(tamanho_da_turma(.escola_municipal = .escola_municipal, .id_serie = .id_serie, .ano = c(2011, 2013, 2015, 2017))) %>%
    left_join(read_greve(.nivel = if_else(.escola_municipal == 1, "municipais", "estaduais"), ID_UF == 29, ANO %in% c(2011, 2013, 2015, 2017))) %>%
    mutate(DIAS_GREVE_MUNICIPAL = tidyr::replace_na(DIAS_GREVE_MUNICIPAL, 0),
           GREVE_MUNICIPAL = if_else(DIAS_GREVE_MUNICIPAL == 0, 0, 1))
}



base_greve_5ano <- base_completa(.escola_municipal = 1, .id_serie = 5, ANO, ID_MUNICIPIO, ID_ESCOLA, RURAL) %>%
  write_csv("./edu-covid-data/data/projeto_greve/processed/base_greve/5ef_greve_11131517.csv")
base_greve_9ano <- base_completa(.escola_municipal = 1, .id_serie = 9, ANO, ID_MUNICIPIO, ID_ESCOLA, RURAL) %>%
  write_csv("./edu-covid-data/data/projeto_greve/processed/base_greve/9ef_greve_11131517.csv")
base_greve <- base_completa(.escola_municipal = 1, .id_serie = c(5, 9), ANO, ID_MUNICIPIO, ID_ESCOLA, RURAL) %>%
  write_csv("./edu-covid-data/data/projeto_greve/processed/base_greve/59ef_greve_11131517.csv")


