library(readr)
library(dplyr)
library(purrr)
library(stringr)

files <- fs::dir_ls(path = "~/Documents/base_de_dados/censo_escolar", recurse = TRUE, regexp = "ESCOLAS.CSV")

num_fun_clean <- function(path) {
  df <- read_delim(path, delim = "|")
  if (str_detect(path, "2017") | str_detect(path, "2015") ) {
    df <- df %>%
      filter(
        CO_MUNICIPIO == 2927408,
        TP_DEPENDENCIA == 3,
        IN_COMUM_FUND_AI == 1
      ) %>%
      select(
        ANO = NU_ANO_CENSO,
        ID_ESCOLA = CO_ENTIDADE,
        n_fun = NU_FUNCIONARIOS
      )
  } else if (str_detect(path, "2013")) {
    df <- df %>%
      filter(
        FK_COD_MUNICIPIO == 2927408,
        ID_DEPENDENCIA_ADM == 3,
        ID_REG_FUND_8_ANOS == 1 | ID_REG_FUND_9_ANOS == 1
      ) %>%
      select(
        ANO = ANO_CENSO,
        ID_ESCOLA = PK_COD_ENTIDADE,
        n_fun = NUM_FUNCIONARIOS
      )
  }
}

censo_num_fun <- map_dfr(
  files,
  ~ num_fun_clean(.x)
)

write_csv(censo_num_fun,  "./edu-covid-data/data/projeto_greve/intermed/censo/n_fum_mun_salvador_2013_2015_2017.csv")