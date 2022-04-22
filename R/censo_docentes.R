library(dplyr)
library(arrow)
library(purrr)
library(stringr)

files <- fs::dir_ls(path = "~/Documents/base_de_dados/censo_escolar", recurse = TRUE, regexp = "DOCENTES.*\\.CSV")


prof_art_ef_clean <- function(path) {
  df <- read_delim_arrow(path, delim = "|")
  if (str_detect(path, "2015|2017|2019")) {
    df <- df %>%
      filter(
        IN_REGULAR == 1,
        TP_ETAPA_ENSINO %in% c(7, 18),
        CO_MUNICIPIO == 2927408,
        TP_DEPENDENCIA == 3
      ) %>%
      select(
        ANO = NU_ANO_CENSO,
        ID_ESCOLA = CO_ENTIDADE,
        prof_artes = IN_DISC_ARTES,
        prof_edfis = IN_DISC_EDUCACAO_FISICA
      )
  } else if (str_detect(path, "2013")) {
    df <- df %>%
      filter(
        FK_COD_MOD_ENSINO == 1,
        FK_COD_ETAPA_ENSINO %in% c(7, 18),
        FK_COD_MUNICIPIO == 2927408,
        ID_DEPENDENCIA_ADM == 3
      ) %>%
      select(
        ANO = ANO_CENSO,
        ID_ESCOLA = PK_COD_ENTIDADE,
        prof_artes = ID_ARTES,
        prof_edfis = ID_EDUCACAO_FISICA
      ) %>%
      mutate(across(c(prof_artes, prof_edfis), ~ as.numeric(.x)))
  }
  df <- df  %>%
    group_by(ANO, ID_ESCOLA) %>%
    summarise(across(c(prof_artes, prof_edfis), ~ max(.x, na.rm = TRUE)))
}

prof_art_ef <- map_dfr(
  files,
  ~ prof_art_ef_clean(.x)
)

readr::write_csv(prof_art_ef, "./edu-covid-data/data/projeto_greve/intermed/censo_salvador/prof_artes_edfis_mun_salvador_2013_2015_2017.csv")


prof_art_ef_19 <- prof_art_ef_clean(files)

readr::write_csv(prof_art_ef_19, "./edu-covid-data/data/projeto_greve/intermed/censo_salvador/prof_artes_edfis_mun_salvador_2019.csv")
