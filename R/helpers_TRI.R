library(ltm)
library(dplyr)
library(stringr)
library(magrittr)

tri <- function(.x, indicador) {
  .df <- .x %>%
    filter_at(vars(contains(indicador)), any_vars(!is.na(.)))
  
  dados_TRI <- .df %>%
    select(contains(indicador))
  
  TRI_const <- dados_TRI %>% grm(constrained = TRUE)
  
  TRI_var <- dados_TRI %>% grm()
  
  an <- anova(TRI_const, TRI_var)
  
  if (an$bic0 > an$bic1) {
    TRI <- TRI_const 
  } else if (an$bic0 < an$bic1) {
    TRI <- TRI_var
  }
  
  TRI <- TRI %>%
    factor.scores(resp.pattern = dados_TRI) %>%
    extract2("score.dat") %>%
    select(z1) %>%
    bind_cols(.df, .) %>%
    select(ANO, ID_MUNICIPIO, ID_ESCOLA, contains(indicador), z1) %>%
    rename_with(~ str_replace(.x, "z1", indicador))
}

