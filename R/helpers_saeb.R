library(magrittr)
library(tidyselect)
library(dplyr)
library(stringr)

# ---- reader ----

read_saeb <- function(.path_saeb, ...) {
  delim <- if_else(str_detect(.path_saeb, "2011"), ";", ",")
  arrow::read_delim_arrow(.path_saeb, delim = delim) %>%
    filter(...)
}

# ---- select helpers ----

select_saeb_aluno_cols_2011 <- function(.data) {
  aluno_5ano <- .data %>%
    filter(ID_SERIE == 5) %>%
    select(
      ANO = ID_SAEB,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      ID_ESCOLA,
      ID_SERIE,
      mulher = TX_RESP_Q001,
      raca = TX_RESP_Q002,
      mes = TX_RESP_Q003,
      idade = TX_RESP_Q004,
      tv_col_nse = TX_RESP_Q005,
      radio_nse = TX_RESP_Q006,
      dvd_nse = TX_RESP_Q007,
      geladeira_nse = TX_RESP_Q008,
      freezer_separado_nse = TX_RESP_Q010,
      maq_lav_nse = TX_RESP_Q011,
      carro_nse = TX_RESP_Q012,
      computador_nse = TX_RESP_Q013,
      banheiro_nse = TX_RESP_Q014,
      domestica_nse = TX_RESP_Q015,
      quartos_nse = TX_RESP_Q016,
      mora_mae = TX_RESP_Q018,
      escola_mae_nse = TX_RESP_Q019,
      mora_pai = TX_RESP_Q022,
      escola_pai_nse = TX_RESP_Q023,
      resp_reunioes = TX_RESP_Q026,
      pais_incent_estud = TX_RESP_Q027,
      pais_incent_dev_casa = TX_RESP_Q028,
      pais_incent_ler = TX_RESP_Q029,
      pais_incent_presenc = TX_RESP_Q030,
      pais_conv_escola = TX_RESP_Q031,
      #tres_ou_mais_horas_tv_jogos_internet = TX_RESP_Q043,
      aluno_trab = TX_RESP_Q045,
      quando_entrou_escola = TX_RESP_Q046,
      ja_reprov = TX_RESP_Q048,
      ja_aband = TX_RESP_Q049
    )
  
  aluno_9ano <- .data %>%
    filter(ID_SERIE == 9) %>%
    select(
      ANO = ID_SAEB,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      ID_ESCOLA,
      ID_SERIE,
      mulher = TX_RESP_Q001,
      raca = TX_RESP_Q002,
      mes = TX_RESP_Q003,
      idade = TX_RESP_Q004,
      tv_col_nse = TX_RESP_Q005,
      radio_nse = TX_RESP_Q006,
      dvd_nse = TX_RESP_Q007,
      geladeira_nse = TX_RESP_Q008,
      #feeezer_junto_nse = TX_RESP_Q009,
      freezer_separado_nse = TX_RESP_Q010,
      maq_lav_nse = TX_RESP_Q011,
      carro_nse = TX_RESP_Q012,
      computador_nse = TX_RESP_Q013,
      banheiro_nse = TX_RESP_Q014,
      domestica_nse = TX_RESP_Q015,
      quartos_nse = TX_RESP_Q016,
      mora_mae = TX_RESP_Q018,
      escola_mae_nse = TX_RESP_Q019,
      mora_pai = TX_RESP_Q022,
      escola_pai_nse = TX_RESP_Q023,
      resp_reunioes = TX_RESP_Q026,
      pais_incent_estud = TX_RESP_Q027,
      pais_incent_dev_casa = TX_RESP_Q028,
      pais_incent_ler = TX_RESP_Q029,
      pais_incent_presenc = TX_RESP_Q030,
      pais_conv_escola = TX_RESP_Q031,
      #tres_ou_mais_horas_tv_jogos_internet = TX_RESP_Q044, # Incompatível entre os anos
      aluno_trab = TX_RESP_Q046,
      quando_entrou_escola = TX_RESP_Q047,
      ja_reprov = TX_RESP_Q049,
      ja_aband = TX_RESP_Q050
    )
  
  bind_rows(aluno_5ano, aluno_9ano)
}

select_saeb_aluno_cols <- function(.data) {
  id_serie = select(.data, ID_SERIE) %>% distinct() %>% pull()
  expr <- rlang::expr(
    c(
      ANO = ID_PROVA_BRASIL,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      ID_ESCOLA,
      ID_SERIE,
      mulher = TX_RESP_Q001,
      raca = TX_RESP_Q002,
      mes = TX_RESP_Q003,
      idade = TX_RESP_Q004,
      tv_col_nse = TX_RESP_Q005,
      radio_nse = TX_RESP_Q006,
      dvd_nse = TX_RESP_Q007,
      geladeira_nse = TX_RESP_Q008,
      #feeezer_junto_nse = TX_RESP_Q009,
      freezer_separado_nse = TX_RESP_Q010,
      maq_lav_nse = TX_RESP_Q011,
      carro_nse = TX_RESP_Q012,
      computador_nse = TX_RESP_Q013,
      banheiro_nse = TX_RESP_Q014,
      domestica_nse = TX_RESP_Q017,
      quartos_nse = TX_RESP_Q015,
      mora_mae = TX_RESP_Q018,
      escola_mae_nse = TX_RESP_Q019,
      mora_pai = TX_RESP_Q022,
      escola_pai_nse = TX_RESP_Q023,
      resp_reunioes = TX_RESP_Q026,
      pais_incent_estud = TX_RESP_Q027,
      pais_incent_dev_casa = TX_RESP_Q028,
      pais_incent_ler = TX_RESP_Q029,
      pais_incent_presenc = TX_RESP_Q030,
      pais_conv_escola = TX_RESP_Q031,
      #tres_ou_mais_horas_tv_jogos_internet = case_when( # Incompatível entre os anos
      #  id_serie == 5 ~ !!"TX_RESP_Q040",
      #  id_serie == 9 ~ !!"TX_RESP_Q043",
      #),
      aluno_trab = case_when(
        id_serie == 5 ~ !!"TX_RESP_Q042",
        id_serie == 9 ~ !!"TX_RESP_Q045",
      ),
      quando_entrou_escola = case_when(
        id_serie == 5 ~ !!"TX_RESP_Q043",
        id_serie == 9 ~ !!"TX_RESP_Q046",
      ),
      ja_reprov = case_when(
        id_serie == 5 ~ !!"TX_RESP_Q045",
        id_serie == 9 ~ !!"TX_RESP_Q048",
      ),
      ja_aband = case_when(
        id_serie == 5 ~ !!"TX_RESP_Q046",
        id_serie == 9 ~ !!"TX_RESP_Q049",
      )
    )
  )
  pos <- eval_select(expr, .data)
  rlang::set_names(.data[pos], names(pos))
}


select_saeb_escola_cols_2011 <- function(.data, .path_escola_quest_2011) {
  .data %>%
    select(
      ANO = ID_SAEB,
      ID_SERIE,
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM, 
      MEDIA_LP,
      MEDIA_MT
    ) %>%
    inner_join(
      read_saeb(.path_escola_quest_2011, ID_DEPENDENCIA_ADM %in% c(2, 3), ID_UF == 29) %>%
        select(
          ANO = ID_SAEB,
          ID_ESCOLA,
          ID_DEPENDENCIA_ADM,
          ID_LOCALIZACAO,
          ID_UF,
          ID_MUNICIPIO,
          telhado_ice = TX_RESP_Q001, 
          paredes_ice = TX_RESP_Q002, 
          pisos_ice = TX_RESP_Q003, 
          portas_ice = TX_RESP_Q008, 
          janelas_ice = TX_RESP_Q009, 
          banheiros_ice = TX_RESP_Q010, 
          cozinha_ice = TX_RESP_Q011, 
          inst_hidraulicas_ice = TX_RESP_Q012, 
          inst_eletricas_ice = TX_RESP_Q013, 
          muros_ise = TX_RESP_Q016, 
          contr_entrada_aluno_ise = TX_RESP_Q017, 
          contr_entrada_estranho_ise = TX_RESP_Q018, 
          portoes_fechados_durante_aula_ise = TX_RESP_Q019, 
          pol_furtos_ise = TX_RESP_Q023, 
          pol_trafico_dentro_escola_ise = TX_RESP_Q024, 
          pol_trafico_imed_escola_ise = TX_RESP_Q025,
          biblioteca_iie = TX_RESP_Q050,
          quadra_esportes_iie = TX_RESP_Q051,
          laboratorio_iie = TX_RESP_Q052,
          auditorio_iie = TX_RESP_Q053,
          televisao_ipe = TX_RESP_Q043,
          xerox_ipe = TX_RESP_Q038,
          projetor_ipe = TX_RESP_Q041,
          impressora_ipe = TX_RESP_Q039,
          computador_alunos_ipe = TX_RESP_Q031
        )
    ) %>%
    mutate(
      across(c(MEDIA_LP, MEDIA_MT),
             ~ as.double(str_replace_all(.x, ",", "."))) # Substituindo todas as virgulas por pontos, para transformar as colunas de media para numérico
    )
}


select_saeb_escola_cols <- function(.data) {
  .data %>%
    select(
      ANO = ID_PROVA_BRASIL,
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      telhado_ice = TX_RESP_Q007, 
      paredes_ice = TX_RESP_Q008, 
      pisos_ice = TX_RESP_Q009, 
      portas_ice = TX_RESP_Q014, 
      janelas_ice = TX_RESP_Q015, 
      banheiros_ice = TX_RESP_Q016, 
      cozinha_ice = TX_RESP_Q017, 
      inst_hidraulicas_ice = TX_RESP_Q018, 
      inst_eletricas_ice = TX_RESP_Q019, 
      muros_ise = TX_RESP_Q032, 
      contr_entrada_aluno_ise = TX_RESP_Q022, 
      contr_entrada_estranho_ise = TX_RESP_Q023, 
      portoes_fechados_durante_aula_ise = TX_RESP_Q034, 
      pol_furtos_ise = TX_RESP_Q027, 
      pol_trafico_dentro_escola_ise = TX_RESP_Q028, 
      pol_trafico_imed_escola_ise = TX_RESP_Q029,
      biblioteca_iie = TX_RESP_Q057,
      quadra_esportes_iie = TX_RESP_Q058,
      laboratorio_iie = TX_RESP_Q059,
      auditorio_iie = TX_RESP_Q062,
      televisao_ipe = TX_RESP_Q049,
      xerox_ipe = TX_RESP_Q044,
      projetor_ipe = TX_RESP_Q047,
      impressora_ipe = TX_RESP_Q045,
      computador_alunos_ipe = TX_RESP_Q037,
      MEDIA_5EF_LP, 
      MEDIA_5EF_MT,
      MEDIA_9EF_LP,
      MEDIA_9EF_MT
    ) %>%
    inner_join(
      readr::read_csv("./data/intermed/saeb/aluno/aluno_11131517.csv") %>%
        distinct(ID_ESCOLA)
    ) %>%
    pivot_longer(MEDIA_5EF_LP:MEDIA_9EF_MT, names_to = "ID_SERIE_T", values_to = "MEDIA") %>%
    mutate(
      ID_SERIE = as.double(str_extract(ID_SERIE_T, "(5|9)")),
      MEDIA_LP = if_else(str_detect(ID_SERIE_T, "LP"), MEDIA, NA_real_),
      MEDIA_MT = if_else(str_detect(ID_SERIE_T, "MT"), MEDIA, NA_real_)
    ) %>%
    fill(MEDIA_MT, .direction = "up") %>%
    select(-c(ID_SERIE_T, MEDIA)) %>%
    drop_na(MEDIA_LP)
}

select_saeb_professor_cols <- function(.data, .ano) {
  .data %>%
    select(
      ANO = case_when(
        .ano == 2011 ~ !!"ID_SAEB",
        .ano %in% c(2013, 2015, 2017) ~ !!"ID_PROVA_BRASIL"
      ),
      ID_ESCOLA,
      ID_SERIE,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      prof_mulher = TX_RESP_Q001,
      prof_ensino_sup = TX_RESP_Q004,
      prof_50_ou_mais = TX_RESP_Q002,
      #prof_experiencia_lecion # incompatível entre anos
      prof_um_sal_e_meio = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q014",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q010"
      ),
      trab_mais_uma_escola = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q021",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q018"
      ),
      prof_mais_de_um_terco_atv_extraclas = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q036",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q020"
      ),
      aprendizagem_falta_infraes = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q046",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q070"
      ),
      aprendizagem_salarios = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q052",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q075"
      ),
      aprendizagem_falta_interesse = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q058",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q080"
      )
    )
}


select_saeb_diretor_cols <- function(.data, .ano) {
  .data %>%
    select(
      ANO = case_when(
        .ano == 2011 ~ !!"ID_SAEB",
        .ano %in% c(2013, 2015, 2017) ~ !!"ID_PROVA_BRASIL"
      ),
      ID_ESCOLA,
      ID_DEPENDENCIA_ADM,
      ID_LOCALIZACAO,
      ID_UF,
      ID_MUNICIPIO,
      dir_mulher = TX_RESP_Q001,
      dir_ensino_superior = TX_RESP_Q004,
      #dir_mais_5_ou_mais_anos_exper = TX_RESP_Q019, #incompatível entre os anos
      dir_40h_ou_mais_carga_horar =  case_when(
        .ano == 2011 ~ !!"TX_RESP_Q020",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q013"
      ),
      interrup_ativ_grave = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q060",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q072"
      ),
      falta_prof_grave = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q061",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q073"
      ),
      falta_aluno_grave = case_when(
        .ano == 2011 ~ !!"TX_RESP_Q062",
        .ano %in% c(2013, 2015, 2017) ~ !!"TX_RESP_Q074"
      )
    ) %>%
    inner_join(
      readr::read_csv("./data/intermed/saeb/escola/escola_11131517.csv") %>%
        distinct(ID_ESCOLA)
    )
}

# ---- padronizações ----

padr_saeb_aluno <- function(.data) {
  .data  %>%
    rename(
      ESCOLA_MUNICIPAL = ID_DEPENDENCIA_ADM,
      RURAL = ID_LOCALIZACAO
    ) %>%
    mutate(
      across(c(tv_col_nse, radio_nse, carro_nse),
             ~ case_when(
               ANO == 2011 & .x == "D" ~ 1,
               ANO == 2011 & .x == "A" ~ 2,
               ANO == 2011 & .x == "B" ~ 3,
               ANO == 2011 & .x == "C" ~ 4,
               ANO %in% c(2013, 2015, 2017) & .x == "A" ~ 1,
               ANO %in% c(2013, 2015, 2017) & .x == "B" ~ 2,
               ANO %in% c(2013, 2015, 2017) & .x == "C" ~ 3,
               ANO %in% c(2013, 2015, 2017) & .x %in% c("D", "E") ~ 4
             )),
      across(c(dvd_nse, freezer_separado_nse, maq_lav_nse),
             ~ case_when(
                 ANO == 2011 & .x == "B" ~ 1,
                 ANO == 2011 & .x == "A" ~ 2,
                 ANO %in% c(2013, 2015, 2017) & .x == "A" ~ 1,
                 ANO %in% c(2013, 2015, 2017) & .x %in% c("B", "C", "D", "E") ~ 2
             )),
      geladeira_nse = case_when(
        ANO == 2011 & geladeira_nse == "C" ~ 1,
        ANO == 2011 & geladeira_nse == "A" ~ 2,
        ANO == 2011 & geladeira_nse == "B" ~ 3,
        ANO %in% c(2013, 2015, 2017) & geladeira_nse == "A" ~ 1,
        ANO %in% c(2013, 2015, 2017) & geladeira_nse == "B" ~ 2,
        ANO %in% c(2013, 2015, 2017) & geladeira_nse %in% c("C", "D", "E") ~ 3
      ),
      computador_nse = case_when(
        ANO == 2011 & computador_nse == "C" ~ 1,
        ANO == 2011 & computador_nse %in% c("A", "B") ~ 2,
        ANO %in% c(2013, 2015, 2017) & computador_nse == "A" ~ 1,
        ANO %in% c(2013, 2015, 2017) & computador_nse %in% c("B", "C", "D", "E") ~ 2
      ),
      across(
        c(banheiro_nse, quartos_nse),
        ~ case_when(
          ANO == 2011 & .x == "E" ~ 1,
          ANO == 2011 & .x == "A" ~ 2,
          ANO == 2011 & .x == "B" ~ 3,
          ANO == 2011 & .x == "C" ~ 4,
          ANO == 2011 & .x == "D" ~ 5,
          ANO %in% c(2013, 2015, 2017) & .x == "A" ~ 1,
          ANO %in% c(2013, 2015, 2017) & .x == "B" ~ 2,
          ANO %in% c(2013, 2015, 2017) & .x == "C" ~ 3,
          ANO %in% c(2013, 2015, 2017) & .x == "D" ~ 4,
          ANO %in% c(2013, 2015, 2017) & .x == "E" ~ 5
        ),
      ),
      domestica_nse = case_when(
        ANO == 2011 & domestica_nse %in% c("A", "D") ~ 1, # Feito para padronizar com as questões de 2013/15/17
        ANO == 2011 & domestica_nse %in% c("B", "C") ~ 2,
        ANO %in% c(2013, 2015, 2017) & domestica_nse == "A" ~ 1,
        ANO %in% c(2013, 2015, 2017) & domestica_nse %in% c("B", "C", "D", "E") ~ 1
      ),
      across(
        c(escola_mae_nse, escola_pai_nse),
        ~ case_when(
          .x %in% c("A", "B") ~ 1,
          .x == "C" ~ 2,
          .x == "D" ~ 3,
          .x == "E" ~ 4,
          .x == "F" ~ 5
        )
      ),
      entr_creche = case_when(
        quando_entrou_escola == "A" ~ 1,
        quando_entrou_escola %in% c("B", "C", "D") ~ 0
      ),
      entr_pre_esc = case_when(
        quando_entrou_escola == "B" ~ 1,
        quando_entrou_escola %in% c("A", "C", "D") ~ 0
      ),
      entr_prim_serie = case_when(
        quando_entrou_escola == "C" ~ 1,
        quando_entrou_escola %in% c("A", "B", "D") ~ 0
      ),
      entr_dep_prim_serie = case_when(
        quando_entrou_escola == "D" ~ 1,
        quando_entrou_escola %in% c("A", "B", "C") ~ 0
      ),
      mulher = case_when(
        mulher == "B" ~ 1,
        mulher == "A" ~ 0
      ),
      negro = case_when(
        raca %in% c("B", "C") ~ 1,
        raca %in% c("A", "D", "E") ~ 0
      ),
      indigena = case_when(
        raca %in% c("E") ~ 1,
        raca %in% c("A", "B", "C", "D") ~ 0
      ),
      mora_mae_e_pai = case_when(
        mora_mae %in% c("A", "C") & mora_pai %in% c("A", "C") ~ 1, # Mora com mãe/pai ou responsável
        (mora_mae == "B" & !(mora_pai %in% c(".", "*"))) | (mora_pai == "B" & !(mora_mae %in% c(".", "*"))) ~ 0,
        mora_mae %in% c(".", "*") | mora_pai %in% c(".", "*") ~ NA_real_
      ),
      across(
        c(resp_reunioes:aluno_trab),
        ~ case_when(
          .x == "A" ~ 1,
          .x %in% c("B", "C") ~ 0
        )
      ),
      across(
        c(ja_reprov, ja_aband),
        ~ case_when(
          .x == "A" ~ 0,
          .x %in% c("B", "C") ~ 1
        )
      ),
      atraso = case_when(
        ID_SERIE == 5 & idade %in% c("A", "B", "C", "D") ~ 0,
        ID_SERIE == 5 & idade %in% c("E", "F", "G", "H") ~ 1,
        ID_SERIE == 9 & idade == "C" & mes %in% c("D", "E", "F", "G", "H", "I", "J", "K", "L") ~ 0,
        ID_SERIE == 9 & idade %in% c("A", "B") ~ 0,
        ID_SERIE == 9 & idade == "C" & mes %in% c("A", "B", "C") ~ 1,
        ID_SERIE == 9 & idade %in% c("D", "E", "F", "G", "H") ~ 1,
        ID_SERIE == 9 & idade == "C" & ja_reprov == 1 & ja_aband == 1 ~ 0,
        ID_SERIE == 9 & idade == "C" & (ja_reprov == 2 | ja_aband == 2) ~ 1,
        ID_SERIE == 5 & idade %in% c("A", "B", "C", "D") ~ 0,
        ID_SERIE == 5 & idade %in% c("E", "F", "G", "H") ~ 1
      ),
      ESCOLA_MUNICIPAL = ESCOLA_MUNICIPAL - 2,
      RURAL = RURAL - 1
    ) %>%
    select(-c(idade, mes, quando_entrou_escola, raca, mora_mae, mora_pai))
}


padr_saeb_escola <- function(.data) {
  .data %>%
    rename(
      ESCOLA_MUNICIPAL = ID_DEPENDENCIA_ADM,
      RURAL = ID_LOCALIZACAO
    ) %>%
    mutate(
      across(c(telhado_ice:inst_eletricas_ice, biblioteca_iie:computador_alunos_ipe), 
             ~ case_when(
               .x == "D" ~ 1,
               .x == "C" ~ 2,
               .x == "B" ~ 3,
               .x == "A" ~ 4)),
      across(c(muros_ise:pol_trafico_imed_escola_ise), 
             ~ case_when(
               ANO %in% c(2011) & .x == "B" ~ 0,
               ANO %in% c(2011) & .x == "A" ~ 1,
               ANO %in% c(2013, 2015, 2017) & .x == "D" ~ 0,
               ANO %in% c(2013, 2015, 2017) & .x %in% c("A", "B", "C", "D") ~ 1)),
      ESCOLA_MUNICIPAL = ESCOLA_MUNICIPAL - 2,
      RURAL = RURAL - 1
    )
}


padr_saeb_professor <- function(.data) {
  .data %>%
    rename(
      ESCOLA_MUNICIPAL = ID_DEPENDENCIA_ADM,
      RURAL = ID_LOCALIZACAO
    ) %>%
    mutate(
      ESCOLA_MUNICIPAL = ESCOLA_MUNICIPAL - 2,
      RURAL = RURAL - 1,
      across(c(aprendizagem_falta_infraes:aprendizagem_falta_interesse),
             ~ case_when(
               .x == "A" ~ 1,
               .x == "B" ~ 0
             )),
      prof_mulher = case_when(
        prof_mulher == "A" ~ 0,
        prof_mulher == "B" ~ 1
      ),
      prof_ensino_sup = case_when(
        prof_ensino_sup %in% c("A", "B", "C") ~ 0,
        prof_ensino_sup %in% c("D", "E", "F", "G", "H", "I") ~ 1
      ),
      prof_50_ou_mais = case_when(
        prof_50_ou_mais %in% c("E", "F") ~ 1,
        prof_50_ou_mais %in% c("A", "B", "C", "D") ~ 0
      ),
      prof_um_sal_e_meio = case_when(
        ANO == 2011 & prof_um_sal_e_meio %in% c("A", "B") ~ 1,
        ANO == 2011 & prof_um_sal_e_meio %in% c("C", "D", "E", "F", "G", "H", "I", "J", "K") ~ 0,
        ANO %in% c(2013, 2015, 2017) & prof_um_sal_e_meio == "A" ~ 1,
        ANO %in% c(2013, 2015, 2017) & prof_um_sal_e_meio %in% c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K") ~ 0,
      ),
      trab_mais_uma_escola = case_when(
        trab_mais_uma_escola == "A" ~ 0,
        trab_mais_uma_escola %in% c("B", "C", "D") ~ 1
      ),
      prof_mais_de_um_terco_atv_extraclas = case_when(
        ANO == 2011 & prof_mais_de_um_terco_atv_extraclas == "C" ~ 1,
        ANO == 2011 & prof_mais_de_um_terco_atv_extraclas %in% c("A", "B") ~ 0,
        ANO %in% c(2013, 2015, 2017) & prof_mais_de_um_terco_atv_extraclas == "D" ~ 1,
        ANO %in% c(2013, 2015, 2017) & prof_mais_de_um_terco_atv_extraclas %in% c("A", "B", "C") ~ 0
      )
    )
}


padr_saeb_diretor <- function(.data) {
  .data %>%
    rename(
      ESCOLA_MUNICIPAL = ID_DEPENDENCIA_ADM,
      RURAL = ID_LOCALIZACAO
    ) %>%
    mutate(
      dir_mulher = case_when(
        dir_mulher == "B" ~ 1,
        dir_mulher == "A" ~ 0
      ),
      dir_ensino_superior = case_when(
        dir_ensino_superior %in% c("A", "B", "C") ~ 0,
        dir_ensino_superior %in% c("D", "E", "F", "G", "H", "I") ~ 1
      ),
      dir_40h_ou_mais_carga_horar = case_when(
        ANO %in% c(2011) & dir_40h_ou_mais_carga_horar %in% c("C", "D") ~ 1,
        ANO %in% c(2011) & dir_40h_ou_mais_carga_horar %in% c("A", "B") ~ 0,
        ANO %in% c(2013, 2015, 2017) & dir_40h_ou_mais_carga_horar %in% c("A", "B") ~ 1,
        ANO %in% c(2013, 2015, 2017) & dir_40h_ou_mais_carga_horar %in% c("C", "D") ~ 0,
      ),
      across(c(interrup_ativ_grave:falta_aluno_grave),
             ~ case_when(
               ANO %in% c(2007, 2009, 2011) & .x == "C" ~ 1,
               ANO %in% c(2007, 2009, 2011) & .x %in% c("A", "B") ~ 0,
               ANO %in% c(2013, 2015, 2017) & .x == "D" ~ 1,
               ANO %in% c(2013, 2015, 2017) & .x %in% c("A", "B", "C") ~ 0
             )),
      ESCOLA_MUNICIPAL = ESCOLA_MUNICIPAL - 2,
      RURAL = RURAL - 1
    )
}