# =============================================================================
# 01_simula_dados.R
# Gera o banco simulado com relações de probabilidade explícitas, salva a
# versão ROTULADA (preserva o dataset original) e a versão CODIFICADA em
# números. As equivalências código <-> rótulo vêm do dicionário JSON canônico.
# Rodar a partir da raiz do projeto (RProj).
# =============================================================================

set.seed(42)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(jsonlite)
})

# ---- 1. Parâmetros ----------------------------------------------------------
n_total   <- 2000
n_por_ano <- 1000
anos      <- c(1997, 2013)
cidades   <- c("SLZ", "PEL", "RP")

# ---- 2. Estrutura base + sociodemográficas ----------------------------------
df <- tibble(
  id     = 1:n_total,
  ano    = rep(anos, each = n_por_ano),
  cidade = sample(cidades, n_total, replace = TRUE)
) |>
  mutate(
    maternidade = case_when(
      cidade == "SLZ" ~ sample(c("A1", "A2", "A3"), n(), replace = TRUE),
      cidade == "PEL" ~ sample(c("B1", "B2", "B3"), n(), replace = TRUE),
      cidade == "RP"  ~ sample(c("C1", "C2", "C3"), n(), replace = TRUE)
    ),
    idade_mae = round(pmax(18, pmin(45, rnorm(n(), mean = 28, sd = 6)))),
    idade_mae_categorica = cut(
      idade_mae,
      breaks = c(0, 20, 30, 40, 100),
      labels = c("18-20", "21-30", "31-40", ">40"),
      include.lowest = TRUE
    ),
    renda_mae_reais = case_when(
      ano == 1997 ~ round(exp(rnorm(n(), mean = 6.5, sd = 0.8)), 2),
      ano == 2013 ~ round(exp(rnorm(n(), mean = 7.8, sd = 0.9)), 2)
    ),
    salario_minimo = case_when(ano == 1997 ~ 120, ano == 2013 ~ 678),
    renda_mae_quintis_sm = case_when(
      renda_mae_reais < salario_minimo     ~ "< 1 SM",
      renda_mae_reais < salario_minimo * 2 ~ "1-2 SM",
      renda_mae_reais < salario_minimo * 3 ~ "2-3 SM",
      renda_mae_reais < salario_minimo * 5 ~ "3-5 SM",
      TRUE                                 ~ "> 5 SM"
    ),
    mae_solteira = ifelse(
      renda_mae_reais < salario_minimo * 2,
      sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.45, 0.55)),
      sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.20, 0.80))
    )
  ) |>
  select(-salario_minimo)

# ---- 3. Antropometria materna -----------------------------------------------
df <- df |>
  mutate(
    peso_mae_pre_gestacao_kg = round(
      pmax(45, pmin(120, rnorm(n(), mean = 65, sd = 12))), 1
    ),
    peso_mae_pos_gestacao_kg = round(
      peso_mae_pre_gestacao_kg + rnorm(n(), mean = 12, sd = 4), 1
    ),
    peso_mae_categoria = cut(
      peso_mae_pre_gestacao_kg,
      breaks = c(0, 50, 65, 80, 200),
      labels = c("Baixo peso", "Normal", "Sobrepeso", "Obesidade"),
      include.lowest = TRUE
    )
  )

# ---- 4. Pré-natal -----------------------------------------------------------
df <- df |>
  mutate(
    salario_minimo = case_when(ano == 1997 ~ 120, ano == 2013 ~ 678),
    prob_prenatal = case_when(
      ano == 1997 & renda_mae_reais < salario_minimo     ~ 0.35,
      ano == 1997 & renda_mae_reais < salario_minimo * 3 ~ 0.55,
      ano == 1997                                        ~ 0.70,
      ano == 2013 & renda_mae_reais < salario_minimo     ~ 0.60,
      ano == 2013 & renda_mae_reais < salario_minimo * 3 ~ 0.80,
      ano == 2013                                        ~ 0.90
    ),
    prenatal_6_consultas_12sem =
      ifelse(runif(n()) < prob_prenatal, "Sim", "Não")
  ) |>
  select(-prob_prenatal, -salario_minimo)

# ---- 5. Comorbidades gestacionais -------------------------------------------
df <- df |>
  mutate(
    imc_pre = peso_mae_pre_gestacao_kg / (1.62^2),
    prob_diabetes = case_when(
      idade_mae >= 35 & imc_pre >= 25 ~ 0.15,
      idade_mae >= 35                 ~ 0.10,
      imc_pre >= 25                   ~ 0.08,
      TRUE                            ~ 0.03
    ),
    diabetes_gestacional =
      ifelse(runif(n()) < prob_diabetes, "Sim", "Não"),
    prob_hipertensao = case_when(
      idade_mae >= 35 & imc_pre >= 25            ~ 0.12,
      idade_mae >= 35                            ~ 0.08,
      imc_pre >= 25 &
        prenatal_6_consultas_12sem == "Não"      ~ 0.10,
      imc_pre >= 25                              ~ 0.05,
      prenatal_6_consultas_12sem == "Não"        ~ 0.05,
      TRUE                                       ~ 0.02
    ),
    hipertensao_gestacional =
      ifelse(runif(n()) < prob_hipertensao, "Sim", "Não"),
    prob_preeclampsia = case_when(
      hipertensao_gestacional == "Sim" & idade_mae >= 35 ~ 0.20,
      hipertensao_gestacional == "Sim"                   ~ 0.12,
      idade_mae >= 35 & imc_pre >= 25                    ~ 0.05,
      TRUE                                               ~ 0.02
    ),
    pre_eclampsia =
      ifelse(runif(n()) < prob_preeclampsia, "Sim", "Não"),
    eclampsia = ifelse(
      pre_eclampsia == "Sim" & runif(n()) < 0.05, "Sim", "Não"
    )
  ) |>
  select(-imc_pre, -prob_diabetes, -prob_hipertensao, -prob_preeclampsia)

# ---- 6. Parto ---------------------------------------------------------------
df <- df |>
  mutate(
    n_comorbidades =
      (diabetes_gestacional == "Sim") +
      (hipertensao_gestacional == "Sim") +
      (pre_eclampsia == "Sim") +
      (eclampsia == "Sim"),
    prob_alto_risco = case_when(
      n_comorbidades >= 2 ~ 0.80,
      n_comorbidades == 1 ~ 0.45,
      idade_mae >= 35     ~ 0.30,
      TRUE                ~ 0.15
    ),
    gravidade_parto = ifelse(
      runif(n()) < prob_alto_risco, "Alto risco", "Baixo risco"
    ),
    prob_cesariana = case_when(
      eclampsia == "Sim"               ~ 0.95,
      pre_eclampsia == "Sim"           ~ 0.75,
      diabetes_gestacional == "Sim" &
        hipertensao_gestacional == "Sim" ~ 0.60,
      gravidade_parto == "Alto risco"  ~ 0.50,
      idade_mae >= 40                  ~ 0.45,
      idade_mae < 20                   ~ 0.40,
      TRUE                             ~ 0.25
    ),
    tipo_parto =
      ifelse(runif(n()) < prob_cesariana, "Cesárea", "Normal")
  ) |>
  select(-n_comorbidades, -prob_alto_risco, -prob_cesariana)

# ---- 7. Desfechos neonatais -------------------------------------------------
df <- df |>
  mutate(
    peso_ao_nascer_kg = case_when(
      pre_eclampsia == "Sim" & eclampsia == "Sim" ~
        round(pmax(1.8, rnorm(n(), mean = 2.4, sd = 0.4)), 2),
      pre_eclampsia == "Sim" ~
        round(pmax(1.8, rnorm(n(), mean = 2.7, sd = 0.5)), 2),
      diabetes_gestacional == "Sim" ~
        round(pmax(2.0, rnorm(n(), mean = 3.5, sd = 0.6)), 2),
      peso_mae_pre_gestacao_kg < 50 ~
        round(pmax(1.8, rnorm(n(), mean = 2.6, sd = 0.5)), 2),
      peso_mae_pre_gestacao_kg >= 80 ~
        round(pmin(4.5, rnorm(n(), mean = 3.6, sd = 0.7)), 2),
      TRUE ~
        round(pmax(1.8, pmin(4.5, rnorm(n(), mean = 3.2, sd = 0.6))), 2)
    ),
    baixo_peso_ao_nascer =
      ifelse(peso_ao_nascer_kg < 2.5, "Sim", "Não"),
    prob_complicacoes = case_when(
      peso_ao_nascer_kg < 2.0 & pre_eclampsia == "Sim" ~ 0.80,
      peso_ao_nascer_kg < 2.0                          ~ 0.60,
      pre_eclampsia == "Sim"                           ~ 0.45,
      diabetes_gestacional == "Sim"                    ~ 0.35,
      gravidade_parto == "Alto risco"                  ~ 0.30,
      tipo_parto == "Cesárea" &
        gravidade_parto == "Alto risco"                ~ 0.25,
      TRUE                                             ~ 0.08
    ),
    complicacoes_neonatais =
      ifelse(runif(n()) < prob_complicacoes, "Sim", "Não"),
    prob_obito = case_when(
      peso_ao_nascer_kg < 1.5 ~ 0.15,
      peso_ao_nascer_kg < 2.0 &
        complicacoes_neonatais == "Sim" ~ 0.08,
      complicacoes_neonatais == "Sim"   ~ 0.03,
      TRUE                              ~ 0.001
    ),
    obito_neonatal_precoce =
      ifelse(runif(n()) < prob_obito, "Sim", "Não")
  ) |>
  select(-prob_complicacoes, -prob_obito)

# ---- 8. Missings (MCAR) -----------------------------------------------------
# Correção: aplicar a função diretamente no pipe (o df entra como 1o argumento).
# A ordem das chamadas é preservada porque cada sample() consome o gerador.
introduzir_missings <- function(df, col, pct = 0.02) {
  n_miss <- round(nrow(df) * pct)
  idx <- sample(seq_len(nrow(df)), size = n_miss, replace = FALSE)
  df[[col]][idx] <- NA
  df
}

df <- df |>
  introduzir_missings("renda_mae_reais",            0.04) |>
  introduzir_missings("peso_mae_pre_gestacao_kg",   0.03) |>
  introduzir_missings("diabetes_gestacional",       0.02) |>
  introduzir_missings("hipertensao_gestacional",    0.02) |>
  introduzir_missings("pre_eclampsia",              0.02) |>
  introduzir_missings("prenatal_6_consultas_12sem", 0.03) |>
  introduzir_missings("peso_ao_nascer_kg",          0.02) |>
  introduzir_missings("complicacoes_neonatais",     0.02)

# ---- 9. Versão ROTULADA (preserva o dataset original) -----------------------
ordem <- c(
  "id", "ano", "cidade", "maternidade",
  "idade_mae", "idade_mae_categorica",
  "renda_mae_reais", "renda_mae_quintis_sm", "mae_solteira",
  "peso_mae_pre_gestacao_kg", "peso_mae_pos_gestacao_kg",
  "peso_mae_categoria", "prenatal_6_consultas_12sem",
  "diabetes_gestacional", "hipertensao_gestacional",
  "pre_eclampsia", "eclampsia", "gravidade_parto", "tipo_parto",
  "peso_ao_nascer_kg", "baixo_peso_ao_nascer",
  "complicacoes_neonatais", "obito_neonatal_precoce"
)

df_rotulado <- df |>
  mutate(
    idade_mae_categorica = as.character(idade_mae_categorica),
    peso_mae_categoria   = as.character(peso_mae_categoria),
    mae_solteira         = ifelse(mae_solteira, "Sim", "Não")
  ) |>
  select(all_of(ordem)) |>
  arrange(ano, cidade, id)

dir.create("data/simulado", recursive = TRUE, showWarnings = FALSE)

saveRDS(df_rotulado, "data/simulado/coorte_rotulada.rds")

write.csv(df_rotulado, "data/simulado/coorte_rotulada.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- 10. Recodificação número <- rótulo, guiada pelo JSON canônico ----------
dic <- fromJSON("dicionario/dicionario.json", simplifyVector = FALSE)

recodificar <- function(df, dic) {
  for (v in dic$variaveis) {
    if (!identical(v$tipo, "categorica")) next
    if (is.null(v$valores) || length(v$valores) == 0) next
    col <- df[[v$nome]]
    if (is.numeric(col)) next  # já codificada (ex.: ano)
    
    # rótulo presente nos dados crus: rotulo_origem, ou rotulo se ausente
    rot <- vapply(v$valores, function(x) {
      if (!is.null(x$rotulo_origem)) x$rotulo_origem else x$rotulo
    }, character(1))
    cod <- vapply(v$valores, function(x) as.numeric(x$codigo), numeric(1))
    
    mapa <- setNames(cod, rot)
    df[[v$nome]] <- unname(mapa[as.character(col)])
  }
  df
}

df_codificado <- recodificar(df_rotulado, dic)

saveRDS(df_codificado, "data/simulado/coorte_codificada.rds")
write.csv(df_codificado, "data/simulado/coorte_codificada.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- 11. Validação ----------------------------------------------------------
cat("\n=== DATASET CRIADO ===\n")
cat("Linhas :", nrow(df_codificado), "\n")
cat("Colunas:", ncol(df_codificado), "\n\n")
cat("Missings por variável:\n")
print(colSums(is.na(df_codificado)))
cat("\nPrévia (versão codificada):\n")
print(utils::head(df_codificado))
cat("\nArquivos salvos em data/simulado/ (rotulada e codificada).\n")