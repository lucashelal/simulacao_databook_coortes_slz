# =============================================================================
# 04_databook.R — RUNNER (banco real, modo simples: SÓ TABELAS)
#
# Lê data/raw/dataset.xlsx, monta o objeto `df`, classifica TODAS as 235
# variáveis automaticamente (sem listar nomes) e gera o databook em
# output/databook.html — somente tabelas, sem gráficos nem figuras.
#
# Como cada variável é classificada (sem rótulos, pois vem de xlsx):
#   - texto (chr)            -> categórica  (ou identificador se quase única)
#   - data/hora (dttm/Date)  -> data        (resumo mín/mediana/máx)
#   - numérica c/ >12 níveis -> contínua
#   - numérica c/ <=12 níveis-> categórica  (binárias, contagens curtas)
#
# Uso (a partir da raiz do projeto):
#   source("scripts/04_databook.R")      # gera output/databook.html
#   # ou:  Rscript scripts/04_databook.R
# =============================================================================

# 1) Motor do databook (define as funções; não executa nada por si só)
source("scripts/03_gera_databook.R")

# 2) Lê o banco real (xlsx) -> objeto df no ambiente
if (!requireNamespace("readxl", quietly = TRUE))
  stop("Pacote 'readxl' necessário. Rode: install.packages('readxl')")

df <- readxl::read_excel("data/raw/dataset.xlsx", guess_max = 1048576)
df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

# 3) Databook automático das 235 variáveis — SOMENTE TABELAS
databook_auto(
  df,
  arquivo         = "output/databook.html",
  titulo          = "Databook — Coorte SLZ (banco real)",
  graficos        = FALSE,      # <- sem gráficos/figuras; só tabelas
  limiar_continua = 12          # numérica com >12 níveis distintos -> contínua
  #
  # AJUSTE FINO (opcional): variáveis que são CÓDIGO numérico (não medida)
  # podem cair como "contínua" por terem muitos níveis. Como o xlsx não traz
  # rótulos de valor, force-as como categóricas aqui se quiser:
  # , categoricas = c("a_bairro", "a_seriepai", "a_seriemae", "a_firma",
  #                   "a_ocupa", "a_chefe", "a_horanasc", "a_dia")
)

cat("\u2713 output/databook.html gerado (somente tabelas).\n")

# -----------------------------------------------------------------------------
# OBS. sobre códigos-sentinela: este banco usa 88/99/999.9/8888/9999 como
# "ignorado/não se aplica". O databook os exibe como valores reais (eles
# inflam médias e frequências). Se quiser tratá-los como NA antes de gerar,
# faça algo como (ajuste a lista conforme o dicionário):
#   sentinelas <- c(88, 99, 999.9, 99.9, 8888, 9999, 8.8, 9.9)
#   num <- vapply(df, is.numeric, logical(1))
#   df[num] <- lapply(df[num], function(v) { v[v %in% sentinelas] <- NA; v })
# e então rode o databook_auto novamente.
# -----------------------------------------------------------------------------
