set.seed(123)
pacman::p_load(descr)
# ==============================================================
# BLOCO 1 — BANCO ORIGINAL (mantido sem alterações)
# ==============================================================
n <- 2000

idade_mae <- rnorm(n, mean = 28, sd = 6)
idade_mae <- round(idade_mae, 0)

sexo_crianca <- rbinom(n, size = 1, prob = 0.51)

escolaridade <- sample(1:4, size = n, replace = TRUE,
                       prob = c(0.20, 0.30, 0.30, 0.20))
# 1 = fundamental incompleto | 2 = fundamental completo/médio incompleto
# 3 = médio completo | 4 = superior (completo ou incompleto)

renda_cod <- sample(1:3, size = n, replace = TRUE,
                    prob = c(0.40, 0.40, 0.20))
renda_familiar <- factor(renda_cod,
                         levels = 1:3,
                         labels = c("Baixa", "Média", "Alta"))

prob_prematuro <- ifelse(renda_familiar == "Baixa", 0.15, 0.07)
prematuro <- rbinom(n, size = 1, prob = prob_prematuro)

peso_nascer <- ifelse(
  prematuro == 1,
  rnorm(n, mean = 2300, sd = 400),
  rnorm(n, mean = 3300, sd = 400)
)

df <- data.frame(
  idade_mae,
  sexo_crianca,
  escolaridade,
  renda_familiar,
  prematuro,
  peso_nascer
)

# ==============================================================
# BLOCO 2 — NOVAS VARIÁVEIS: INSEGURANÇA ALIMENTAR
# ==============================================================

# --------------------------------------------------------------
# Pergunta 1 — Consumiu refrigerante ontem? (1 = Sim, 0 = Não)
# Probabilidade decresce com a escolaridade materna (0,35 a 0,20)
# --------------------------------------------------------------
prob_refrigerante <- 0.35 - 0.05 * (escolaridade - 1)
refrigerante_ontem <- rbinom(n, size = 1, prob = prob_refrigerante)


# --------------------------------------------------------------
# Pergunta 2 — Consumiu frutas ontem? (1 = Sim, 0 = Não)
# Probabilidade cresce com escolaridade e com a renda familiar
# --------------------------------------------------------------
prob_frutas <- 0.40 + 0.05 * (escolaridade - 1) +
  ifelse(renda_familiar == "Alta", 0.15,
         ifelse(renda_familiar == "Média", 0.05, 0))
prob_frutas <- pmin(prob_frutas, 0.95)  # garante limite teórico de probabilidade
frutas_ontem <- rbinom(n, size = 1, prob = prob_frutas)

# --------------------------------------------------------------
# Variável derivada 1 — Consumo alimentar adequado
# Definição: NÃO consumiu refrigerante E consumiu frutas no dia anterior
# (herda o efeito de renda/escolaridade por construção, via pergunta_1/pergunta_2)
# --------------------------------------------------------------
consumo_adequado <- ifelse(refrigerante_ontem == 0 & frutas_ontem == 1, 1, 0)

# --------------------------------------------------------------
# Escore de insegurança alimentar (0 a 8), inspirado na escala
# reduzida EBIA/PNAD (versão sem moradores menores de 18 anos),
# com categorização: 0 segurança | 1-3 leve | 4-5 moderada | 6-8 grave
#
# Probabilidades calibradas por estrato de renda para reproduzir,
# em média, a prevalência-alvo de 40% / 30% / 20% / 10%
# --------------------------------------------------------------
probs_inseg <- list(
  "Baixa" = c(seg = 0.25, leve = 0.35, moderada = 0.35, grave = 0.05),
  "Média" = c(seg = 0.30, leve = 0.30, moderada = 0.20, grave = 0.20),
  "Alta"  = c(seg = 0.15, leve = 0.35, moderada = 0.20, grave = 0.30)
)

categoria_inseg <- character(n)
for (nivel in levels(renda_familiar)) {
  idx <- which(renda_familiar == nivel)
  categoria_inseg[idx] <- sample(
    names(probs_inseg[[nivel]]),
    size = length(idx),
    replace = TRUE,
    prob = probs_inseg[[nivel]]
  )
}

# Sorteia o escore dentro da faixa correspondente à categoria sorteada
sorteia_escore <- function(cat) {
  switch(cat,
         seg      = 0L,
         leve     = sample(1:3, 1),
         moderada = sample(4:5, 1),
         grave    = sample(6:8, 1)
  )
}
escore_inseguranca <- vapply(categoria_inseg, sorteia_escore, integer(1))

categoria_inseguranca <- factor(
  categoria_inseg,
  levels = c("seg", "leve", "moderada", "grave"),
  labels = c("Segurança alimentar", "Insegurança leve",
             "Insegurança moderada", "Insegurança grave"),
  ordered = TRUE
)

# ==============================================================
# BLOCO 3 — NOVA VARIÁVEL: CONSUMO ENERGÉTICO
# ==============================================================

# --------------------------------------------------------------
# Pergunta 3 — caloria_total (kcal consumidas no dia)
# Distribuição assimétrica à direita (log-normal), com média
# deslocada por: consumo de refrigerante (+) e renda alta (+)
# Truncada no intervalo fisiologicamente plausível [500, 4500]
# --------------------------------------------------------------
log_mu <- log(1800) +
  0.15 * refrigerante_ontem +
  ifelse(renda_familiar == "Alta", 0.10,
         ifelse(renda_familiar == "Média", 0.04, 0))

caloria_total <- rlnorm(n, meanlog = log_mu, sdlog = 0.30)
caloria_total <- pmin(pmax(caloria_total, 500), 4500)
caloria_total <- round(caloria_total, 0)

# ==============================================================
# BLOCO 4 — BANCO EXPANDIDO
# ==============================================================
df_expandido <- data.frame(
  df,
  refrigerante_ontem,
  frutas_ontem,
  consumo_adequado,
  escore_inseguranca,
  categoria_inseguranca,
  caloria_total
)

prev_inseg <- prop.table(table(df_expandido$categoria_inseguranca))
prev_inseg

head(df_expandido)
summary(df_expandido[c("escore_inseguranca", "caloria_total")])

# --------------------------------------------------------------
# Checagem do gradiente social (renda x insegurança alimentar)
# --------------------------------------------------------------
round(prop.table(table(df_expandido$renda_familiar,
                       df_expandido$categoria_inseguranca), margin = 1), 2)

# Prevalência marginal (deve ficar próxima de 40/30/20/10)
round(prop.table(table(df_expandido$categoria_inseguranca)), 2)