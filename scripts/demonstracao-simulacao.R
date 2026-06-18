# ============================================================
# Simulação de banco de dados — Epidemiologia Nutricional
# Criação de variáveis simuladas em R
# ============================================================
#
# 1. Definir a semente aleatória
#    Isso garante que TODO MUNDO, ao rodar o mesmo código,
#    obtenham exatamente os mesmos números (reprodutibilidade)
set.seed(123)

# 2. Definir o tamanho da amostra simulada
n <- 200


# ------------------------------------------------------------
# Variável 1 — IDADE MATERNA (contínua)
# ------------------------------------------------------------
# rnorm() gera números aleatórios de uma distribuição normal
# mean = média da distribuição, sd = desvio-padrão
idade_mae <- rnorm(n, mean = 28, sd = 6)

# Arredondamos para simular idade em anos completos
idade_mae <- round(idade_mae)


# ------------------------------------------------------------
# Variável 2 — SEXO DA CRIANÇA (dummy / binária)
# ------------------------------------------------------------
# rbinom(n, size, prob) simula n resultados binários (0 ou 1)
# prob = probabilidade de o resultado ser "1"
# Aqui: 1 = masculino, 0 = feminino
sexo_crianca <- rbinom(n, size = 1, prob = 0.51)


# ------------------------------------------------------------
# Variável 3 — ESCOLARIDADE MATERNA (categórica, SEM labels)
# ------------------------------------------------------------
# sample() sorteia valores de um vetor de opções, com probabilidades
# diferentes para cada categoria (prob soma 1)
# Propositalmente deixamos só os códigos numéricos (1 a 4),
# sem atribuir rótulos — para contrastar com a próxima variável
escolaridade <- sample(1:4, size = n, replace = TRUE,
                       prob = c(0.20, 0.30, 0.30, 0.20))
# 1 = fundamental incompleto | 2 = fundamental completo/médio incompleto
# 3 = médio completo | 4 = superior (completo ou incompleto)
# -> no banco, esses significados existem só no comentário, não no R


# ------------------------------------------------------------
# Variável 4 — RENDA FAMILIAR (categórica, COM labels)
# ------------------------------------------------------------
renda_cod <- sample(1:3, size = n, replace = TRUE,
                    prob = c(0.40, 0.40, 0.20))

# factor() transforma os códigos numéricos em categorias rotuladas
# levels = códigos originais | labels = texto correspondente
renda_familiar <- factor(renda_cod,
                         levels = 1:3,
                         labels = c("Baixa", "Média", "Alta"))


# ------------------------------------------------------------
# Variável 5 — PREMATURIDADE (dummy, CONDICIONAL à renda)
# ------------------------------------------------------------
# Primeira condicionalidade: a chance de o bebê ser prematuro
# depende da renda familiar (reproduzindo uma associação real
# de iniquidade em saúde)
prob_prematuro <- ifelse(renda_familiar == "Baixa", 0.15, 0.07)

prematuro <- rbinom(n, size = 1, prob = prob_prematuro)


# ------------------------------------------------------------
# Variável 6 — PESO AO NASCER (contínua, CONDICIONAL à prematuridade)
# ------------------------------------------------------------
# Segunda condicionalidade: o peso médio ao nascer muda conforme
# o bebê é prematuro ou não
peso_nascer <- ifelse(
  prematuro == 1,
  rnorm(n, mean = 2300, sd = 400),
  rnorm(n, mean = 3300, sd = 400)
)


# ------------------------------------------------------------
# Juntando tudo em um único banco de dados
# ------------------------------------------------------------
banco <- data.frame(
  idade_mae,
  sexo_crianca,
  escolaridade,
  renda_familiar,
  prematuro,
  peso_nascer
)

# Conferindo a estrutura e o conteúdo do banco simulado
str(banco)
head(banco)
summary(banco)