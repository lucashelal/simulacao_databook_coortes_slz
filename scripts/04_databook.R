source("scripts/03_gera_databook.R")   # só define as funções, não executa nada

databook_html(
  construir_databook(df),                 # sem o dic -> modo simplório
  "output/databook.html",
  titulo = "Databook — Coorte simulada"
)

browseURL("output/databook.html")          # abre no navegador
