# simulacao_databook_coortes_slz

Pipeline reprodutível para (1) simular um banco fake com relações de
probabilidade explícitas e (2) gerar o databook completo de forma
automatizada, sem documentar variável por variável.

## Como gerar tudo

1. Abrir `simulacao_databook_coortes_slz.Rproj` no RStudio.
2. Rodar `source("render.R")` — gera os dois PDFs em `output/`.

## Saídas
- `output/01_relatorio_metodos.pdf` — explica o banco e o código.
- `output/02_databook.pdf` — o databook.
