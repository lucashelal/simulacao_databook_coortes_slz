# =============================================================================
# 03_gera_databook.R
# Gera um databook organizado de TODAS as variáveis, automaticamente.
#
# Dois modos:
#   (A) Rico   : construir_databook(df_codificado, dic)  -> usa o JSON
#                (agrupa por eixo, traduz código -> rótulo, usa unidades).
#   (B) Simples: construir_databook(df)                  -> sem JSON
#                (infere o tipo direto dos dados; "de forma mais simplória").
#
# O renderizador databook_html() produz um .html autocontido (CSS embutido).
# Escrito em R base; só precisa de jsonlite para LER o dicionário.
#
# Uso:
#   source("R/03_gera_databook.R")   # define as funções (não executa nada)
#   Rscript R/03_gera_databook.R     # executa o bloco final e gera o HTML
# =============================================================================

# ---- Utilitários ------------------------------------------------------------
escapar_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

fmt_num <- function(x, dec = 2) {
  formatC(x, format = "f", digits = dec,
          big.mark = ".", decimal.mark = ",")
}

fmt_pct <- function(x) {
  paste0(formatC(x, format = "f", digits = 1, decimal.mark = ","), "%")
}

# ---- Resumos por tipo -------------------------------------------------------
resumo_continua <- function(x, dec = 2) {
  n_total <- length(x)
  n_na    <- sum(is.na(x))
  v       <- x[!is.na(x)]
  stats <- data.frame(
    Medida = c("N válido", "Missing", "Média", "Desvio-padrão",
               "Mínimo", "P25", "Mediana", "P75", "Máximo"),
    Valor  = c(
      as.character(length(v)),
      paste0(n_na, " (", fmt_pct(100 * n_na / n_total), ")"),
      fmt_num(mean(v), dec),
      fmt_num(stats::sd(v), dec),
      fmt_num(min(v), dec),
      fmt_num(stats::quantile(v, 0.25), dec),
      fmt_num(stats::median(v), dec),
      fmt_num(stats::quantile(v, 0.75), dec),
      fmt_num(max(v), dec)
    ),
    stringsAsFactors = FALSE
  )
  list(tipo = "continua", stats = stats, n_na = n_na, n_total = n_total)
}

resumo_categorica <- function(x, valores = NULL) {
  n_total <- length(x)
  n_na    <- sum(is.na(x))
  n_valid <- n_total - n_na
  
  if (!is.null(valores)) {
    cods <- vapply(valores, function(z) as.character(z$codigo), character(1))
    labs <- vapply(valores, function(z) as.character(z$rotulo),  character(1))
  } else {
    cods <- sort(unique(as.character(x[!is.na(x)])))
    labs <- cods
  }
  
  tab <- table(as.character(x))           # NA já fica de fora
  n   <- as.integer(tab[cods])
  n[is.na(n)] <- 0L
  pct <- if (n_valid > 0) 100 * n / n_valid else rep(0, length(n))
  
  freq <- data.frame(
    Codigo = cods,
    Rotulo = labs,
    n      = n,
    pct    = vapply(pct, fmt_pct, character(1)),
    stringsAsFactors = FALSE
  )
  list(tipo = "categorica", freq = freq, n_na = n_na, n_total = n_total)
}

resumo_identificador <- function(x) {
  list(tipo = "identificador",
       n_distinct = length(unique(x[!is.na(x)])),
       n_na = sum(is.na(x)), n_total = length(x))
}

# Dispatcher: decide o resumo a partir do meta (JSON) ou por inferência.
resumir_variavel <- function(x, meta = NULL) {
  if (!is.null(meta) && !is.null(meta$tipo)) {
    if (meta$tipo == "continua") {
      dec <- if (!is.null(meta$decimais)) meta$decimais else 2
      return(resumo_continua(x, dec))
    } else if (meta$tipo == "categorica") {
      return(resumo_categorica(x, meta$valores))
    } else {
      return(resumo_identificador(x))
    }
  }
  # ---- modo simplório: inferir o tipo ----
  n_distinct <- length(unique(x[!is.na(x)]))
  if (is.numeric(x) && n_distinct == length(x)) {
    resumo_identificador(x)
  } else if (is.numeric(x) && n_distinct > 12) {
    resumo_continua(x, 2)
  } else {
    resumo_categorica(x, NULL)
  }
}

# ---- Montagem da estrutura do databook --------------------------------------
construir_databook <- function(df, dic = NULL) {
  secoes <- list()
  
  if (!is.null(dic)) {
    eixos <- dic$eixos
    for (e in names(eixos)) {
      vs <- Filter(function(v) identical(v$eixo, e), dic$variaveis)
      if (length(vs) == 0) next
      conteudo <- lapply(vs, function(v) {
        list(meta = v, resumo = resumir_variavel(df[[v$nome]], v))
      })
      secoes[[length(secoes) + 1]] <-
        list(titulo = eixos[[e]], chave = e, vars = conteudo)
    }
    meta_geral <- list(dataset = dic$dataset, n = nrow(df),
                       seed = dic$seed, descricao = dic$descricao)
  } else {
    conteudo <- lapply(names(df), function(nm) {
      r <- resumir_variavel(df[[nm]], NULL)
      list(meta = list(nome = nm, rotulo = nm, tipo = r$tipo), resumo = r)
    })
    secoes[[1]] <- list(titulo = "Variáveis", chave = "todas",
                        vars = conteudo)
    meta_geral <- list(dataset = "df", n = nrow(df), seed = NA,
                       descricao = "Databook gerado direto do data.frame.")
  }
  list(meta = meta_geral, secoes = secoes)
}

# ---- Renderização HTML (CSS embutido) ---------------------------------------
.css_databook <- "
:root{
  --tinta:#1f2933; --suave:#52606d; --linha:#e4e7eb; --fundo:#ffffff;
  --acento:#2b6cb0; --acento2:#2f855a; --acento3:#975a16;
  --zebra:#f7f9fb;
}
*{box-sizing:border-box}
body{margin:0;background:#eef1f4;color:var(--tinta);
  font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,
  Arial,sans-serif;line-height:1.5}
.wrap{max-width:920px;margin:0 auto;padding:32px 20px 80px}
header.db{background:var(--fundo);border:1px solid var(--linha);
  border-radius:12px;padding:26px 28px;margin-bottom:22px}
header.db h1{margin:0 0 6px;font-size:1.5rem}
header.db .sub{color:var(--suave);font-size:.92rem}
.meta-grid{display:flex;flex-wrap:wrap;gap:18px;margin-top:16px;
  font-size:.85rem;color:var(--suave)}
.meta-grid b{color:var(--tinta)}
.legenda{margin-top:14px;padding:10px 14px;background:var(--zebra);
  border-left:3px solid var(--acento);border-radius:6px;
  font-size:.82rem;color:var(--suave)}
nav.toc{background:var(--fundo);border:1px solid var(--linha);
  border-radius:12px;padding:18px 22px;margin-bottom:22px}
nav.toc h2{margin:0 0 10px;font-size:.8rem;letter-spacing:.06em;
  text-transform:uppercase;color:var(--suave)}
nav.toc a{display:inline-block;margin:3px 14px 3px 0;
  color:var(--acento);text-decoration:none;font-size:.9rem}
nav.toc a:hover{text-decoration:underline}
section.eixo{margin-bottom:30px}
section.eixo > h2{font-size:1.15rem;margin:0 0 4px;
  padding-bottom:6px;border-bottom:2px solid var(--linha)}
.var{background:var(--fundo);border:1px solid var(--linha);
  border-radius:10px;padding:16px 18px;margin-top:14px}
.varhead{display:flex;align-items:center;gap:10px;flex-wrap:wrap}
.vname{font-family:'SFMono-Regular',Consolas,Menlo,monospace;
  font-size:.95rem;font-weight:600}
.vlabel{color:var(--suave);font-size:.9rem;margin:2px 0 10px}
.vunit{color:var(--suave);font-size:.8rem;font-style:italic}
.badge{font-size:.7rem;font-weight:600;padding:2px 8px;border-radius:20px;
  letter-spacing:.03em}
.b-continua{background:#e6f0fa;color:var(--acento)}
.b-categorica{background:#e6f4ec;color:var(--acento2)}
.b-identificador{background:#faf0e0;color:var(--acento3)}
.b-miss{background:#fdecec;color:#9b2c2c;margin-left:auto}
table{border-collapse:collapse;width:100%;font-size:.86rem;margin-top:6px}
th,td{padding:6px 10px;border-bottom:1px solid var(--linha);text-align:left}
thead th{font-size:.72rem;text-transform:uppercase;letter-spacing:.04em;
  color:var(--suave);border-bottom:1.5px solid var(--linha)}
tbody tr:nth-child(even){background:var(--zebra)}
td.cod{font-family:'SFMono-Regular',Consolas,Menlo,monospace;
  color:var(--acento)}
td.num,th.num{text-align:right;
  font-variant-numeric:tabular-nums}
footer{color:var(--suave);font-size:.78rem;text-align:center;margin-top:30px}
"

.tabela_continua_html <- function(stats) {
  linhas <- vapply(seq_len(nrow(stats)), function(i) {
    paste0("<tr><td>", escapar_html(stats$Medida[i]),
           "</td><td class='num'>", escapar_html(stats$Valor[i]),
           "</td></tr>")
  }, character(1))
  paste0("<table><thead><tr><th>Medida</th><th class='num'>Valor</th>",
         "</tr></thead><tbody>", paste(linhas, collapse = ""),
         "</tbody></table>")
}

.tabela_freq_html <- function(freq) {
  linhas <- vapply(seq_len(nrow(freq)), function(i) {
    paste0("<tr><td class='cod'>", escapar_html(freq$Codigo[i]),
           "</td><td>", escapar_html(freq$Rotulo[i]),
           "</td><td class='num'>", freq$n[i],
           "</td><td class='num'>", freq$pct[i], "</td></tr>")
  }, character(1))
  paste0("<table><thead><tr><th>Código</th><th>Rótulo</th>",
         "<th class='num'>n</th><th class='num'>%</th></tr></thead><tbody>",
         paste(linhas, collapse = ""), "</tbody></table>")
}

.bloco_variavel_html <- function(item) {
  meta <- item$meta; r <- item$resumo
  ancora <- paste0("v-", meta$nome)
  badge_tipo <- paste0("<span class='badge b-", r$tipo, "'>",
                       r$tipo, "</span>")
  n_na <- if (!is.null(r$n_na)) r$n_na else 0
  n_tot <- if (!is.null(r$n_total)) r$n_total else 1
  badge_miss <- paste0("<span class='badge b-miss'>missing: ", n_na,
                       " (", fmt_pct(100 * n_na / n_tot), ")</span>")
  
  unidade <- ""
  if (!is.null(meta$unidade)) {
    unidade <- paste0("<div class='vunit'>Unidade: ",
                      escapar_html(meta$unidade), "</div>")
  }
  
  corpo <- if (r$tipo == "continua") {
    .tabela_continua_html(r$stats)
  } else if (r$tipo == "categorica") {
    .tabela_freq_html(r$freq)
  } else {
    paste0("<table><tbody><tr><td>Valores distintos</td>",
           "<td class='num'>", r$n_distinct, "</td></tr></tbody></table>")
  }
  
  rotulo <- if (!is.null(meta$rotulo)) meta$rotulo else meta$nome
  paste0(
    "<div class='var' id='", ancora, "'>",
    "<div class='varhead'><span class='vname'>", escapar_html(meta$nome),
    "</span>", badge_tipo, badge_miss, "</div>",
    "<div class='vlabel'>", escapar_html(rotulo), "</div>",
    unidade, corpo, "</div>"
  )
}

databook_html <- function(db, arquivo = "output/databook.html",
                          titulo = "Databook") {
  m <- db$meta
  
  # TOC
  toc_links <- vapply(db$secoes, function(s) {
    paste0("<a href='#sec-", s$chave, "'>", escapar_html(s$titulo), "</a>")
  }, character(1))
  toc <- paste0("<nav class='toc'><h2>Conteúdo</h2>",
                paste(toc_links, collapse = ""), "</nav>")
  
  # Seções
  secoes_html <- vapply(db$secoes, function(s) {
    vars <- paste(vapply(s$vars, .bloco_variavel_html, character(1)),
                  collapse = "")
    paste0("<section class='eixo' id='sec-", s$chave, "'><h2>",
           escapar_html(s$titulo), "</h2>", vars, "</section>")
  }, character(1))
  
  n_vars <- sum(vapply(db$secoes, function(s) length(s$vars), integer(1)))
  seed_txt <- if (is.na(m$seed)) "—" else m$seed
  
  cabecalho <- paste0(
    "<header class='db'><h1>", escapar_html(titulo), "</h1>",
    "<div class='sub'>", escapar_html(m$descricao), "</div>",
    "<div class='meta-grid'>",
    "<div>Dataset: <b>", escapar_html(m$dataset), "</b></div>",
    "<div>Observações: <b>", m$n, "</b></div>",
    "<div>Variáveis: <b>", n_vars, "</b></div>",
    "<div>Seed: <b>", seed_txt, "</b></div>",
    "<div>Gerado em: <b>", format(Sys.Date(), "%d/%m/%Y"), "</b></div>",
    "</div>",
    "<div class='legenda'>As porcentagens das variáveis categóricas são ",
    "calculadas sobre os casos válidos (excluindo missings). O total de ",
    "missing aparece no selo de cada variável.</div></header>"
  )
  
  html <- paste0(
    "<!DOCTYPE html><html lang='pt-BR'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", escapar_html(titulo), "</title><style>", .css_databook,
    "</style></head><body><div class='wrap'>",
    cabecalho, toc, paste(secoes_html, collapse = ""),
    "<footer>Databook reprodutível — gerado por 03_gera_databook.R</footer>",
    "</div></body></html>"
  )
  
  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  con <- file(arquivo, open = "w", encoding = "UTF-8")
  writeLines(html, con)
  close(con)
  invisible(arquivo)
}

# ---- Execução (só quando rodado via Rscript, não via source) ----------------
if (sys.nframe() == 0) {
  suppressPackageStartupMessages(library(jsonlite))
  
  # (A) MODO RICO — usa o JSON e a base codificada
  dic <- fromJSON("dicionario/dicionario.json", simplifyVector = FALSE)
  df_cod <- readRDS("data/simulado/coorte_codificada.rds")
  db <- construir_databook(df_cod, dic)
  databook_html(db, "output/databook.html",
                titulo = "Databook — Coorte simulada (SLZ / PEL / RP)")
  cat("✓ output/databook.html\n")
  
  # (B) MODO SIMPLÓRIO — direto no df rotulado, sem JSON
  df_rot <- readRDS("data/simulado/coorte_rotulada.rds")
  db2 <- construir_databook(df_rot)            # sem dic
  databook_html(db2, "output/databook_simples.html",
                titulo = "Databook (modo simples, sem dicionário)")
  cat("✓ output/databook_simples.html\n")
}
