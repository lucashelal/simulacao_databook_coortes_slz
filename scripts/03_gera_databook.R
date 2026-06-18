# =============================================================================
# 03_gera_databook.R  (v2 — com mini-gráficos SVG e tabelas cruzadas)
# Gera um databook organizado de TODAS as variáveis, automaticamente.
#
# Dois modos:
#   (A) Rico   : construir_databook(df_codificado, dic)  -> usa o JSON
#                (agrupa por eixo, traduz código -> rótulo, usa unidades).
#   (B) Simples: construir_databook(df)                  -> sem JSON
#                (infere o tipo direto dos dados; "de forma mais simplória").
#
# NOVIDADES (v2), todas em R base + SVG inline (sem ggplot, sem PNG):
#   - Contínuas : mini-histograma (escala de densidade) + curva KDE + quartis.
#   - Categóricas: barra de proporção inline na tabela de frequência.
#   - Datas     : resumo (mín/mediana/máx/amplitude) + mini-timeline por ano.
#   - Tabelas cruzadas (argumento `cruzamentos` ou campo dic$cruzamentos):
#       * cat x cat  -> contingência sombreada (n + % linha) + χ² + V de Cramér
#       * cont x cat -> resumo por grupo + mini boxplots
#       * cont x cont-> correlação (Pearson/Spearman) + mini dispersão
#
# (v3) CLASSIFICADOR AUTOMÁTICO — roda um df que JÁ está no ambiente, inteiro
#      e de uma vez, sem listar variáveis: databook_auto(df). Classifica
#      automaticamente (colunas com rótulos/labelled -> categórica; numérica
#      c/ muitos níveis -> contínua; Date/dttm -> data); robusto a colunas NA.
#      Interruptor `graficos`: databook_auto(df, graficos = FALSE) gera o
#      databook SOMENTE COM TABELAS (sem histogramas, densidade, barras,
#      boxplots ou dispersão). Útil para o modo simples do banco em xlsx.
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

# NEW: paleta (espelha as variáveis CSS; usada nos SVG, onde var(--x) é frágil).
.pal <- list(
  acento = "#2b6cb0", acento2 = "#2f855a", acento3 = "#975a16",
  linha  = "#e4e7eb", suave   = "#52606d", tinta   = "#1f2933",
  zebra  = "#f7f9fb", dens    = "#2b6cb0", barra   = "#9fc3e8"
)

# ---- Resumos por tipo -------------------------------------------------------
resumo_continua <- function(x, dec = 2) {
  n_total <- length(x)
  n_na    <- sum(is.na(x))
  v       <- suppressWarnings(as.numeric(x[!is.na(x)]))  # NEW: garante numérico
  v       <- v[is.finite(v)]
  if (length(v) == 0) {                                  # NEW: guarda all-NA
    stats <- data.frame(
      Medida = c("N válido", "Missing"),
      Valor  = c("0", paste0(n_na, " (", fmt_pct(100 * n_na / n_total), ")")),
      stringsAsFactors = FALSE)
    return(list(tipo = "continua", stats = stats, valores = numeric(0),
                n_na = n_na, n_total = n_total))
  }
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
  # NEW: guardamos os valores limpos para desenhar o mini-gráfico depois.
  list(tipo = "continua", stats = stats, valores = v,
       n_na = n_na, n_total = n_total)
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
    prop   = pct / 100,                    # NEW: proporção numérica p/ a barra
    stringsAsFactors = FALSE
  )
  list(tipo = "categorica", freq = freq, n_na = n_na, n_total = n_total)
}

resumo_identificador <- function(x) {
  list(tipo = "identificador",
       n_distinct = length(unique(x[!is.na(x)])),
       n_na = sum(is.na(x)), n_total = length(x))
}

# NEW: resumo para colunas de data (Date / POSIXct) — robusto a all-NA
resumo_data <- function(x) {
  n_total <- length(x)
  n_na    <- sum(is.na(x))
  v       <- x[!is.na(x)]
  if (length(v) == 0) {
    stats <- data.frame(
      Medida = c("N válido", "Missing"),
      Valor  = c("0", paste0(n_na, " (", fmt_pct(100 * n_na / n_total), ")")),
      stringsAsFactors = FALSE)
    return(list(tipo = "data", stats = stats,
                datas = as.Date(character(0)), n_na = n_na, n_total = n_total))
  }
  v  <- as.Date(v)
  fd <- function(d) format(d, "%d/%m/%Y")
  stats <- data.frame(
    Medida = c("N válido", "Missing", "Mínimo", "Mediana", "Máximo",
               "Amplitude (dias)", "Valores distintos"),
    Valor  = c(
      as.character(length(v)),
      paste0(n_na, " (", fmt_pct(100 * n_na / n_total), ")"),
      fd(min(v)), fd(stats::median(v)), fd(max(v)),
      as.character(as.integer(max(v) - min(v))),
      as.character(length(unique(v)))
    ),
    stringsAsFactors = FALSE)
  list(tipo = "data", stats = stats, datas = v, n_na = n_na, n_total = n_total)
}

# Dispatcher: decide o resumo a partir do meta (JSON) ou por inferência.
resumir_variavel <- function(x, meta = NULL) {
  if (!is.null(meta) && !is.null(meta$tipo)) {
    if (meta$tipo == "continua") {
      dec <- if (!is.null(meta$decimais)) meta$decimais else 2
      return(resumo_continua(x, dec))
    } else if (meta$tipo == "categorica") {
      return(resumo_categorica(x, meta$valores))
    } else if (meta$tipo == "data") {                    # NEW
      return(resumo_data(x))
    } else {
      return(resumo_identificador(x))
    }
  }
  # ---- modo simplório: inferir o tipo ----
  if (inherits(x, c("Date", "POSIXct", "POSIXt")))        # NEW: datas
    return(resumo_data(x))
  n_distinct <- length(unique(x[!is.na(x)]))
  if (is.numeric(x) && n_distinct == length(x)) {
    resumo_identificador(x)
  } else if (is.numeric(x) && n_distinct > 12) {
    resumo_continua(x, 2)
  } else {
    resumo_categorica(x, NULL)
  }
}

# =============================================================================
# NEW: GERADORES DE SVG (R base; sem dependências)
# =============================================================================
.svg_abrir <- function(w, h) {
  paste0("<svg class='spark' viewBox='0 0 ", w, " ", h, "' width='", w,
         "' height='", h, "' xmlns='http://www.w3.org/2000/svg' ",
         "preserveAspectRatio='xMidYMid meet' role='img'>")
}

# Mini-histograma (densidade) + curva KDE + marcas de quartis -----------------
svg_hist_densidade <- function(v, w = 340, h = 120) {
  v <- v[is.finite(v)]
  if (length(v) < 3 || diff(range(v)) == 0) return("")  # nada a plotar

  pl <- 8; pr <- 8; pt <- 8; pb <- 22          # margens internas
  pw <- w - pl - pr; ph <- h - pt - pb

  nb <- max(8, min(28, ceiling(sqrt(length(v)))))
  brk <- seq(min(v), max(v), length.out = nb + 1)
  hh  <- hist(v, breaks = brk, plot = FALSE)
  dd  <- tryCatch(stats::density(v), error = function(e) NULL)

  xr <- range(c(hh$breaks, if (!is.null(dd)) dd$x))
  ymax <- max(c(hh$density, if (!is.null(dd)) dd$y), na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) return("")

  sx <- function(x) pl + (x - xr[1]) / (xr[2] - xr[1]) * pw
  sy <- function(y) pt + ph - (y / ymax) * ph

  # Barras
  barras <- vapply(seq_along(hh$density), function(i) {
    x0 <- sx(hh$breaks[i]); x1 <- sx(hh$breaks[i + 1])
    yt <- sy(hh$density[i])
    paste0("<rect x='", fmt6(x0), "' y='", fmt6(yt),
           "' width='", fmt6(max(0.4, x1 - x0 - 0.6)),
           "' height='", fmt6(pt + ph - yt),
           "' fill='", .pal$barra, "' />")
  }, character(1))

  # Curva de densidade
  curva <- ""
  if (!is.null(dd)) {
    keep <- dd$x >= xr[1] & dd$x <= xr[2]
    pts  <- paste0(fmt6(sx(dd$x[keep])), ",", fmt6(sy(dd$y[keep])),
                   collapse = " ")
    curva <- paste0("<polyline points='", pts, "' fill='none' stroke='",
                    .pal$dens, "' stroke-width='1.6' />")
  }

  # Eixo de base + marcas de quartis (P25, mediana, P75)
  base_y <- pt + ph
  eixo <- paste0("<line x1='", pl, "' y1='", base_y, "' x2='", pl + pw,
                 "' y2='", base_y, "' stroke='", .pal$linha,
                 "' stroke-width='1' />")
  qs <- stats::quantile(v, c(.25, .5, .75))
  ticks <- vapply(seq_along(qs), function(i) {
    xx <- sx(qs[i])
    cor <- if (i == 2) .pal$acento3 else .pal$suave
    paste0("<line x1='", fmt6(xx), "' y1='", pt, "' x2='", fmt6(xx),
           "' y2='", base_y, "' stroke='", cor,
           "' stroke-width='1' stroke-dasharray='2,2' opacity='.7' />")
  }, character(1))

  # Rótulos min / mediana / máx
  rot <- paste0(
    "<text x='", pl, "' y='", h - 6, "' class='axl' text-anchor='start'>",
    fmt_num(min(v)), "</text>",
    "<text x='", fmt6(sx(qs[2])), "' y='", h - 6,
    "' class='axl axl-md' text-anchor='middle'>", fmt_num(qs[2]), "</text>",
    "<text x='", pl + pw, "' y='", h - 6,
    "' class='axl' text-anchor='end'>", fmt_num(max(v)), "</text>"
  )

  paste0(.svg_abrir(w, h), paste(barras, collapse = ""), curva, eixo,
         paste(ticks, collapse = ""), rot, "</svg>")
}

# Mini-dispersão (cont x cont) ------------------------------------------------
svg_dispersao <- function(x, y, w = 320, h = 150) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3 || diff(range(x)) == 0 || diff(range(y)) == 0) return("")
  if (length(x) > 400) {                      # amostra p/ não pesar o HTML
    idx <- sample.int(length(x), 400); x <- x[idx]; y <- y[idx]
  }
  pl <- 10; pr <- 8; pt <- 8; pb <- 18
  pw <- w - pl - pr; ph <- h - pt - pb
  xr <- range(x); yr <- range(y)
  sx <- function(v) pl + (v - xr[1]) / (xr[2] - xr[1]) * pw
  sy <- function(v) pt + ph - (v - yr[1]) / (yr[2] - yr[1]) * ph
  pts <- paste0(vapply(seq_along(x), function(i)
    paste0("<circle cx='", fmt6(sx(x[i])), "' cy='", fmt6(sy(y[i])),
           "' r='1.8' fill='", .pal$acento, "' opacity='.45' />"),
    character(1)), collapse = "")
  eixos <- paste0(
    "<line x1='", pl, "' y1='", pt + ph, "' x2='", pl + pw, "' y2='",
    pt + ph, "' stroke='", .pal$linha, "' stroke-width='1'/>",
    "<line x1='", pl, "' y1='", pt, "' x2='", pl, "' y2='", pt + ph,
    "' stroke='", .pal$linha, "' stroke-width='1'/>")
  paste0(.svg_abrir(w, h), eixos, pts, "</svg>")
}

# Mini-boxplots por grupo (cont x cat) — escala x compartilhada ---------------
svg_boxplots <- function(grupos, w = 360) {
  vals <- lapply(grupos, function(g) g$v)
  todos <- unlist(vals)
  todos <- todos[is.finite(todos)]
  if (length(todos) < 3 || diff(range(todos)) == 0) return("")
  k  <- length(grupos)
  lh <- 24                                     # altura por grupo
  pl <- 90; pr <- 12; pt <- 6; pb <- 16
  h  <- pt + pb + k * lh
  pw <- w - pl - pr
  xr <- range(todos)
  sx <- function(v) pl + (v - xr[1]) / (xr[2] - xr[1]) * pw

  linhas <- vapply(seq_len(k), function(i) {
    g  <- grupos[[i]]; v <- g$v[is.finite(g$v)]
    yc <- pt + (i - 1) * lh + lh / 2
    if (length(v) < 1) return("")
    q  <- stats::quantile(v, c(.25, .5, .75))
    iqr <- q[3] - q[1]
    lo <- max(min(v), q[1] - 1.5 * iqr); hi <- min(max(v), q[3] + 1.5 * iqr)
    bh <- 9
    paste0(
      "<text x='", pl - 8, "' y='", yc + 3, "' class='axl' ",
      "text-anchor='end'>", escapar_html(g$rotulo), "</text>",
      # whiskers
      "<line x1='", fmt6(sx(lo)), "' y1='", yc, "' x2='", fmt6(sx(hi)),
      "' y2='", yc, "' stroke='", .pal$suave, "' stroke-width='1'/>",
      # caixa
      "<rect x='", fmt6(sx(q[1])), "' y='", yc - bh, "' width='",
      fmt6(max(0.5, sx(q[3]) - sx(q[1]))), "' height='", 2 * bh,
      "' fill='", .pal$barra, "' stroke='", .pal$acento,
      "' stroke-width='1'/>",
      # mediana
      "<line x1='", fmt6(sx(q[2])), "' y1='", yc - bh, "' x2='",
      fmt6(sx(q[2])), "' y2='", yc + bh, "' stroke='", .pal$acento3,
      "' stroke-width='1.6'/>")
  }, character(1))

  eixo <- paste0(
    "<line x1='", pl, "' y1='", h - pb + 2, "' x2='", pl + pw, "' y2='",
    h - pb + 2, "' stroke='", .pal$linha, "' stroke-width='1'/>",
    "<text x='", pl, "' y='", h - 3, "' class='axl' text-anchor='start'>",
    fmt_num(xr[1]), "</text>",
    "<text x='", pl + pw, "' y='", h - 3,
    "' class='axl' text-anchor='end'>", fmt_num(xr[2]), "</text>")

  paste0(.svg_abrir(w, h), paste(linhas, collapse = ""), eixo, "</svg>")
}

# Mini-histograma de datas por ano (cont. de datas) --------------------------
svg_hist_datas <- function(datas, w = 340, h = 110) {
  d <- as.Date(datas[!is.na(datas)])
  if (length(d) < 3) return("")
  anos <- as.integer(format(d, "%Y"))
  rng  <- range(anos)
  if (diff(rng) == 0) return("")
  pl <- 8; pr <- 8; pt <- 8; pb <- 18; pw <- w - pl - pr; ph <- h - pt - pb
  brk <- seq(rng[1] - 0.5, rng[2] + 0.5, by = 1)
  hh  <- hist(anos, breaks = brk, plot = FALSE)
  ymax <- max(hh$counts); if (ymax <= 0) return("")
  x0r <- rng[1] - 0.5; x1r <- rng[2] + 0.5
  sx <- function(x) pl + (x - x0r) / (x1r - x0r) * pw
  sy <- function(y) pt + ph - (y / ymax) * ph
  barras <- vapply(seq_along(hh$counts), function(i) {
    xa <- sx(brk[i]); xb <- sx(brk[i + 1])
    paste0("<rect x='", fmt6(xa), "' y='", fmt6(sy(hh$counts[i])),
           "' width='", fmt6(max(0.5, xb - xa - 0.6)),
           "' height='", fmt6(pt + ph - sy(hh$counts[i])),
           "' fill='", .pal$barra, "' />")
  }, character(1))
  by <- pt + ph
  eixo <- paste0("<line x1='", pl, "' y1='", by, "' x2='", pl + pw, "' y2='",
                 by, "' stroke='", .pal$linha, "' stroke-width='1'/>")
  rot <- paste0(
    "<text x='", pl, "' y='", h - 5, "' class='axl' text-anchor='start'>",
    rng[1], "</text>",
    "<text x='", pl + pw, "' y='", h - 5,
    "' class='axl' text-anchor='end'>", rng[2], "</text>")
  paste0(.svg_abrir(w, h), paste(barras, collapse = ""), eixo, rot, "</svg>")
}

# formata coordenada SVG (poucas casas, ponto decimal)
fmt6 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ".")

# =============================================================================
# NEW: TABELAS CRUZADAS
# =============================================================================
.meta_por_nome <- function(dic, nome) {
  if (is.null(dic)) return(NULL)
  for (v in dic$variaveis) if (identical(v$nome, nome)) return(v)
  NULL
}

.tipo_de <- function(x, meta) {
  if (!is.null(meta) && !is.null(meta$tipo)) return(meta$tipo)
  nd <- length(unique(x[!is.na(x)]))
  if (is.numeric(x) && nd == length(x)) "identificador"
  else if (is.numeric(x) && nd > 12)    "continua"
  else                                   "categorica"
}

.niveis <- function(x, meta) {
  if (!is.null(meta) && !is.null(meta$valores)) {
    cods <- vapply(meta$valores, function(z) as.character(z$codigo), character(1))
    labs <- vapply(meta$valores, function(z) as.character(z$rotulo),  character(1))
  } else {
    cods <- sort(unique(as.character(x[!is.na(x)]))); labs <- cods
  }
  list(cods = cods, labs = labs)
}

# Monta o objeto de um cruzamento (decide o tipo conforme os dados/JSON).
montar_cruzamento <- function(df, nx, ny, dic = NULL) {
  if (is.null(df[[nx]]) || is.null(df[[ny]])) return(NULL)
  mx <- .meta_por_nome(dic, nx); my <- .meta_por_nome(dic, ny)
  tx <- .tipo_de(df[[nx]], mx);  ty <- .tipo_de(df[[ny]], my)
  rot <- function(m, nm) if (!is.null(m) && !is.null(m$rotulo)) m$rotulo else nm

  base <- list(nx = nx, ny = ny,
               rx = rot(mx, nx), ry = rot(my, ny))

  if (tx == "categorica" && ty == "categorica") {
    cx <- df[[nx]]; cy <- df[[ny]]
    nvx <- .niveis(cx, mx); nvy <- .niveis(cy, my)
    fx <- factor(as.character(cx), levels = nvx$cods)
    fy <- factor(as.character(cy), levels = nvy$cods)
    m  <- table(fx, fy)
    # remove níveis nunca observados (linhas/colunas zeradas)
    keepr <- rowSums(m) > 0; keepc <- colSums(m) > 0
    m <- m[keepr, keepc, drop = FALSE]
    labx <- nvx$labs[keepr]; laby <- nvy$labs[keepc]
    N <- sum(m)
    rowpct <- sweep(m, 1, ifelse(rowSums(m) == 0, 1, rowSums(m)), "/") * 100
    test <- tryCatch(suppressWarnings(stats::chisq.test(m)),
                     error = function(e) NULL)
    cram <- if (!is.null(test) && N > 0 && min(dim(m)) > 1)
      sqrt(as.numeric(test$statistic) / (N * (min(dim(m)) - 1))) else NA
    return(c(base, list(tipo = "cat_cat", m = m, rowpct = rowpct,
                        labx = labx, laby = laby, N = N,
                        chi = test, cramer = cram)))
  }

  if (xor(tx == "continua", ty == "continua") &&
      (tx %in% c("continua","categorica")) &&
      (ty %in% c("continua","categorica"))) {
    # cont x cat: define qual é contínua e qual é grupo
    if (tx == "continua") { vc <- df[[nx]]; gc <- df[[ny]]; mg <- my
      rcont <- base$rx; rgrp <- base$ry
    } else { vc <- df[[ny]]; gc <- df[[nx]]; mg <- mx
      rcont <- base$ry; rgrp <- base$rx }
    vc <- suppressWarnings(as.numeric(vc))
    nv <- .niveis(gc, mg)
    grupos <- list(); resumo <- list()
    for (i in seq_along(nv$cods)) {
      sel <- as.character(gc) == nv$cods[i] & !is.na(gc) & is.finite(vc)
      v <- vc[sel]
      if (length(v) == 0) next
      grupos[[length(grupos) + 1]] <- list(rotulo = nv$labs[i], v = v)
      resumo[[length(resumo) + 1]] <- data.frame(
        Grupo = nv$labs[i], n = length(v),
        Media = fmt_num(mean(v)), DP = fmt_num(stats::sd(v)),
        Mediana = fmt_num(stats::median(v)),
        P25 = fmt_num(stats::quantile(v, .25)),
        P75 = fmt_num(stats::quantile(v, .75)),
        stringsAsFactors = FALSE)
    }
    if (length(grupos) == 0) return(NULL)
    return(c(base, list(tipo = "cont_cat", grupos = grupos,
                        resumo = do.call(rbind, resumo),
                        rcont = rcont, rgrp = rgrp)))
  }

  if (tx == "continua" && ty == "continua") {
    vx <- suppressWarnings(as.numeric(df[[nx]]))
    vy <- suppressWarnings(as.numeric(df[[ny]]))
    ok <- is.finite(vx) & is.finite(vy)
    if (sum(ok) < 3) return(NULL)
    pe <- suppressWarnings(stats::cor(vx[ok], vy[ok], method = "pearson"))
    sp <- suppressWarnings(stats::cor(vx[ok], vy[ok], method = "spearman"))
    return(c(base, list(tipo = "cont_cont", vx = vx[ok], vy = vy[ok],
                        n = sum(ok), pearson = pe, spearman = sp)))
  }
  NULL  # identificadores ou combinações não suportadas
}

# Render de um cruzamento -----------------------------------------------------
.cruzamento_html <- function(cz, graficos = TRUE) {
  if (is.null(cz)) return("")
  cab <- paste0(
    "<div class='xhead'><span class='vname'>", escapar_html(cz$nx),
    "</span> &times; <span class='vname'>", escapar_html(cz$ny), "</span></div>",
    "<div class='vlabel'>", escapar_html(cz$rx), " &times; ",
    escapar_html(cz$ry), "</div>")

  if (cz$tipo == "cat_cat") {
    m <- cz$m; rp <- cz$rowpct
    th <- paste0("<th></th>",
                 paste0("<th class='num'>", escapar_html(cz$laby), "</th>",
                        collapse = ""), "<th class='num'>Total</th>")
    linhas <- vapply(seq_len(nrow(m)), function(i) {
      cels <- vapply(seq_len(ncol(m)), function(j) {
        a <- min(0.85, rp[i, j] / 100 * 0.85)
        paste0("<td class='num xcell' style='background:rgba(43,108,176,",
               fmt6(a), ")'>", m[i, j], "<span class='xp'>",
               fmt_pct(rp[i, j]), "</span></td>")
      }, character(1))
      paste0("<tr><td class='xrow'>", escapar_html(cz$labx[i]), "</td>",
             paste(cels, collapse = ""),
             "<td class='num'><b>", sum(m[i, ]), "</b></td></tr>")
    }, character(1))
    tot <- paste0("<tr><td class='xrow'><b>Total</b></td>",
                  paste0("<td class='num'><b>", colSums(m), "</b></td>",
                         collapse = ""),
                  "<td class='num'><b>", sum(m), "</b></td></tr>")
    tabela <- paste0("<table class='xtab'><thead><tr>", th,
                     "</tr></thead><tbody>", paste(linhas, collapse = ""),
                     tot, "</tbody></table>")
    stat <- ""
    if (!is.null(cz$chi)) {
      stat <- paste0("<div class='xstat'>&chi;&sup2; = ",
        fmt_num(as.numeric(cz$chi$statistic), 2), " (gl = ",
        cz$chi$parameter, "), p = ",
        formatC(cz$chi$p.value, format = "g", digits = 3),
        if (!is.na(cz$cramer)) paste0(" &nbsp;•&nbsp; V de Cramér = ",
                                      fmt_num(cz$cramer, 3)) else "",
        ". Células: n e % por linha; sombra ∝ % da linha.</div>")
    }
    return(paste0("<div class='xbloco'>", cab, tabela, stat, "</div>"))
  }

  if (cz$tipo == "cont_cat") {
    rs <- cz$resumo
    th <- "<th>Grupo</th><th class='num'>n</th><th class='num'>Média</th><th class='num'>DP</th><th class='num'>Mediana</th><th class='num'>P25</th><th class='num'>P75</th>"
    linhas <- vapply(seq_len(nrow(rs)), function(i)
      paste0("<tr><td>", escapar_html(rs$Grupo[i]),
             "</td><td class='num'>", rs$n[i],
             "</td><td class='num'>", rs$Media[i],
             "</td><td class='num'>", rs$DP[i],
             "</td><td class='num'>", rs$Mediana[i],
             "</td><td class='num'>", rs$P25[i],
             "</td><td class='num'>", rs$P75[i], "</td></tr>"),
      character(1))
    tabela <- paste0("<table><thead><tr>", th, "</tr></thead><tbody>",
                     paste(linhas, collapse = ""), "</tbody></table>")
    box <- if (graficos) svg_boxplots(cz$grupos) else ""
    cont_nm <- escapar_html(cz$rcont); grp_nm <- escapar_html(cz$rgrp)
    return(paste0("<div class='xbloco'>", cab,
                  "<div class='xnote'>", cont_nm, " por ", grp_nm, "</div>",
                  tabela,
                  if (nzchar(box)) paste0("<div class='spark-wrap'>", box,
                                          "</div>") else "",
                  "</div>"))
  }

  if (cz$tipo == "cont_cont") {
    disp <- if (graficos) svg_dispersao(cz$vx, cz$vy) else ""
    stat <- paste0("<div class='xstat'>n = ", cz$n,
      " &nbsp;•&nbsp; Pearson r = ", fmt_num(cz$pearson, 3),
      " &nbsp;•&nbsp; Spearman ρ = ", fmt_num(cz$spearman, 3), "</div>")
    return(paste0("<div class='xbloco'>", cab,
                  if (nzchar(disp)) paste0("<div class='spark-wrap'>", disp,
                                           "</div>") else "",
                  stat, "</div>"))
  }
  ""
}

# ---- Montagem da estrutura do databook --------------------------------------
construir_databook <- function(df, dic = NULL, cruzamentos = NULL) {  # NEW arg
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

  # NEW: cruzamentos vindos do argumento OU do campo dic$cruzamentos
  if (is.null(cruzamentos) && !is.null(dic) && !is.null(dic$cruzamentos))
    cruzamentos <- dic$cruzamentos
  cruz <- list()
  if (!is.null(cruzamentos)) {
    for (par in cruzamentos) {
      par <- as.character(unlist(par))      # aceita c("a","b") ou list("a","b")
      if (length(par) < 2) next
      obj <- montar_cruzamento(df, par[1], par[2], dic)
      if (!is.null(obj)) cruz[[length(cruz) + 1]] <- obj
    }
  }

  list(meta = meta_geral, secoes = secoes, cruzamentos = cruz)  # NEW campo
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
.b-data{background:#eef0f4;color:#3b4453}
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

/* ---- NEW: layout contínua (tabela + mini-gráfico lado a lado) ---- */
.cont-grid{display:flex;gap:18px;align-items:flex-start;flex-wrap:wrap}
.cont-grid > table{flex:1 1 240px;min-width:240px;width:auto}
.spark-wrap{flex:1 1 320px;min-width:280px;display:flex;
  align-items:center;justify-content:center;padding-top:6px}
svg.spark{max-width:100%;height:auto;overflow:visible}
svg.spark .axl{font-size:9px;fill:var(--suave);
  font-family:'SFMono-Regular',Consolas,Menlo,monospace}
svg.spark .axl-md{fill:var(--acento3)}

/* ---- NEW: barra de proporção inline na tabela de frequência ---- */
.bar-cell{width:130px}
.bar-track{position:relative;height:14px;background:var(--zebra);
  border-radius:7px;overflow:hidden}
.bar-fill{position:absolute;top:0;left:0;height:100%;
  background:var(--acento);opacity:.55;border-radius:7px}

/* ---- NEW: seção de tabelas cruzadas ---- */
.xbloco{background:var(--fundo);border:1px solid var(--linha);
  border-radius:10px;padding:16px 18px;margin-top:14px}
.xhead{font-size:.95rem}
.xnote,.xstat{color:var(--suave);font-size:.8rem;margin:6px 0}
.xstat{margin-top:8px;padding:6px 10px;background:var(--zebra);
  border-radius:6px}
table.xtab td.xcell{text-align:right;color:var(--tinta)}
table.xtab .xp{display:block;font-size:.7rem;color:var(--suave);
  font-variant-numeric:tabular-nums}
table.xtab td.xrow{font-weight:600}
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

# Tabela de frequência; barra de proporção opcional (bar = FALSE -> só tabela)
.tabela_freq_html <- function(freq, bar = TRUE) {
  pmax_ <- max(freq$prop, na.rm = TRUE); if (!is.finite(pmax_) || pmax_ <= 0) pmax_ <- 1
  linhas <- vapply(seq_len(nrow(freq)), function(i) {
    cel_bar <- if (bar) {
      larg <- 100 * freq$prop[i] / pmax_     # barra relativa à categoria modal
      paste0("<td class='bar-cell'><div class='bar-track'>",
             "<div class='bar-fill' style='width:", fmt6(larg), "%'></div>",
             "</div></td>")
    } else ""
    paste0("<tr><td class='cod'>", escapar_html(freq$Codigo[i]),
           "</td><td>", escapar_html(freq$Rotulo[i]),
           "</td><td class='num'>", freq$n[i],
           "</td><td class='num'>", freq$pct[i], "</td>", cel_bar, "</tr>")
  }, character(1))
  th_bar <- if (bar) "<th class='bar-cell'>distribuição</th>" else ""
  paste0("<table><thead><tr><th>Código</th><th>Rótulo</th>",
         "<th class='num'>n</th><th class='num'>%</th>", th_bar,
         "</tr></thead><tbody>",
         paste(linhas, collapse = ""), "</tbody></table>")
}

.bloco_variavel_html <- function(item, graficos = TRUE) {
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

  # corpo: tabela (+ mini-gráfico SVG, só quando graficos = TRUE)
  if (r$tipo == "continua") {
    tab  <- .tabela_continua_html(r$stats)
    spk  <- if (graficos) svg_hist_densidade(r$valores) else ""
    corpo <- if (nzchar(spk))
      paste0("<div class='cont-grid'>", tab,
             "<div class='spark-wrap'>", spk, "</div></div>")
    else tab
  } else if (r$tipo == "data") {
    tab <- .tabela_continua_html(r$stats)
    spk <- if (graficos) svg_hist_datas(r$datas) else ""
    corpo <- if (nzchar(spk))
      paste0("<div class='cont-grid'>", tab,
             "<div class='spark-wrap'>", spk, "</div></div>")
    else tab
  } else if (r$tipo == "categorica") {
    corpo <- .tabela_freq_html(r$freq, bar = graficos)
  } else {
    corpo <- paste0("<table><tbody><tr><td>Valores distintos</td>",
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
                          titulo = "Databook", graficos = TRUE) {
  m <- db$meta

  tem_cruz <- !is.null(db$cruzamentos) && length(db$cruzamentos) > 0  # NEW

  # TOC
  toc_links <- vapply(db$secoes, function(s) {
    paste0("<a href='#sec-", s$chave, "'>", escapar_html(s$titulo), "</a>")
  }, character(1))
  if (tem_cruz)                                                        # NEW
    toc_links <- c(toc_links,
                   "<a href='#sec-cruzamentos'>Tabelas cruzadas</a>")
  toc <- paste0("<nav class='toc'><h2>Conteúdo</h2>",
                paste(toc_links, collapse = ""), "</nav>")

  # Seções
  secoes_html <- vapply(db$secoes, function(s) {
    vars <- paste(vapply(s$vars,
                         function(v) .bloco_variavel_html(v, graficos),
                         character(1)),
                  collapse = "")
    paste0("<section class='eixo' id='sec-", s$chave, "'><h2>",
           escapar_html(s$titulo), "</h2>", vars, "</section>")
  }, character(1))

  # NEW: seção de tabelas cruzadas
  cruz_html <- ""
  if (tem_cruz) {
    blocos <- paste(vapply(db$cruzamentos,
                           function(cz) .cruzamento_html(cz, graficos),
                           character(1)),
                    collapse = "")
    cruz_html <- paste0(
      "<section class='eixo' id='sec-cruzamentos'><h2>Tabelas cruzadas</h2>",
      blocos, "</section>")
  }

  n_vars <- sum(vapply(db$secoes, function(s) length(s$vars), integer(1)))
  seed_txt <- if (is.na(m$seed)) "—" else m$seed

  cabecalho <- paste0(
    "<header class='db'><h1>", escapar_html(titulo), "</h1>",
    "<div class='sub'>", escapar_html(m$descricao), "</div>",
    "<div class='meta-grid'>",
    "<div>Dataset: <b>", escapar_html(m$dataset), "</b></div>",
    "<div>Observações: <b>", m$n, "</b></div>",
    "<div>Variáveis: <b>", n_vars, "</b></div>",
    if (tem_cruz) paste0("<div>Cruzamentos: <b>", length(db$cruzamentos),
                         "</b></div>") else "",
    "<div>Seed: <b>", seed_txt, "</b></div>",
    "<div>Gerado em: <b>", format(Sys.Date(), "%d/%m/%Y"), "</b></div>",
    "</div>",
    "<div class='legenda'>As porcentagens das variáveis categóricas são ",
    "calculadas sobre os casos válidos (excluindo missings). O total de ",
    "missing aparece no selo de cada variável. Os mini-gráficos usam ",
    "densidade (contínuas) e proporção por linha (cruzamentos).</div></header>"
  )

  html <- paste0(
    "<!DOCTYPE html><html lang='pt-BR'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", escapar_html(titulo), "</title><style>", .css_databook,
    "</style></head><body><div class='wrap'>",
    cabecalho, toc, paste(secoes_html, collapse = ""), cruz_html,  # NEW
    "<footer>Databook reprodutível — gerado por 03_gera_databook.R</footer>",
    "</div></body></html>"
  )

  dir.create(dirname(arquivo), recursive = TRUE, showWarnings = FALSE)
  con <- file(arquivo, open = "w", encoding = "UTF-8")
  writeLines(html, con)
  close(con)
  invisible(arquivo)
}

# =============================================================================
# NEW: CLASSIFICADOR AUTOMÁTICO — classifica TODAS as variáveis de um df sozinho
# Lê os metadados das colunas: rótulo de variável (attr 'label') e de valor
# (attr 'labels'), quando existirem. Não exige listar nomes nem pacote algum;
# opera sobre o df que já está no ambiente.
# =============================================================================
.codigos_chr <- function(x) as.character(as.vector(unclass(x)))  # tira labels

# Monta {dic, df_base} a partir de um data.frame em memória.
dicionario_automatico <- function(df,
                              agrupar_por     = NULL,   # NULL | "prefixo" | função(nome)
                              limiar_continua = 12,     # numérica c/ >N distintos -> contínua
                              ids             = character(0),
                              continuas       = character(0),
                              categoricas     = character(0)) {
  nomes <- names(df)

  if (is.null(agrupar_por)) {
    eixo_de <- function(nm) "geral"
  } else if (is.character(agrupar_por) && identical(agrupar_por, "prefixo")) {
    eixo_de <- function(nm) sub("[_.].*$", "", nm)       # antes do 1º _ ou .
  } else if (is.function(agrupar_por)) {
    eixo_de <- agrupar_por
  } else {
    eixo_de <- function(nm) "geral"
  }

  variaveis    <- list()
  base         <- vector("list", length(nomes)); names(base) <- nomes
  eixo_titulos <- list()

  for (nm in nomes) {
    x   <- df[[nm]]
    lab <- attr(x, "label",  exact = TRUE)
    rot <- if (!is.null(lab) && nzchar(lab)) lab else nm
    vl  <- attr(x, "labels", exact = TRUE)

    # ---- decide o tipo (overrides > data > labelled > char > numérico) ----
    if (nm %in% ids) {
      tipo <- "identificador"
    } else if (nm %in% continuas) {
      tipo <- "continua"
    } else if (nm %in% categoricas) {
      tipo <- "categorica"
    } else if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
      tipo <- "data"
    } else if (!is.null(vl)) {
      tipo <- "categorica"
    } else if (is.character(x)) {
      nd <- length(unique(x[!is.na(x)])); n <- length(x)
      tipo <- if (nd > 0.9 * n && nd > 20) "identificador" else "categorica"
    } else if (is.numeric(x)) {
      nd <- length(unique(x[!is.na(x)]))
      tipo <- if (nd > limiar_continua) "continua" else "categorica"
    } else {
      tipo <- "categorica"
    }

    meta <- list(nome = nm, rotulo = rot, eixo = eixo_de(nm), tipo = tipo)

    if (tipo == "continua") {
      col <- suppressWarnings(as.numeric(x))
      vv  <- col[is.finite(col)]
      meta$decimais <- if (length(vv) && all(vv == round(vv))) 0L else 2L
    } else if (tipo == "data") {
      col <- as.Date(x)
    } else if (tipo == "identificador") {
      col <- .codigos_chr(x)
    } else {                                   # categórica
      col <- .codigos_chr(x)
      valores <- list()
      if (!is.null(vl)) {                      # rótulos de valor (labelled)
        for (i in seq_along(vl)) {
          valores[[length(valores) + 1]] <-
            list(codigo = as.character(unname(vl[i])), rotulo = names(vl)[i])
        }
      }
      ja  <- vapply(valores, function(z) z$codigo, character(1))
      obs <- unique(col[!is.na(col)])
      extra <- setdiff(obs, ja)                # códigos observados sem rótulo
      if (length(extra)) {
        on <- suppressWarnings(as.numeric(extra))
        extra <- if (!any(is.na(on))) extra[order(on)] else sort(extra)
        for (cd in extra)
          valores[[length(valores) + 1]] <-
            list(codigo = cd, rotulo = paste0("(sem rótulo: ", cd, ")"))
      }
      meta$valores <- valores
    }

    base[[nm]] <- col
    variaveis[[length(variaveis) + 1]] <- meta
    ek <- meta$eixo
    if (is.null(eixo_titulos[[ek]]))
      eixo_titulos[[ek]] <- if (identical(ek, "geral"))
        "Todas as variáveis" else ek
  }

  dic <- list(
    dataset   = "df",
    n         = nrow(df),
    seed      = NA,
    descricao = paste0("Databook gerado automaticamente — ",
                       length(nomes), " variáveis (tipos inferidos)."),
    eixos     = eixo_titulos,
    variaveis = variaveis
  )
  list(dic = dic,
       df  = as.data.frame(base, stringsAsFactors = FALSE, check.names = FALSE))
}

# COMANDO ÚNICO: gera o databook de TODAS as variáveis de um df em memória.
databook_auto <- function(df,
                          arquivo         = "output/databook.html",
                          titulo          = "Databook",
                          graficos        = TRUE,   # FALSE = só tabelas
                          cruzamentos     = NULL,
                          agrupar_por     = NULL,
                          limiar_continua = 12,
                          ids             = character(0),
                          continuas       = character(0),
                          categoricas     = character(0)) {
  prep <- dicionario_automatico(df, agrupar_por, limiar_continua,
                                ids, continuas, categoricas)
  db <- construir_databook(prep$df, prep$dic, cruzamentos)
  databook_html(db, arquivo, titulo, graficos = graficos)
  invisible(db)
}

# ---- Execução (só quando rodado via Rscript, não via source) ----------------
if (sys.nframe() == 0) {
  # =========================================================================
  # USO TÍPICO (interativo): com o seu df já no ambiente,
  #   source("R/03_gera_databook.R")
  #   databook_auto(df, "output/databook.html", "Databook — Coorte")
  # =========================================================================
  if (exists("df")) {
    databook_auto(
      df,
      arquivo         = "output/databook.html",
      titulo          = "Databook — Coorte (banco real)",
      cruzamentos     = NULL,        # ex.: list(c("a_idademae", "a_renda"),
                                     #           c("a_sitconj", "a_escmae"))
      agrupar_por     = NULL,        # NULL | "prefixo" | função(nome)->eixo
      limiar_continua = 12,          # numérica com >12 níveis -> contínua
      categoricas     = character(0) # ex.: c("a_bairro","a_seriepai","a_seriemae")
    )
    cat("\u2713 output/databook.html\n")
  } else {
    message("Defina 'df' no ambiente (ex.: df <- ...) e chame databook_auto(df).")
  }
}
