# 圖的顏色與符號（色盲友善的 Okabe-Ito 色）
COL_DIF <- c(none = "#555555", `favors girls` = "#D55E00", `favors boys` = "#0072B2", inconsistent = "#000000")
SYM_DIF <- c(none = "circle", `favors girls` = "triangle-up", `favors boys` = "triangle-down", inconsistent = "square")
COL_TWO <- c("#0072B2", "#D55E00")
SYM_TWO <- c("circle", "square")
COL_MANY <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")
FONT <- list(family = "Noto Sans CJK TC, sans-serif", size = 14)

#' Age ticks for plots
#'
#' @format A numeric vector: 3, 6, 12, 18, 24, 36, 48, 60, 72, 84, 96.
#' @examples
#' AGE_TICKS
#' @export
AGE_TICKS <- c(3, 6, 12, 18, 24, 36, 48, 60, 72, 84, 96)


# 題幹的短版（內部用，給成長階梯）：拿掉括號裡的例子；中文留前 14 個字；
# 英文留前 36 個字元，在字和字之間切，切掉的加刪節號。
short_stem <- function(stem) {
  stem <- ifelse(is.na(stem), "", stem)
  stem <- trimws(gsub("[\uff08(].*$", "", stem))
  english_short <- function(x) {
    if (nchar(x) <= 36) return(x)
    paste0(sub("\\s+\\S*$", "", substr(x, 1, 36)), "\u2026")
  }
  is_zh <- grepl("[\u4e00-\u9fa5]", stem)
  ifelse(is_zh, substr(stem, 1, 14), vapply(stem, english_short, character(1)))
}


# 滑鼠停在題目上看到的文字
item_hover <- function(d) {
  domain <- lab_name("domain", d$domain)
  paste0("<b>", d$item_id, "</b>  ", domain, "<br>", d$stem,
         "<br>", LAB$hover_step, " = ", sprintf("%.1f", d$D),
         "<br>", LAB$hover_age50, " = ", ifelse(is.na(d$age50), LAB$hover_na, sprintf("%.1f", d$age50)),
         "<br>", LAB$hover_reach, " = ", ifelse(is.na(d$age_reach), "\u2014", sprintf("%.1f",
                                                                                 d$age_reach)),
         "<br>", LAB$hover_dif, ": ", LAB$dif[d$DIF])
}

# plot() 方法的標題：靠左，不和右上角的工具列重疊
plot_title <- function(text) {
  list(text = text, x = 0.01, xanchor = "left", y = 0.98, yanchor = "top", font = list(size = 15))
}

# 圖格左上角的標題
panel_title <- function(text) {
  list(list(x = 0.02, y = 0.98, xref = "paper", yref = "paper", text = paste0("<b>", text, "</b>"),
            showarrow = FALSE, xanchor = "left", font = list(size = 15)))
}

# 一個領域的圖格標題；沒有領域（NA）時沒有標題
domain_panel_title <- function(domain) {
  if (is.na(domain)) {
    return(list())
  }
  panel_title(lab_name("domain", domain))
}

# 格子右下角的註記：這一格沒畫出來的題（圖上沒有位置）
not_drawn_note <- function(ids, label) {
  if (length(ids) == 0) return(list())
  list(list(x = 0.98, y = 0.02, xref = "paper", yref = "paper", text = paste0(label, paste(ids, collapse = ", ")),
            showarrow = FALSE, xanchor = "right", yanchor = "bottom", font = list(size = 11, color = "#666666")))
}

# 檢查題目表的 DIF 欄。NA 當成 "none"。
clean_dif <- function(dif) {
  dif[is.na(dif)] <- "none"
  bad <- setdiff(unique(dif), names(COL_DIF))
  if (length(bad) > 0) {
    use <- paste(names(COL_DIF), collapse = ", ")
    stop("Unknown DIF value(s): ", paste(bad, collapse = ", "), ". Use: ", use, ".", call. = FALSE)
  }
  dif
}

# D 軸的範圍：有給就用；沒給就用所有格的資料算一個共同範圍（取到 10 的倍數）
shared_range <- function(values, y_range) {
  if (!is.null(y_range)) return(y_range)
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    stop("No finite values to plot.", call. = FALSE)
  }
  r <- range(values)
  c(floor(r[1] / 10) * 10, ceiling(r[2] / 10) * 10)
}

# 檢查領域的代碼在資料裡
need_domains <- function(domains, data = NULL, arg = "data", word = "domain") {
  if (length(domains) == 0) {
    stop("`", word, "s` is empty.", call. = FALSE)
  }
  if (!is.null(data)) {
    none <- setdiff(domains, data$domain)
    if (length(none) > 0) {
      stop("No rows in `", arg, "` for ", word, "(s): ", paste(none, collapse = ", "), ".",
           call. = FALSE)
    }
  }
}

# ---- 成長曲線和階梯（plot(kits_item_table())）-----------------------------------------------------------

# 成長曲線：一個領域一格；灰色的線是常模的 P10、P50、P90，點是題目（顏色和形狀是 DIF）
growth_plot <- function(norm, items, domains, age_ticks = AGE_TICKS, y_range = NULL) {
  need_cols(norm, c("domain", "age_m", "P10", "P50", "P90"), "norm")
  need_cols(items, c("item_id", "domain", "x", "D", "empirical", "DIF", "stem", "age50",
                     "age_reach"), "items")
  need_domains(domains, norm, "norm")
  items$DIF <- clean_dif(items$DIF)
  norm <- dplyr::filter(as.data.frame(norm), domain %in% domains)
  items <- dplyr::filter(as.data.frame(items), domain %in% domains)
  y_range <- shared_range(c(norm$P10, norm$P90, items$D), y_range)
  no_x <- dplyr::filter(items, is.na(x))$item_id
  if (length(no_x) > 0) {
    message("Not drawn (no age on the chart; harder than the highest norm median and no age at 50% passing): ",
            paste(no_x, collapse = ", "))
  }
  legend_dif <- first_dif_panel(items, domains)
  panels <- purrr::imap(domains, \(dm, i) {
    growth_panel(dplyr::filter(norm, domain %in% dm), dplyr::filter(items, domain %in% dm), dm,
                 y_range,
                 legend_lines = i == 1, legend_dif = names(legend_dif)[legend_dif == i])
  })
  axis <- list(title = LAB$age, tickvals = age_ticks)
  plotly::subplot(panels, nrows = ceiling(length(domains) / 2), shareX = TRUE, titleY = TRUE,
                  margin = 0.04) |>
    plotly::layout(title = LAB$title_growth, xaxis = axis, xaxis2 = axis, font = FONT)
}

# 圖例每一類只出現一次：DIF 的每一類在第一個有它的格
first_dif_panel <- function(items, domains) {
  first <- items |>
    dplyr::filter(!is.na(x)) |>
    dplyr::summarise(panel = min(match(domain, domains)), .by = DIF)
  stats::setNames(first$panel, first$DIF)
}

# 成長曲線的一格：常模的 P10、P50、P90，和這個領域的題目
growth_panel <- function(norm, items, domain, y_range, legend_lines, legend_dif) {
  segments <- norm_segments(norm)
  drawn <- dplyr::filter(items, !is.na(x))
  p <- plotly::plot_ly()
  for (s in names(segments)) {
    d <- segments[[s]]
    for (q in c("P10", "P50", "P90")) {
      line <- list(color = "#999999", width = if (q == "P50") 2.5 else 1)
      legend <- legend_lines && s == names(segments)[1]
      p <- plotly::add_lines(p, x = d$age_m, y = d[[q]], line = line,
                             name = q, legendgroup = q, showlegend = legend, hoverinfo = "skip")
    }
  }
  for (lv in names(COL_DIF)) {
    d <- dplyr::filter(drawn, DIF == lv)
    if (nrow(d) == 0) next
    symbol <- ifelse(d$empirical, SYM_DIF[[lv]], paste0(SYM_DIF[[lv]], "-open"))
    marker <- list(color = COL_DIF[[lv]], symbol = symbol, size = 8)
    p <- plotly::add_markers(p, x = d$x, y = d$D, name = LAB$dif[[lv]], legendgroup = lv,
                             showlegend = lv %in% legend_dif, marker = marker,
                             hovertext = item_hover(d), hoverinfo = "text")
  }
  note <- not_drawn_note(dplyr::filter(items, is.na(x))$item_id, LAB$not_drawn)
  y_axis <- list(title = LAB$D, range = y_range)
  plotly::layout(p, annotations = c(domain_panel_title(domain), note), yaxis = y_axis)
}

# 成長階梯：一欄一個領域（或 column），每題一條橫線在它的 D；右邊的刻度是常模中位數到達這個 D 的月齡
ladder_plot <- function(items, columns, marks) {
  need_cols(items, c("item_id", "domain", "column", "D", "DIF", "stem", "age50", "age_reach"),
            "items")
  need_cols(marks, c("age_m", "D"), "marks")
  it <- dplyr::filter(as.data.frame(items), column %in% columns)
  if (nrow(it) == 0) {
    stop("No items in `items` have a `column` in `columns`.", call. = FALSE)
  }
  it <- it |>
    dplyr::mutate(DIF = clean_dif(DIF),
                  x = match(column, columns),
                  label = paste0(sprintf("%.1f", D), "  ", short_stem(stem)))
  p <- plotly::plot_ly()
  for (lv in names(COL_DIF)) {
    d <- dplyr::filter(it, DIF == lv)
    if (nrow(d) == 0) next
    p <- p |>
      plotly::add_trace(x = as.vector(rbind(d$x - 0.2, d$x + 0.02, NA)), y = as.vector(rbind(d$D, d$D, NA)),
                        type = "scatter", mode = "lines",
                        line = list(color = COL_DIF[[lv]], width = if (lv == "none") 2 else 4),
                        name = LAB$dif[[lv]], legendgroup = lv, hoverinfo = "skip") |>
      plotly::add_trace(x = d$x + 0.05, y = d$D, type = "scatter", mode = "text",
                        text = d$label, textposition = "middle right",
                        textfont = list(size = 10, color = COL_DIF[[lv]]),
                        hovertext = item_hover(d), hoverinfo = "text",
                        legendgroup = lv, showlegend = FALSE)
  }
  right <- length(columns) + 0.6
  for (D in marks$D) {
    p <- plotly::add_lines(p, x = c(0.5, right), y = c(D, D),
                           line = list(color = "#E5E5E5", width = 1), hoverinfo = "skip", showlegend = FALSE)
  }
  mark_text <- purrr::map2(marks$D, marks$age_m, \(D, age) {
    list(x = right + 0.02, y = D, text = paste0(age, LAB$month),
         showarrow = FALSE, xanchor = "left", font = list(size = 11, color = "#666666"))
  })
  x_axis <- list(tickvals = seq_along(columns), ticktext = lab_name("column", columns),
                 range = c(0.4, length(columns) + 0.9), showgrid = FALSE, zeroline = FALSE)
  plotly::layout(p, title = LAB$title_ladder, xaxis = x_axis, yaxis = list(title = LAB$D_step,
                                                                           dtick = 10),
                 annotations = mark_text, font = FONT)
}


# ---- 常模的格子（plot() 照組的常模）------------------------------------------------------------------

# 一列一個領域，一欄一組（例如性別）。norms 有 domain、group 兩欄（沒有時 NA）和百分位的欄；
# 最外面的兩條百分位畫虛線，P50 畫粗線。children：畫在每一格曲線後面的孩子（theta、age_m、domain、group）
norm_grid_plot <- function(norms, domains, groups, pct, age_ticks, y_range, title,
                           children = NULL) {
  need_domains(domains, norms, "norms")
  empty <- data.frame(group = groups) |>
    dplyr::cross_join(data.frame(domain = domains)) |>   # 領域變得最快，和錯誤訊息的順序相同
    dplyr::anti_join(norms, by = c("domain", "group"))
  if (nrow(empty) > 0) {
    stop("No rows in `norms` for: ", paste(empty$domain, empty$group, collapse = ", "), ".",
         call. = FALSE)
  }
  cols <- paste0("P", pct)
  shown <- dplyr::filter(norms, domain %in% domains, group %in% groups)
  y_range <- shared_range(c(shown[[cols[1]]], shown[[cols[length(cols)]]]), y_range)
  grid <- dplyr::cross_join(data.frame(dm = domains), data.frame(i = seq_along(groups)))
  panels <- purrr::map2(grid$dm, grid$i, \(dm, i) {
    norm_grid_panel(norms, dm, groups, i, cols, dm %in% domains[1] && i == 1, y_range, children)
  })
  axis <- list(title = LAB$age, tickvals = age_ticks)
  plotly::subplot(panels, nrows = length(domains), shareX = TRUE, titleY = TRUE,
                  margin = c(0.03, 0.03, 0.015, 0.015)) |>
    plotly::layout(title = title, xaxis = axis, xaxis2 = axis, font = FONT)
}

# 一格：一個領域、一組的百分位曲線（每一段各畫一次），和這一組的孩子
norm_grid_panel <- function(norms, dm, groups, i, cols, legend, y_range, children) {
  gr <- groups[i]
  colour <- COL_MANY[(i - 1) %% length(COL_MANY) + 1]
  segments <- norm_segments(dplyr::filter(norms, domain %in% dm, group %in% gr))
  outer <- cols[c(1, length(cols))]
  p <- plotly::plot_ly()
  if (!is.null(children)) {
    own <- dplyr::filter(children, domain %in% dm, group %in% gr)
    p <- plotly::add_markers(p, x = own$age_m, y = own$theta, marker = list(color = "#BBBBBB",
                                                                            size = 3),
                             name = LAB$persons, showlegend = legend, hoverinfo = "skip")
  }
  for (s in names(segments)) {
    d <- segments[[s]]
    for (q in cols) {
      hover <- sprintf("%s, %s, %.1f%s: D = %.1f", norm_panel_name(dm, gr), q, d$age_m, LAB$month,
                       d[[q]])
      line <- list(color = colour, width = if (q == "P50") 2.5 else 1,
                   dash = if (q %in% outer && length(cols) > 1) "dot" else "solid")
      p <- plotly::add_lines(p, x = d$age_m, y = d[[q]], name = q, legendgroup = q,
                             showlegend = legend && s == names(segments)[1], line = line,
                             hovertext = hover, hoverinfo = "text")
    }
  }
  plotly::layout(p, annotations = norm_panel_title(dm, gr), yaxis = list(title = LAB$D,
                                                                         range = y_range))
}

# 一格的名稱：「領域, 組」（滑鼠停的文字）；沒有的部分不寫
norm_panel_name <- function(dm, gr) {
  parts <- c(if (!is.na(dm)) lab_name("domain", dm), if (!is.na(gr)) lab_name("sex", gr))
  paste(parts, collapse = ", ")
}

# 一格的標題：「領域 (組)」；只有一個常模（沒有領域、組）時沒有標題
norm_panel_title <- function(dm, gr) {
  if (is.na(dm) && is.na(gr)) {
    return(list())
  }
  text <- if (is.na(dm)) lab_name("sex", gr) else lab_name("domain", dm)
  if (!is.na(dm) && !is.na(gr)) text <- paste0(text, " (", lab_name("sex", gr), ")")
  panel_title(text)
}


#' Save a plot: interactive html and static png
#'
#' The html is an interactive plot.
#' It can be zoomed, and hovering over an item shows the item.
#' The png is a browser screenshot of the same plot.
#'
#' Saving html needs pandoc.
#' RStudio and Quarto both come with pandoc.
#' If it is not found, set the environment variable `RSTUDIO_PANDOC` to the folder that contains pandoc.
#'
#' Saving png needs the webshot2 package.
#' It also needs a Chromium-based browser (Chrome, Edge, Brave).
#' The path of the browser is set in the environment variable `CHROMOTE_CHROME`.
#'
#' @param p A plotly plot.
#' @param html The html file name; it must end in `.html`.
#' @param png The png file name. Omit it when no png is wanted.
#' @param width,height Width and height of the png (pixels).
#' @return The saved file names (invisibly).
#' @examples
#' s <- data.frame(cohort = rep(c("M03", "M36"), each = 100), wave_m = 36,
#'                 theta = rep(c(68, 69), each = 100) + rnorm(200, 0, 4))
#' p <- plot(kits_vertical(s), type = "median")
#' \donttest{
#' # Save html only (needs pandoc)
#' if (rmarkdown::pandoc_available()) kits_save_plot(p, file.path(tempdir(), "cohort.html"))
#' }
#' \dontrun{
#' # Also save png (needs webshot2 and a browser)
#' kits_save_plot(p, file.path(tempdir(), "cohort.html"), file.path(tempdir(), "cohort.png"),
#'           width = 800, height = 420)
#' }
#' @export
kits_save_plot <- function(p, html, png = NULL, width = 1200, height = 800) {
  if (!inherits(p, "plotly")) {
    stop("`p` must be a plotly plot (for example from plot()).", call. = FALSE)
  }
  if (!grepl("\\.html$", html)) {
    stop("`html` must end with .html", call. = FALSE)
  }
  name <- sub("\\.html$", "", basename(html))
  htmlwidgets::saveWidget(p, html, selfcontained = TRUE, title = name)
  unlink(sub("\\.html$", "_files", html), recursive = TRUE)
  if (is.null(png)) {
    return(invisible(html))
  }

  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("Saving png needs the webshot2 package: install.packages(\"webshot2\")", call. = FALSE)
  }
  still <- p |>
    plotly::layout(margin = list(t = 90)) |>
    plotly::config(displayModeBar = FALSE)
  still$width <- width     # 圖的大小設在 htmlwidget 上（plotly 不再建議用 layout() 設）
  still$height <- height
  tmp <- file.path(tempdir(), paste0(name, ".html"))
  htmlwidgets::saveWidget(still, tmp, selfcontained = TRUE)
  webshot2::webshot(tmp, png, vwidth = width, vheight = height, zoom = 2, quiet = TRUE)
  invisible(c(html, png))
}
