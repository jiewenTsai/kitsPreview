# 垂直量尺的位置和分散（kits_vertical，和 kits 0.22.0 同名）：每個世代 x 波次的人數、月齡中位數、平均、SD、
# 中位數、P25、P75；plot() 畫兩個世代的平均 +/- SD 或中位數（P25 到 P75）。

#' Location and spread of each wave on one scale
#'
#' Once the scores of every cohort are on one scale (for example D), `kits_vertical()` gives for
#' each cohort x wave: `n`, `age_med` (the median age, when the scores have `age_m`), `mean`, `sd`,
#' `median`, and the quartiles `P25` and `P75` (`stats::quantile()`, its default type 7).
#'
#' `plot()` draws one line per cohort (blue circles, orange squares, ...) at the actual median age
#' of each wave (`age_med`; the wave when the scores have no `age_m`):
#' `type = "mean"` (the default) the mean +/- SD, `type = "median"` the median with bars from P25 to
#' P75. Where the two cohorts share a wave, their points should be close (the cohort link).
#' When the median age differs from the wave by more than 1 month, a note under the title gives it,
#' and hovering over a point shows it.
#'
#' Several domains: make one `kits_vertical()` per domain and stack them with a `domain` column,
#' `list(lan = v_lan, motor = v_motor) |> purrr::list_rbind(names_to = "domain")`; `plot()` then
#' draws one panel per domain.
#'
#' kits has the same function; it also gives the growth from wave to wave, the effect size, the
#' probability of superiority, the SD without the measurement error (`sd_true`), the linking error
#' of each mean and a comparison of the cohorts at shared waves (Kolen & Brennan, 2014, pp.
#' 461-469). Until kitsPreview 0.22.0 this plot was also `kits_plot_cohort()` on a table summarised
#' by hand (removed in 0.23.0).
#'
#' @param s Scores on one scale: the scores of [kits_subj()] (converted) with `wave_m` and
#'   optionally `cohort` and `age_m`; or a data.frame with `theta` and `wave_m` and any of those.
#' @return A `kitsPreview_vertical` (a data.frame) with one row per cohort x wave, in the order of
#'   cohort and wave: `cohort` (when given), `wave_m`, `n`, `age_med` (when `age_m` is given),
#'   `mean`, `sd`, `median`, `P25`, `P75`.
#' @references
#' Kolen, M. J., & Brennan, R. L. (2014). *Test equating, scaling, and linking* (3rd ed.). Springer.
#' \doi{10.1007/978-1-4939-0317-7}
#' @examples
#' set.seed(1)
#' s <- data.frame(cohort = rep(c("M03", "M36"), c(600, 400)),
#'                 wave_m = c(rep(c(12, 24, 36), each = 200), rep(c(36, 48), each = 200)))
#' s$theta <- 40 + 8 * log(s$wave_m) + rnorm(1000, 0, 4)
#' v <- kits_vertical(s)
#' v
#' p <- plot(v)
#'
#' # The medians with P25 to P75
#' p <- plot(v, type = "median")
#' if (interactive()) p
#'
#' # Two domains stacked: one panel per domain
#' s_motor <- s
#' s_motor$theta <- s$theta + 5
#' both <- list(lan = v, motor = kits_vertical(s_motor)) |>
#'   purrr::list_rbind(names_to = "domain")
#' p <- plot(both)
#' @export
kits_vertical <- function(s) {
  if (!is.data.frame(s)) {
    stop("`s` must be the scores of kits_subj() or a data.frame with theta and wave_m.",
         call. = FALSE)
  }
  need_cols(s, c("theta", "wave_m"), "s")
  if (!is.numeric(s$theta) || !is.numeric(s$wave_m)) {
    stop("`s$theta` and `s$wave_m` must be numbers.", call. = FALSE)
  }
  d <- as.data.frame(s) |>
    dplyr::filter(is.finite(theta), !is.na(wave_m))
  has_cohort <- "cohort" %in% names(d)
  has_age <- "age_m" %in% names(d)
  d$cohort <- if (has_cohort) as.character(d$cohort) else NA_character_
  if (!has_age) d$age_m <- NA_real_
  tab <- d |>
    dplyr::summarise(n = dplyr::n(),
                     age_med = stats::median(age_m, na.rm = TRUE),
                     mean = mean(theta),
                     sd = if (dplyr::n() > 1) stats::sd(theta) else NA_real_,
                     median = stats::median(theta),
                     P25 = stats::quantile(theta, 0.25, names = FALSE),
                     P75 = stats::quantile(theta, 0.75, names = FALSE),
                     .by = c(cohort, wave_m)) |>
    dplyr::arrange(cohort, wave_m)
  if (!has_cohort) tab$cohort <- NULL
  if (!has_age) tab$age_med <- NULL
  preview_table(tab, "kitsPreview_vertical")
}

#' @rdname kits_vertical
#' @param x A `kitsPreview_vertical`.
#' @param digits Decimals to print.
#' @param n Rows to print: by default all rows of a table of up to 30 rows, else the first 10.
#' @param ... Not used.
#' @export
print.kitsPreview_vertical <- function(x, digits = 2, n = NULL, ...) {
  cat(sprintf("<kits_vertical> %d cohort x wave group(s)\n", nrow(x)))
  print_rows(x, digits, n, row.names = FALSE)
}

#' @rdname kits_vertical
#' @param type What `plot()` draws: `"mean"` (the mean +/- SD) or `"median"` (the median, bars from
#'   P25 to P75).
#' @param cohorts Which cohorts to draw, in this order (the first blue circles, the second orange
#'   squares); by default all of them.
#' @param age_ticks Age ticks.
#' @param y_range The range of the y axis, the same in every panel; by default from the data.
#' @export
plot.kitsPreview_vertical <- function(x, type = c("mean", "median"), cohorts = NULL,
                                      age_ticks = AGE_TICKS,
                                      y_range = NULL, ...) {
  type <- match.arg(type)
  vertical_plot(vertical_rows(x), type, cohorts, age_ticks, y_range)
}

# 圖用的列：沒有 domain 欄（一個領域）時 NA，沒有 cohort 欄時 "all"，沒有 age_med 時 NA
vertical_rows <- function(x) {
  d <- as.data.frame(x)
  if (!"domain" %in% names(d)) d$domain <- NA_character_
  if (!"cohort" %in% names(d)) d$cohort <- "all"
  if (!"age_med" %in% names(d)) d$age_med <- NA_real_
  d
}

# 一個領域一格，每個世代一條線（plot() 用）
vertical_plot <- function(d, type, cohorts, age_ticks, y_range, domains = unique(d$domain)) {
  if (is.null(cohorts)) cohorts <- unique(d$cohort)
  if (!any(cohorts %in% d$cohort)) {
    stop("None of `cohorts` (", paste(cohorts, collapse = ", "), ") is in the table: ",
         paste(unique(d$cohort), collapse = ", "), ". Set `cohorts`.", call. = FALSE)
  }
  d <- dplyr::filter(d, cohort %in% cohorts, domain %in% domains)
  use_age <- all(is.finite(d$age_med))
  d$x <- if (use_age) d$age_med else d$wave_m
  titled <- length(domains) > 1 || !is.na(domains[1])
  panels <- purrr::imap(domains, \(dm, i) vertical_panel(dplyr::filter(d, domain %in% dm), dm,
                                                         cohorts, type,
                                                           titled, i == 1))
  y_axis <- list(title = if (type == "mean") LAB$mean_sd else LAB$median_iqr, range = y_range)
  title <- paste0(if (type == "mean") LAB$title_vertical else LAB$title_vertical_median,
                  age_note(d))
  x_axis <- list(title = if (use_age) LAB$age else LAB$wave_axis, tickvals = age_ticks)
  panels <- purrr::map(panels, \(p) plotly::layout(p, yaxis = y_axis))
  plotly::subplot(panels, nrows = ceiling(length(panels) / 2), shareX = TRUE, titleY = TRUE,
                  margin = 0.04) |>
    plotly::layout(title = title, xaxis = x_axis, xaxis2 = x_axis, font = FONT)
}

# 實際月齡的中位數和波次差超過 1 個月的波次：標題下的註記（沒有時空字串）
age_note <- function(d) {
  late <- d |>
    dplyr::filter(is.finite(age_med), abs(age_med - wave_m) > 1) |>
    dplyr::summarise(age_med = stats::median(age_med), .by = wave_m) |>
    dplyr::arrange(wave_m)
  if (nrow(late) == 0) {
    return("")
  }
  note <- paste0(late$wave_m, LAB$wave, " ", sprintf("%.1f", late$age_med), LAB$month,
                 collapse = " / ")
  paste0("<br>", LAB$median_age, ": ", note)
}

# 一個領域的一格：每個世代一條線；圖例只在第一格
vertical_panel <- function(d, dm, cohorts, type, titled, legend) {
  p <- plotly::plot_ly()
  for (i in seq_along(cohorts)) {
    g <- dplyr::filter(d, cohort == cohorts[i])
    if (nrow(g) > 0) p <- add_vertical_trace(p, g, cohorts[i], i, type, legend)
  }
  if (titled) p <- plotly::layout(p, annotations = domain_panel_title(dm))
  p
}

# 一個世代的線：平均和對稱的 SD，或中位數和不對稱的 P25、P75
add_vertical_trace <- function(p, g, cohort, i, type, legend) {
  col <- COL_MANY[(i - 1) %% length(COL_MANY) + 1]
  age <- ifelse(is.finite(g$age_med), sprintf(" (%s %.1f)", LAB$median_age, g$age_med), "")
  wave <- paste0(cohort, " ", format(g$wave_m), LAB$wave, age)
  if (type == "mean") {
    y <- g$mean
    bars <- list(type = "data", array = g$sd, color = col)
    hover <- sprintf("%s: mean %.1f, SD %.1f, n = %d", wave, g$mean, g$sd, as.integer(g$n))
  } else {
    y <- g$median
    bars <- list(type = "data", symmetric = FALSE, array = g$P75 - g$median,
                 arrayminus = g$median - g$P25,
                 color = col)
    hover <- sprintf("%s: P50 = %.1f (P25 %.1f, P75 %.1f), n = %d", wave, g$median, g$P25, g$P75,
                     as.integer(g$n))
  }
  marker <- list(color = col, size = 9, symbol = SYM_TWO[(i - 1) %% 2 + 1])
  line <- list(color = col)
  plotly::add_trace(p, x = g$x, y = y, type = "scatter", mode = "lines+markers", name = cohort,
                    legendgroup = cohort, showlegend = legend, line = line, marker = marker,
                    error_y = bars, hovertext = hover, hoverinfo = "text")
}
