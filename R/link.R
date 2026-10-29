# mean/sigma 的直線（內部用）：新值 = intercept + slope x 舊值。slope = 兩邊 SD 的比，intercept 讓兩邊的
# 平均對齊（Kolen & Brennan 的 B、A）。不篩題的結果是連結的 intercept0、slope0（kits_link_screen()）。
mean_sigma <- function(from, to) {
  slope <- stats::sd(to) / stats::sd(from)
  list(intercept = mean(to) - slope * mean(from), slope = slope)
}

# 共用斜率、各組截距的 mean/sigma（內部用，給 kits_linking_d()）。
# 每組的 from、to 先減掉自己組的平均；合併後的 SD 比是共用斜率；每組的截距 = 該組 to 的平均 − 斜率 × 該組 from 的平均。
mean_sigma_shared_slope <- function(from, to, group) {
  dx <- from - stats::ave(from, group)   # ave()：每一筆換成所屬組的平均
  dy <- to - stats::ave(to, group)
  slope <- sqrt(sum(dy^2) / sum(dx^2))
  intercept <- tapply(to, group, mean) - slope * tapply(from, group, mean)
  list(intercept = c(intercept), slope = slope)
}

# kits_item() 的結果 → 它的門檻表；data.frame 照用。
# domains = TRUE 時也收有名字的 list（一個領域一個 kits_item() 結果或門檻表），疊起來並加 domain 欄。
as_steps <- function(x, arg, domains = FALSE) {
  if (inherits(x, "kitsPreview_fit")) x <- x$item
  if (inherits(x, "kitsPreview_item")) return(x$thresholds)
  if (domains && is.list(x) && !is.data.frame(x)) {
    if (is.null(names(x)) || any(names(x) == "") || anyDuplicated(names(x))) {
      stop(sprintf("`%s` is a list: name each element by its domain, e.g. list(lan = item_lan, cog = item_cog).", arg), call. = FALSE)
    }
    parts <- lapply(names(x), function(d) {
      s <- as_steps(x[[d]], arg)
      need_cols(s, c("item_id", "item_score", "thr"), arg)
      data.frame(s[c("item_id", "item_score", "thr")], domain = d)
    })
    return(do.call(rbind, parts))
  }
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a kitsPreview::kits_item() result or a table with item_id, item_score, thr. ", arg),
         "For a result of kits::kits_item(), give coef(item).", call. = FALSE)
  }
  x
}

# 檢查門檻表：要有這些欄，(item_id, item_score) 不能重複
need_steps <- function(steps, arg) {
  need_cols(steps, c("item_id", "item_score", "thr"), arg)
  if (!is.numeric(steps$thr) || any(!is.finite(steps$thr))) {
    stop(sprintf("`%s$thr` must be finite numbers (no NA or Inf).", arg), call. = FALSE)
  }
  dup <- duplicated(steps[c("item_id", "item_score")])
  if (any(dup)) {
    stop(sprintf("`%s` has repeated item steps (for example %s:%s). Use the table of one cohort only.",
                 arg, steps$item_id[dup][1], steps$item_score[dup][1]), call. = FALSE)
  }
}


#' Print or plot a link
#'
#' A link is a straight line: new value = intercept + slope x old value (Kolen and Brennan, 2014,
#' call the slope A and the intercept B).
#' `print()` shows the line, the linking error, and the dropped points.
#' The result is still a list: use `link$intercept`, `link$slope`, `link$points`, and so on;
#' `coef(link)` gives `c(intercept = , slope = )`, in the order of `coef()` of `lm()`.
#'
#' Until kitsPreview 0.22.0 the intercept and the slope were called `a` and `b` (and `a_error`,
#' `b_error`, `a0`, `b0`); these names are no longer elements, so `link$a` is `NULL`.
#'
#' `plot()` draws every point (`from` against `to`) with the line.
#' Dropped points are marked; hovering shows the point and its residual.
#' The dashed lines show why a point was dropped: the line before screening (all points,
#' `intercept0`, `slope0`)
#' and the screening band around it, `cut` x SD of the residuals from that line.
#' A point outside the band is dropped; the solid line is then fitted with the kept points.
#' When no point is dropped, the two lines coincide.
#' Points far from the line, or a curve instead of a line, mean the link should not be trusted as it is.
#'
#' @param x The result of [kits_linking()], [kits_linking_d()] or [kits_link_line()] (a
#'   `kitsPreview_link`).
#' @param digits Decimals to print (only the printout is rounded; the object keeps full precision).
#' @param ... Not used.
#' @return `print()`: `x` (invisibly). `plot()`: a plotly plot. `coef()`: `c(intercept = , slope = )`.
#' @references
#' Kolen, M. J., & Brennan, R. L. (2014). *Test equating, scaling, and linking* (3rd ed.). Springer.
#' \doi{10.1007/978-1-4939-0317-7}
#' @examples
#' points <- data.frame(id = paste0("p", 1:6), item = paste0("p", 1:6),
#'                      from = c(-2, -1, 0, 1, 2, 3), to = c(-1, 0, 1, 2, 3.2, 6))
#' link <- kits_link_line(kits_link_screen(points))
#' link
#' coef(link)
#' link$slope
#'
#' p <- plot(link)
#' if (interactive()) p
#' @export
print.kitsPreview_link <- function(x, digits = 2, ...) {
  need_digits(digits)
  err <- function(e) if (length(e) != 1 || is.na(e)) "" else sprintf("  (linking error %s)", fmt(e, digits))
  cat("<kits_link> new value = intercept + slope x old value\n")
  if (!is.null(x$from_scale) || !is.null(x$to_scale)) {
    cat(sprintf("  scales: %s -> %s\n", x$from_scale %||% "?", x$to_scale %||% "?"))
  }
  cat(sprintf("  intercept = %s%s\n", fmt(x$intercept, digits), err(x$intercept_error)))
  cat(sprintf("  slope     = %s%s\n", fmt(x$slope, digits), err(x$slope_error)))
  if (!is.null(link_points_text(x))) cat(sprintf("  points from: %s\n", link_points_text(x)))
  cat(sprintf("  points: %d%s; dropped: %d%s\n", x$n, if (is.null(x$n_items)) "" else sprintf(" (items: %d)", x$n_items),
              length(x$dropped), if (length(x$dropped) > 0) paste0(" (", paste(x$dropped, collapse = ", "), ")") else ""))
  cat(sprintf("  before screening: intercept %s, slope %s; screening: |e| > %s SD(e)\n",
              fmt(x$intercept0, digits), fmt(x$slope0, digits), format(x$settings$cut)))
  if (!is.null(x$domains) && nrow(x$domains) > 1) {
    cat(sprintf("  kept points by domain: %s\n", domain_balance_text(x$domains)))
  }
  if (!is.null(x$domains)) {
    cat("  domain check: one slope shared by the domains, fitted with domain intercepts (kept points);\n",
        "  it is not the slope of the line above:\n", sep = "")
    print(format_table(x$domains, digits), row.names = FALSE, right = TRUE)
  }
  cat("Next: plot(x), coef(x), kits_code(x); scores: s$theta <- x$intercept + x$slope * s$theta.\n")
  invisible(x)
}

# 連結用了哪些點（和 kits 的 print 相同的字）；自己準備的點（kits_link_line()）是 NULL
link_points_text <- function(x) {
  st <- x$settings
  if (is.null(st$kind)) {
    return(NULL)
  }
  if (st$kind == "calibration") {
    return("every common item step")
  }
  sprintf("the pass step of each anchor item (score >= %s%s)", st$step,
          if (isTRUE(st$own_step)) "; some anchors set their own" else "")
}

# 每個領域留下的點：人數和比例，多的在前（和 kits 相同）
domain_balance_text <- function(domains) {
  d <- as.data.frame(domains)
  d$share <- 100 * d$n / sum(d$n)
  d <- d[order(-d$n, d$domain), ]
  paste(sprintf("%s %d (%.0f%%)", d$domain, as.integer(d$n), d$share), collapse = ", ")
}

#' @rdname print.kitsPreview_link
#' @param object A link.
#' @export
coef.kitsPreview_link <- function(object, ...) {
  c(intercept = object$intercept, slope = object$slope)
}

#' @rdname print.kitsPreview_link
#' @export
plot.kitsPreview_link <- function(x, ...) {
  pts <- x$points
  hover <- sprintf("%s<br>from %.2f, to %.2f<br>residual %.2f", pts$id, pts$from, pts$to, pts$resid_final)
  line_x <- range(pts$from)
  p <- plotly::plot_ly() |>
    plotly::add_lines(x = line_x, y = x$intercept + x$slope * line_x, line = list(color = "#999999"),
                      name = sprintf("%.2f + %.2f x", x$intercept, x$slope), hoverinfo = "skip")
  band <- x$settings$cut * stats::sd(pts$resid)                    # 篩選的邊界：第一條線 ± cut x SD(e)
  p <- p |>
    plotly::add_lines(x = line_x, y = x$intercept0 + x$slope0 * line_x,
                      line = list(color = "#D55E00", width = 1, dash = "dot"),
                      name = sprintf(LAB$first_line, x$intercept0, x$slope0), hoverinfo = "skip") |>
    plotly::add_lines(x = c(line_x, NA, line_x),
                      y = c(x$intercept0 + x$slope0 * line_x + band, NA,
                            x$intercept0 + x$slope0 * line_x - band),
                      line = list(color = "#D55E00", width = 1, dash = "dash"),
                      name = sprintf(LAB$band, x$settings$cut), hoverinfo = "skip")
  for (drop in c(FALSE, TRUE)) {
    d <- pts$dropped == drop
    if (!any(d)) next
    p <- plotly::add_markers(p, x = pts$from[d], y = pts$to[d], hovertext = hover[d], hoverinfo = "text",
                             name = if (drop) LAB$dropped else LAB$kept,
                             marker = list(color = if (drop) "#D55E00" else "#0072B2", size = 9,
                                           symbol = if (drop) "x" else "circle"))
  }
  plotly::layout(p, title = plot_title(LAB$title_link), margin = list(t = 70), xaxis = list(title = LAB$link_from),
                 yaxis = list(title = LAB$link_to), font = FONT)
}

#' Cohort link: put one cohort's scale onto the reference cohort's scale
#'
#' Use the common items of the two cohorts.
#' Steps are matched by `item_id` and `item_score`.
#' A step that exists in only one cohort (for example a score that nobody reached in one cohort) is not used.
#' The result is always in kits' convention: new value = intercept + slope x old value,
#' where "old" is the scale of `from` and "new" the scale of `to`.
#' To convert this cohort's theta to the reference scale: `link$intercept + link$slope * theta`
#' (and the SE: `link$slope * se`).
#'
#' Each step of each common item is one point: its threshold in `from` and in `to`.
#' The line is fitted with the steps of [kits_link_steps] (one round of screening),
#' and the linking error resamples whole items. No other package is used: [kits_code()] writes the whole link in base R.
#'
#' @param from The [kits_item()] result of the cohort to be converted (for example M36), or its `thresholds` table.
#' @param to The [kits_item()] result of the reference cohort (for example M03), or its `thresholds` table.
#' @inheritParams kits_link_steps
#' @return A `kitsPreview_link` (a list, see [kits_link_steps]), plus `n_items` (the number of
#'   common items). The points are named `"item_id:item_score"`.
#' By default (`n_boot = 0`, as in kits) the linking error is not computed: give `n_boot` (for
#' example 200 to see its size, 2000 for reporting), or add it later with [kits_link_error()].
#' Until kitsPreview 0.22.0 the default was `n_boot = 2000`.
#'
#' @examples
#' # Each cohort answers part of the items; a3, a4, a5 are common
#' betas <- list(a1 = -2, a2 = -1, a3 = c(-0.5, 0.5), a4 = 0, a5 = c(0.5, 1.5), a6 = 2, a7 = 3)
#' young <- kits_sim(rnorm(500, 0), betas[1:5], seed = 1)
#' old <- kits_sim(rnorm(500, 2), betas[3:7], prefix = "q", seed = 2)
#' link <- kits_linking(kits_item(old), kits_item(young), n_boot = 200)
#' link
#'
#' # Theta of the older cohort on the younger cohort's scale
#' theta <- c(-1, 0, 1)
#' link$intercept + link$slope * theta
#' @export
kits_linking <- function(from, to, cut = 2, n_boot = 0, seed = 1) {
  steps_from <- as_steps(from, "from")
  steps_to <- as_steps(to, "to")
  need_steps(steps_from, "from")
  need_steps(steps_to, "to")
  common <- merge(steps_from, steps_to, by = c("item_id", "item_score"), suffixes = c("_from", "_to"))
  if (nrow(common) < 3) {
    stop("The two tables have ", nrow(common), " common item step(s); at least 3 are needed.", call. = FALSE)
  }
  link <- mean_sigma_link(common$thr_from, common$thr_to, id = paste0(common$item_id, ":",
                                                                      common$item_score),
                          item = common$item_id, cut = cut, n_boot = n_boot, seed = seed)
  link$n_items <- length(unique(common$item_id))
  link$settings[c("kind", "step")] <- list("calibration", "all")
  link
}

#' Link to D: anchor items against the GSED difficulties
#'
#' Each anchor item is one point:
#' the threshold of its pass step on this scale, and its GSED difficulty `tau` on the D scale.
#'
#' GSED items are pass/fail, so each KIT item needs a pass cut: "pass" is a score of at least `step`.
#' By default `step = 1`, the milestone step.
#' With KIT's recoding (raw 1-4 becomes 0-3) that is a raw score of at least 2.
#' A different cut for some anchors: give `anchors` a `step` column (it overrides `step` for those items).
#' The line is fitted with the steps of [kits_link_steps] (one round of screening),
#' and the linking error resamples the anchors.
#'
#' Several domains can share one line:
#' give a named list, one [kits_item()] per domain (for example `list(lan = item_lan, cog = item_cog)`).
#' Then the output also has `domains`: with the same kept anchors,
#' one shared slope and one intercept per domain.
#' When the domain intercepts are close to the common `intercept`, one common line is reasonable.
#'
#' To convert theta (on the reference cohort's scale) to D: `link$intercept + link$slope * theta`
#' (and the SE: `link$slope * se`).
#' For a domain without anchors, the same line gives a score in the same unit,
#' but it cannot be read as the GSED D.
#'
#' @param steps The [kits_item()] result of the reference cohort, a named list of them (one per domain),
#'   or a thresholds table (optionally with a `domain` column).
#' @param anchors Anchor items: `item_id` and `tau` (the GSED difficulty of the matched item);
#'   optionally `step`, the pass step of that item.
#' @param step The pass step of the anchor items: "pass" is a score of at least `step` (default 1).
#' @inheritParams kits_link_steps
#' @return A `kitsPreview_link` (a list, see [kits_link_steps]), plus `n_items` (the number of
#'   anchors used). When `steps` has a `domain` column, also `domains`:
#'   one row per domain, `domain`, `n` (kept anchors), `intercept`, `slope` (shared slope).
#' By default (`n_boot = 0`, as in kits) the linking error is not computed: give `n_boot` (for
#' example 200 to see its size, 2000 for reporting). Until kitsPreview 0.22.0 the default was
#' `n_boot = 2000`.
#'
#' @examples
#' set.seed(1)
#' steps <- data.frame(item_id = paste0("a", 1:12), item_score = 1, thr = rnorm(12),
#'                     domain = rep(c("lan", "cog"), each = 6))
#' anchors <- data.frame(item_id = steps$item_id, tau = 64 + 7 * steps$thr + rnorm(12, 0, 0.5))
#' link <- kits_linking_d(steps, anchors, n_boot = 200)
#' link
#'
#' # D of theta = -1, 0, 1
#' link$intercept + link$slope * c(-1, 0, 1)
#' @export
kits_linking_d <- function(steps, anchors, step = 1, cut = 2, n_boot = 0, seed = 1) {
  steps <- as_steps(steps, "steps", domains = TRUE)
  if (!is.numeric(step) || length(step) != 1 || is.na(step) || step < 1 || step != round(step)) {
    stop("`step` must be one whole number (1 = the milestone step).", call. = FALSE)
  }
  need_steps(steps, "steps")
  need_cols(anchors, c("item_id", "tau"), "anchors")
  if (anyDuplicated(anchors$item_id)) {
    stop("`anchors` has repeated item_id: ", anchors$item_id[duplicated(anchors$item_id)][1], ".", call. = FALSE)
  }
  if ("domain" %in% names(steps) && anyNA(steps$domain)) {
    stop("`steps$domain` has NA; give every item a domain.", call. = FALSE)
  }
  if (!is.numeric(anchors$tau) || any(!is.finite(anchors$tau))) {
    stop("`anchors$tau` must be finite numbers.", call. = FALSE)
  }
  anchors$step <- if ("step" %in% names(anchors)) ifelse(is.na(anchors$step), step, anchors$step) else step
  pass <- merge(steps[intersect(c("item_id", "item_score", "thr", "domain"), names(steps))],
                anchors[c("item_id", "step", "tau")], by = "item_id")
  missing_step <- unique(pass$item_id)[!unique(pass$item_id) %in% pass$item_id[pass$item_score == pass$step]]
  if (length(missing_step) > 0) {
    stop("Anchor item(s) without the pass step in `steps`: ", paste(missing_step, collapse = ", "), ".", call. = FALSE)
  }
  an <- pass[pass$item_score == pass$step, ]
  if (nrow(an) < 3) {
    stop(nrow(an), " anchor item(s) found in `steps`; at least 3 are needed.", call. = FALSE)
  }
  link <- mean_sigma_link(an$thr, an$tau, id = an$item_id, cut = cut, n_boot = n_boot, seed = seed)
  link$n_items <- nrow(an)
  link$settings[c("kind", "step", "own_step")] <- list("anchors", step, any(an$step != step))
  if (!"domain" %in% names(an)) {
    return(link)
  }

  link$points$domain <- an$domain
  keep <- !link$points$dropped
  shared <- mean_sigma_shared_slope(an$thr[keep], an$tau[keep], an$domain[keep])
  domains <- sort(unique(an$domain))
  link$domains <- preview_table(data.frame(domain = domains, n = as.integer(table(factor(an$domain[keep], levels = domains))),
                                        intercept = unname(shared$intercept[domains]),
                                        slope = shared$slope))
  lost <- link$domains$domain[link$domains$n == 0]
  if (length(lost) > 0) {
    warning("All anchors of domain(s) ", paste(lost, collapse = ", "), " were dropped by the screening.", call. = FALSE)
  }
  link
}
