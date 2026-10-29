# 連結一步一步（和 kits 的 kits_link_screen()、kits_link_line()、kits_link_error() 同名）：
# 篩一次、直線、連結誤差。kits_linking()、kits_linking_d() 都走這三步（mean_sigma_link()）。
# 連結的直線：新值 = intercept + slope x 舊值（0.22.0 叫 a、b；Kolen & Brennan 的 B、A）。

#' A link step by step: screening, line, linking error
#'
#' A mean/sigma link is made in three steps, and each step is a function, so that a link can be
#' followed one step at a time, or made from points prepared elsewhere:
#' ```
#' link <- points |>
#'   kits_link_screen() |>          # 1 one round of screening
#'   kits_link_line() |>            # 2 the mean/sigma line
#'   kits_link_error(n_boot = 200)  # 3 the linking error
#' ```
#' [kits_linking()] and [kits_linking_d()] run the same three steps on the thresholds of the common
#' items or of the anchors. The points are a data.frame with one row per point: `id` (its name, for
#' example `"lana01:2"`), `item` (the item it belongs to, for the resampling), `from` (the value on
#' the scale to be converted) and `to` (the value on the target scale).
#'
#' 1. `kits_link_screen()`: fits the line through all points (`intercept0`, `slope0`), computes
#'    each residual e = to - (intercept0 + slope0 x from), and drops the points with |e| beyond
#'    `cut` x SD(e).
#'    The screening is done once, not repeated.
#'    When the residuals are almost all 0 (the points lie exactly on a line), no point is dropped.
#' 2. `kits_link_line()`: the mean/sigma line through the kept points, to = intercept + slope x
#'    from, with slope = SD(to) / SD(from) (the ratio of the two SDs) and intercept = mean(to) -
#'    slope x mean(from) (it aligns the two means). Kolen and Brennan (2014) call the slope A and
#'    the intercept B. The result is a link without its linking error.
#' 3. `kits_link_error()`: resamples whole items `n_boot` times and redoes steps 1 and 2 each time;
#'    the SD of the intercept and of the slope over the resamples is the linking error
#'    (`intercept_error`, `slope_error`).
#'    When an item has several points (each step of a polytomous item), the points are not
#'    independent, so the whole item is resampled together (`item`).
#'    Resamples that cannot be computed (for example, the same item drawn every time) are left out
#'    and counted in `n_failed`. `n_boot = 0` removes the linking error.
#'
#' Two things to watch:
#' * With few points, a deviating point inflates the residual SD, so it may not be dropped itself.
#'   For example, with 10 anchor items and one off by 12 D, that item is not dropped; with 15 items
#'   it is. So also look at the residuals in `points`.
#' * When the points of one item shift together, often the slope is pulled away, instead of that
#'   item being dropped.
#'
#' The three steps give the same link as `kits_mean_sigma(from, to, id, item, cut, n_boot, seed)`
#' of kitsPreview 0.20.0 (removed in 0.23.0). No other package is used: the line is closed-form base R,
#' so [kits_code()] writes it out in full. kits has the same three functions, with more screening
#' rules (`spread`, `unit`) and a first step `kits_link_points()`; here the screening is the rule of
#' kits' default (`spread = "sd"`, `unit = "step"`).
#'
#' @param points A data.frame with `id`, `item`, `from`, `to`: one row per point.
#' @param cut The cutoff for dropping, in residual SDs (a positive number). At least 3 points must
#'   be kept.
#' @param from_scale,to_scale Optional labels of the two scales of the points, for example
#'   `"lan M36"` and `"lan M03"`; the link keeps them and prints them.
#' @param screened A `kits_link_screen()` result.
#' @param link A link: a `kits_link_line()` result (or any [kits_linking()] result, whose linking
#'   error is then computed again).
#' @param n_boot Number of item resamples (at least 2); 0 removes the linking error.
#'   200 is enough to see its size; use more (e.g. 2000) for reporting.
#' @param seed Random seed for the resampling (or `NULL` for a run that cannot be repeated).
#'   Your current random state is left unchanged.
#' @return
#' * `kits_link_screen()`: a `kitsPreview_link_screen` (a data.frame): the points with `resid` (e)
#'   and `dropped`. `attr(, "link")` keeps `cut`, `intercept0`, `slope0` and the scales.
#' * `kits_link_line()`: a `kitsPreview_link`, a list that prints briefly
#'   ([print.kitsPreview_link()]). Its elements:
#'   * `intercept`, `slope`: the line after screening (new value = intercept + slope x old value);
#'     also `coef(link)`.
#'   * `intercept_error`, `slope_error`, `n_failed`: the linking error (NA until
#'     `kits_link_error()`).
#'   * `intercept0`, `slope0`: the line before screening.
#'   * `n`: the number of points.
#'   * `dropped`: the dropped points.
#'   * `points`: for each point, `id`, `item`, `from`, `to`, the residuals from the first line
#'     (`resid`) and from the final line (`resid_final`), and whether it was dropped.
#'   * `settings`: `cut`, `n_boot`, `seed` (used by [kits_code()]).
#'   * `from_scale`, `to_scale`: when given to `kits_link_screen()`.
#' * `kits_link_error()`: the link with `intercept_error`, `slope_error`, `n_failed`, and
#'   `settings$n_boot`, `settings$seed`.
#' @references
#' Marco, G. L. (1977). Item characteristic curve solutions to three intractable testing problems.
#' *Journal of Educational Measurement, 14*(2), 139-160. \doi{10.1111/j.1745-3984.1977.tb00033.x}
#' @examples
#' # Your own points: point p8 is shifted and dropped
#' points <- data.frame(id = paste0("p", 1:8), item = paste0("p", 1:8),
#'                      from = c(-2, -1, 0, 1, 2, 3, 0.5, 1.5),
#'                      to = c(-1, 0, 1, 2, 3, 4, 1.5, 5.5))
#'
#' # 1 one round of screening
#' screened <- kits_link_screen(points, from_scale = "x", to_scale = "y")
#' screened
#'
#' # 2 the line
#' link <- kits_link_line(screened)
#' link
#'
#' # 3 the linking error
#' link <- kits_link_error(link, n_boot = 100)
#' link$intercept_error
#' coef(link)
#' @name kits_link_steps
NULL

#' @rdname kits_link_steps
#' @export
kits_link_screen <- function(points, cut = 2, from_scale = NULL, to_scale = NULL) {
  need_cols(points, c("id", "item", "from", "to"), "points")
  need_points(points$from, points$to, points$id, points$item)
  need_cut(cut)
  warn_negative(points$from, points$to)
  screened <- screen_step(points$from, points$to, points$id, points$item, cut)
  attr(screened, "link")$from_scale <- from_scale
  attr(screened, "link")$to_scale <- to_scale
  screened
}

#' @rdname kits_link_steps
#' @export
kits_link_line <- function(screened) {
  if (!inherits(screened, "kitsPreview_link_screen") || is.null(attr(screened, "link"))) {
    text <- paste0("`screened` must be a kits_link_screen() result, ",
                   "e.g. kits_link_line(kits_link_screen(points)).")
    stop_preview(text, "kitsPreview_error_input")
  }
  line_step(screened)
}

#' @rdname kits_link_steps
#' @export
kits_link_error <- function(link, n_boot = 200, seed = 1) {
  if (!inherits(link, "kitsPreview_link")) {
    stop_preview("`link` must be a link: a kits_link_line() or kits_linking() result.",
                 "kitsPreview_error_input")
  }
  need_seed(seed)
  need_n_boot(n_boot)
  error_step(link, n_boot, seed)
}


# ---- 檢查 ---------------------------------------------------------------------------------------

need_seed <- function(seed) {
  bad <- !is.null(seed) &&
    (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || abs(seed) > .Machine$integer.max)
  if (bad) {
    stop("`seed` must be one whole number (or NULL for a run that cannot be repeated).",
         call. = FALSE)
  }
}

# 點：from、to、id、item 一樣長，數字，沒有 NA，至少 3 個，SD 算得出來而且不是 0
need_points <- function(from, to, id, item) {
  if (length(from) != length(to) || length(from) != length(id) || length(from) != length(item)) {
    stop("`from`, `to`, `id` and `item` must have the same length.", call. = FALSE)
  }
  if (!is.numeric(from) || !is.numeric(to)) {
    stop("`from` and `to` must be numbers.", call. = FALSE)
  }
  if (anyNA(from) || anyNA(to) || any(!is.finite(c(from, to)))) {
    stop("`from` and `to` must not contain NA or infinite values.", call. = FALSE)
  }
  if (length(from) < 3) {
    stop("At least 3 points are needed; there are ", length(from), ".", call. = FALSE)
  }
  if (!is.finite(stats::sd(from)) || !is.finite(stats::sd(to))) {
    stop("`from` and `to` are too large to compute their SD.", call. = FALSE)
  }
  if (stats::sd(from) == 0 || stats::sd(to) == 0) {
    stop("All values of `from` (or of `to`) are the same, so no line can be fitted.", call. = FALSE)
  }
}

need_cut <- function(cut) {
  if (!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut <= 0) {
    stop("`cut` must be one positive number (in residual SDs).", call. = FALSE)
  }
}

need_n_boot <- function(n_boot) {
  if (!is.numeric(n_boot) || length(n_boot) != 1 || is.na(n_boot) || n_boot < 0 || n_boot == 1) {
    stop("`n_boot` must be 0 (no linking error) or a number of resamples of at least 2.",
         call. = FALSE)
  }
}

# from 和 to 負相關：mean/sigma 的斜率一定是正的，點可能沒有對好
warn_negative <- function(from, to) {
  if (stats::cor(from, to) < 0) {
    warning("`from` and `to` are negatively related (r = ", round(stats::cor(from, to), 2),
            "). mean/sigma always gives a positive slope; check that the points are matched.",
            call. = FALSE)
  }
}


# ---- 三步 ---------------------------------------------------------------------------------------

# 1 篩一次：所有點的直線（intercept0、slope0），殘差，|殘差| > cut x SD 的點丟掉
screen_step <- function(from, to, id, item, cut) {
  first <- mean_sigma(from, to)
  resid <- to - first$intercept - first$slope * from
  dropped <- abs(resid) > cut * stats::sd(resid)
  if (stats::sd(resid) < 1e-8) {
    dropped[] <- FALSE
  }
  if (sum(!dropped) < 3) {
    stop("The screening would keep only ", sum(!dropped),
         " point(s); at least 3 are needed. Use a larger `cut`.",
         call. = FALSE)
  }
  out <- data.frame(id = id, item = item, from = from, to = to, resid = resid, dropped = dropped)
  out <- preview_table(out, "kitsPreview_link_screen")
  attr(out, "link") <- list(cut = cut, intercept0 = first$intercept, slope0 = first$slope)
  out
}

# 2 直線：留下的點的 mean/sigma；連結誤差還沒算（NA）
line_step <- function(screened) {
  st <- attr(screened, "link")
  pts <- as.data.frame(screened)
  kept <- !pts$dropped
  final <- mean_sigma(pts$from[kept], pts$to[kept])
  points <- data.frame(id = pts$id, item = pts$item, from = pts$from, to = pts$to,
                       resid = pts$resid,
                       resid_final = pts$to - final$intercept - final$slope * pts$from,
                       dropped = pts$dropped)
  out <- list(intercept = final$intercept, slope = final$slope, intercept_error = NA_real_,
              slope_error = NA_real_, n_failed = NA_integer_,
              intercept0 = st$intercept0, slope0 = st$slope0, n = nrow(pts),
              dropped = pts$id[pts$dropped],
              points = preview_table(points), settings = list(cut = st$cut, n_boot = 0, seed = 1))
  out$from_scale <- st$from_scale
  out$to_scale <- st$to_scale
  structure(out, class = "kitsPreview_link")
}

# 3 連結誤差：整題重抽 n_boot 次，每次重做第 1、2 步；n_boot = 0 時只記下設定
error_step <- function(link, n_boot, seed) {
  link[c("intercept_error", "slope_error", "n_failed")] <- list(NA_real_, NA_real_, NA_integer_)
  link$settings$n_boot <- n_boot
  link$settings["seed"] <- list(seed)   # seed 可以是 NULL
  pts <- link$points
  if (n_boot > 0 && length(unique(pts$item)) < 2) {
    warning("All points belong to one item, so the items cannot be resampled; no linking error.",
            call. = FALSE)
  } else if (n_boot > 0) {
    boot <- boot_mean_sigma(pts$from, pts$to, pts$id, pts$item, link$settings$cut, n_boot, seed)
    link[c("intercept_error", "slope_error", "n_failed")] <- boot
  }
  link
}

# 三步一次做完（kits_linking()、kits_linking_d()、kits_drift() 用）。
# 檢查的順序和 0.20.0 的 kits_mean_sigma() 相同，所以錯誤訊息也相同
mean_sigma_link <- function(from, to, id, item = id, cut = 2, n_boot = 0, seed = 1) {
  need_seed(seed)
  need_points(from, to, id, item)
  need_cut(cut)
  need_n_boot(n_boot)
  warn_negative(from, to)
  link <- screen_step(from, to, id, item, cut) |>
    line_step()
  link$settings <- list(cut = cut, n_boot = n_boot, seed = seed)
  if (n_boot == 0) {
    return(link)
  }
  error_step(link, n_boot, seed)
}

# 整題重抽 n_boot 次，每次重做篩選和直線
boot_mean_sigma <- function(from, to, id, item, cut, n_boot, seed) {
  items <- unique(item)
  rows <- split(seq_along(item), factor(item, levels = items))
  one_draw <- function() {
    i <- unlist(rows[sample(items, replace = TRUE)], use.names = FALSE)
    fit <- function() suppressWarnings(mean_sigma_link(from[i], to[i], id[i], cut = cut))
    link <- tryCatch(fit(), error = function(e) list(intercept = NA, slope = NA))
    c(intercept = link[["intercept"]], slope = link[["slope"]])
  }
  draws <- with_seed(seed, t(replicate(n_boot, one_draw())))
  ok <- is.finite(draws[, "intercept"]) & is.finite(draws[, "slope"])
  list(intercept_error = stats::sd(draws[ok, "intercept"]), slope_error = stats::sd(draws[ok, "slope"]),
       n_failed = sum(!ok))
}
