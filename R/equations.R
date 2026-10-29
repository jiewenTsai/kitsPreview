# kits_equations()：把每一步的公式寫出來給使用者抄（Unicode 或 LaTeX）。
# 版面和 birtRcpp 的 equations() 相同：幾段，每段幾行（式子、註解）。
# 每一條式子都在 tests/testthat/test-equations.R 用數字驗證過：照式子算，結果和套件相同。
# 原始碼只用 ASCII；Unicode 字元用 \u 寫。

# 一行：Unicode 版、LaTeX 版、註解
eq_line <- function(u, l, note = "") list(u = u, l = l, note = note)

# 一段：標題、幾行、引用（eq_refs() 的鍵）
eq_section <- function(title, ..., ref = character()) list(title = title, lines = list(...), ref = ref)

# 方法和套件的引用：short 印在段落標題後面，full 印在最後的 References
eq_refs <- function() list(
  masters1982 = list(short = "Masters, 1982", full = "Masters, G. N. (1982). A Rasch model for partial credit scoring. Psychometrika, 47(2), 149-174. doi:10.1007/BF02296272"),
  dexter = list(short = "dexter: Maris et al., 2026", full = "Maris, G., Bechger, T., Koops, J., & Partchev, I. (2026). dexter: Data management and analysis of tests (R package version 1.8.1). https://dexter-psychometrics.github.io/dexter/"),
  adams2012 = list(short = "Adams, Wu & Wilson, 2012", full = "Adams, R. J., Wu, M. L., & Wilson, M. (2012). The Rasch rating model and the disordered threshold controversy. Educational and Psychological Measurement, 72(4), 547-573. doi:10.1177/0013164411432166"),
  warm1989 = list(short = "Warm, 1989", full = "Warm, T. A. (1989). Weighted likelihood estimation of ability in item response theory. Psychometrika, 54(3), 427-450. doi:10.1007/BF02294627"),
  firth1993 = list(short = "Firth, 1993", full = "Firth, D. (1993). Bias reduction of maximum likelihood estimates. Biometrika, 80(1), 27-38. doi:10.1093/biomet/80.1.27"),
  kolen2014 = list(short = "Kolen & Brennan, 2014", full = "Kolen, M. J., & Brennan, R. L. (2014). Test equating, scaling, and linking (3rd ed.). Springer. doi:10.1007/978-1-4939-0317-7"),
  wright1982 = list(short = "Wright & Masters, 1982", full = "Wright, B. D., & Masters, G. N. (1982). Rating scale analysis. MESA Press."),
  linacre2002 = list(short = "Linacre, 2002", full = "Linacre, J. M. (2002). What do infit and outfit, mean-square and standardized mean? Rasch Measurement Transactions, 16(2), 878."),
  marco1977 = list(short = "Marco, 1977", full = "Marco, G. L. (1977). Item characteristic curve solutions to three intractable testing problems. Journal of Educational Measurement, 14(2), 139-160. doi:10.1111/j.1745-3984.1977.tb00033.x"),
  weber2019 = list(short = "GSED D-score: Weber et al., 2019", full = "Weber, A. M., Rubio-Codina, M., Walker, S. P., van Buuren, S., Eekhout, I., et al. (2019). The D-score: a metric for interpreting the early development of infants and toddlers across global settings. BMJ Global Health, 4(6), e001724. doi:10.1136/bmjgh-2019-001724"),
  jones2009 = list(short = "Jones & Pewsey, 2009", full = "Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. Biometrika, 96(4), 761-780. doi:10.1093/biomet/asp053"),
  lenhard2021 = list(short = "Lenhard & Lenhard, 2021", full = "Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming. Educational and Psychological Measurement, 81(2), 229-261. doi:10.1177/0013164420928457"),
  cnorm = list(short = "cNORM: Lenhard, Lenhard & Gary, 2018", full = "Lenhard, A., Lenhard, W., & Gary, S. (2018). Continuous norming (cNORM). The Comprehensive R Archive Network. https://CRAN.R-project.org/package=cNORM"),
  kits = list(short = "this package", full = "kits: the rule of this package; see the vignette.")
)

eq_topics <- function() c("item", "subj", "fit", "link", "norms")

# 每個主題的式子
eq_text <- function(topic) {
  th <- "\u03b8"
  be <- "\u03b2"
  switch(topic,
    item = list(
      eq_section("Item calibration: partial credit model, CML in dexter (its ENORM with scores 0, 1, ..., m_i is the PCM)",
        eq_line(sprintf("P(X_pi = k | %s_p) = exp(k%s_p \u2212 \u03a3_{g\u2264k} %s_ig) / \u03a3_{h=0..m_i} exp(h%s_p \u2212 \u03a3_{g\u2264h} %s_ig)", th, th, be, th, be),
                "P(X_{pi} = k \\mid \\theta_p) = \\frac{\\exp(k\\theta_p - \\sum_{g=1}^{k}\\beta_{ig})}{\\sum_{h=0}^{m_i}\\exp(h\\theta_p - \\sum_{g=1}^{h}\\beta_{ig})}",
                "k = 0, ..., m_i; beta_ig = step parameter (column beta)"),
        eq_line(sprintf("\u03a3_i \u03a3_g %s_ig = 0", be), "\\sum_i \\sum_g \\beta_{ig} = 0",
                "origin of each calibration (dexter); so cohorts need a link"),
        ref = c("masters1982", "dexter")),
      eq_section("Thurstonian threshold of step k (column thr)",
        eq_line(sprintf("P(X_i \u2265 k | %s = thr_ik) = 0.5", th), "P(X_i \\ge k \\mid \\theta = \\mathit{thr}_{ik}) = 0.5",
                paste0("thresholds of an item always increase; k = 1 is the milestone step; written thr, ",
                       "not tau: tau is the GSED difficulty of an anchor (as in kits)")),
        eq_line(sprintf("SE(thr_ik) = \u221a(J\u1d40 V_i J),  J_g = \u2212(\u2202P/\u2202%s_ig) / (\u2202P/\u2202%s)", be, th),
                paste0("\\mathrm{SE}(\\mathit{thr}_{ik}) = \\sqrt{J^\\top V_i J},\\quad J_g = -\\frac{\\partial ",
                       "P/\\partial\\beta_{ig}}{\\partial P/\\partial\\theta}"),
                "delta method; P = P(X_i is at least k), at theta = thr_ik; V_i = covariance of the betas of item i (dexter)"),
        ref = "adams2012")),
    subj = list(
      eq_section("Person ability: WLE",
        eq_line(sprintf("%s\u0302_p = argmax_%s [ log L_p(%s) + \u00bd log I_p(%s) ]", th, th, th, th),
                "\\hat\\theta_p = \\arg\\max_\\theta \\left[\\log L_p(\\theta) + \\tfrac12 \\log I_p(\\theta)\\right]",
                "the same as the posterior mode under the Jeffreys prior"),
        eq_line(sprintf("L_p(%s) = \u03a0_i P(X_pi = x_pi | %s),  I_p(%s) = \u03a3_i [\u03a3_k k\u00b2 P_ik \u2212 (\u03a3_k k P_ik)\u00b2]", th, th, th),
                "L_p(\\theta) = \\prod_i P(X_{pi} = x_{pi} \\mid \\theta),\\quad I_p(\\theta) = \\sum_i \\Big[\\sum_k k^2 P_{ik} - \\big(\\sum_k k P_{ik}\\big)^2\\Big]",
                "sums over the items person p answered"),
        eq_line(sprintf("SE(%s\u0302_p) = \u221a(I + (I\u2032 / 2I)\u00b2) / I", th),
                "\\mathrm{SE}(\\hat\\theta_p) = \\frac{\\sqrt{I + \\left(I' / 2I\\right)^2}}{I}",
                "I and its derivative I' at theta-hat"),
        ref = c("warm1989", "firth1993", "dexter")),
      eq_section("Linear conversions (each link: new = intercept + slope x old)",
        eq_line(sprintf("%s* = B + A %s,  SE* = A SE", th, th),
                "\\theta^* = B + A\\,\\theta,\\quad \\mathrm{SE}^* = A\\,\\mathrm{SE}",
                paste0("cohort link to the reference scale, then the D link; B = intercept, A = slope ",
                       "(intercept and slope are B and A of Kolen & Brennan, 2014)")),
        ref = "kolen2014")),
    fit = list(
      eq_section("Residuals (at the WLE; persons with a zero or a perfect score left out)",
        eq_line(sprintf("E_pi = \u03a3_k k P_ik(%s\u0302_p),  W_pi = \u03a3_k k\u00b2 P_ik(%s\u0302_p) \u2212 E_pi\u00b2", th, th),
                "E_{pi} = \\sum_k k\\,P_{ik}(\\hat\\theta_p),\\quad W_{pi} = \\sum_k k^2 P_{ik}(\\hat\\theta_p) - E_{pi}^2"),
        ref = "wright1982"),
      eq_section("Mean squares",
        eq_line("infit = \u03a3 (x \u2212 E)\u00b2 / \u03a3 W", "\\text{infit} = \\frac{\\sum (x - E)^2}{\\sum W}",
                "sums over the items of a person (person fit) or over the persons of an item (item fit)"),
        eq_line("outfit = (1/n) \u03a3 (x \u2212 E)\u00b2 / W", "\\text{outfit} = \\frac{1}{n}\\sum \\frac{(x - E)^2}{W}", "n = number of terms"),
        eq_line("flag = misfit if max(infit, outfit) > 2", "\\text{misfit} \\iff \\max(\\text{infit}, \\text{outfit}) > 2",
                "Linacre: above 2 distorts or degrades the measurement"),
        eq_line("rir = cor(x_i, \u03a3_{j\u2260i} x_j)", "r_{ir} = \\operatorname{cor}\\big(x_i, \\textstyle\\sum_{j \\ne i} x_j\\big)",
                paste0("item-rest correlation per item pattern (as rir of dexter tia_tables), weighted ",
                       "mean over item patterns")),
        ref = c("wright1982", "linacre2002", "dexter"))),
    link = list(
      eq_section("mean/sigma line with one round of screening",
        eq_line("A = SD(to) / SD(from),  B = mean(to) \u2212 A mean(from)",
                paste0("A = \\frac{\\mathrm{SD}(\\mathit{to})}{\\mathrm{SD}(\\mathit{from})},",
                       "\\quad B = \\overline{\\mathit{to}} - A\\,\\overline{\\mathit{from}}"),
                paste0("A = slope, B = intercept (intercept and slope are B and A of Kolen & Brennan, 2014); ",
                       "first with all points (intercept0, slope0)")),
        eq_line("e_j = to_j \u2212 (B0 + A0 from_j);  drop j if |e_j| > cut SD(e)",
                paste0("e_j = \\mathit{to}_j - (B_0 + A_0\\,\\mathit{from}_j);\\ \\text{drop } j ",
                       "\\text{ if } |e_j| > \\mathit{cut}\\cdot\\mathrm{SD}(e)"),
                "then the intercept and slope again with the kept points"),
        eq_line("linking error = SD of the intercept (and of the slope) over R resamples of whole items",
                "\\mathrm{SE}_{\\mathrm{link}}(B) = \\mathrm{SD}_{r=1..R}(B^{(r)})",
                "the screening is redone in every resample"),
        eq_line(paste0("cohort link: from, to = thr_ik (every common step);  D link: from = thr_is (pass step s, ",
                       "default 1), to = \u03c4_i, the GSED difficulty (column tau)"),
                paste0("\\text{cohort: } \\mathit{from}, \\mathit{to} = \\mathit{thr}_{ik};\\quad \\text{D: } ",
                       "\\mathit{from} = \\mathit{thr}_{is},\\ \\mathit{to} = \\tau^{\\mathrm{GSED}}_i")),
        ref = c("marco1977", "kolen2014", "weber2019")),
      eq_section("Shared slope A_s with domain intercepts B_d (the domains table)",
        eq_line(paste0("A_s = \u221a( \u03a3 (to \u2212 mean_d(to))\u00b2 / \u03a3 (from \u2212 mean_d(from))\u00b2 ),  ",
                       "B_d = mean_d(to) \u2212 A_s mean_d(from)"),
                paste0("A_s = \\sqrt{\\frac{\\sum (\\mathit{to} - \\overline{\\mathit{to}}_d)^2}{\\sum (\\mathit{from} - ",
                       "\\overline{\\mathit{from}}_d)^2}},\\quad B_d = \\overline{\\mathit{to}}_d - A_s\\,",
                       "\\overline{\\mathit{from}}_d"),
                "kept points; d = domain; columns slope and intercept of the domains table"),
        ref = "kits")),
    norms = list(
      eq_section("Screening and ages",
        eq_line("wave out of the norm if 100 \u00d7 share(D = min D of the wave) \u2265 cut", "\\text{wave out if } 100\\cdot\\Pr(D = \\min D) \\ge \\mathit{cut}",
                "per cohort x wave; default cut = 20"),
        eq_line("ages: from Q_0.01(age | first wave) to Q_0.99(age | last wave), rounded outward to 0.5 month",
                "\\text{ages} = [\\,Q_{.01}(\\text{age}\\mid\\text{first wave}),\\ Q_{.99}(\\text{age}\\mid\\text{last wave})\\,]"),
        ref = "kits"),
      eq_section("Continuous norm: sinh-arcsinh distribution, fitted by cNORM",
        eq_line(sprintf("x = (\u221aage \u2212 m) / s"), "x = (\\sqrt{\\text{age}} - m) / s", "m, s = mean and SD of sqrt(age) in the data"),
        eq_line("\u03bc(x) = \u03a3_j c_j x^j,  log \u03c3(x) = \u03a3_j c_j x^j,  \u03b5(x) = \u03a3_j c_j x^j,  log \u03b4(x) = \u03a3_j c_j x^j",
                "\\mu(x),\\ \\log\\sigma(x),\\ \\varepsilon(x),\\ \\log\\delta(x) \\text{ are polynomials in } x",
                "degrees = c(mu, sigma, epsilon, delta)"),
        eq_line("z = sinh( \u03b4 asinh((D \u2212 \u03bc) / \u03c3) + \u03b5 ),  PR = 100 \u03a6(z)",
                "z = \\sinh\\!\\big(\\delta\\,\\operatorname{asinh}((D - \\mu)/\\sigma) + \\varepsilon\\big),\\quad PR = 100\\,\\Phi(z)",
                "mu, sigma, epsilon, delta at the age (linear interpolation between rows)"),
        eq_line("P_q = \u03bc + \u03c3 sinh( (asinh(\u03a6\u207b\u00b9(q/100)) \u2212 \u03b5) / \u03b4 )",
                "P_q = \\mu + \\sigma \\sinh\\!\\big((\\operatorname{asinh}(\\Phi^{-1}(q/100)) - \\varepsilon)/\\delta\\big)",
                "percentile columns P3, P5, ..."),
        ref = c("jones2009", "lenhard2021", "cnorm")))
  )
}

# 一段的標題加上引用；最後的 References 列全文
eq_title <- function(s) {
  if (length(s$ref) == 0) return(s$title)
  paste0(s$title, " [", paste(vapply(eq_refs()[s$ref], function(r) r$short, ""), collapse = "; "), "]")
}
eq_reference_lines <- function(sections) {
  keys <- unique(unlist(lapply(sections, function(s) s$ref)))
  keys <- keys[keys != "kits"]
  if (length(keys) == 0) return(character())
  c("References:", vapply(eq_refs()[keys], function(r) paste0("  ", r$full), ""))
}


#' The formulas, written out to copy
#'
#' Writes the formulas behind a result: Unicode text for the console,
#' or a LaTeX `aligned` block for a paper (`format = "latex"`).
#' Each section names the method and package it comes from, and the full references are listed at the end
#' (as comment lines in the LaTeX version).
#' The layout follows `equations()` of the birtRcpp package.
#' For a link, the fitted line is added with its numbers.
#'
#' Every formula is checked in the package tests:
#' computing it directly gives the same numbers as the package.
#'
#' @param object A result: [kits_item()], [kits()], [kits_subj()], a link ([kits_linking()], [kits_linking_d()],
#'   [kits_link_line()]), [kits_norm_screen()] or [kits_norms()].
#'   Or one or more topics as text: `"item"`, `"subj"`, `"fit"`, `"link"`, `"norms"`;
#'   when omitted, all of them.
#' @param format `"unicode"` or `"latex"`.
#' @param ... Not used.
#' @return A `kitsPreview_equations` object; it prints the formulas, and `as.character()` gives the lines.
#' @examples
#' kits_equations("item")
#' kits_equations("link", format = "latex")
#'
#' points <- data.frame(id = paste0("p", 1:5), item = paste0("p", 1:5),
#'                      from = c(-2, -1, 0, 1, 2), to = c(-1, 0, 1, 2, 3.2))
#' link <- kits_link_line(kits_link_screen(points))
#' kits_equations(link)
#' @export
kits_equations <- function(object, format = c("unicode", "latex"), ...) {
  if (missing(object)) {
    return(eq_object(eq_topics(), format))
  }
  UseMethod("kits_equations")
}

eq_object <- function(topics, format, extra = list()) {
  if (!is.character(format) || length(format) < 1 || length(format) > 2) {
    stop("`format` must be \"unicode\" or \"latex\".", call. = FALSE)
  }
  format <- match.arg(format[1], c("unicode", "latex"))
  sections <- unlist(lapply(topics, eq_text), recursive = FALSE)
  structure(list(sections = c(sections, extra), format = format), class = "kitsPreview_equations")
}

#' @export
kits_equations.character <- function(object, format = c("unicode", "latex"), ...) {
  if (length(object) == 0 || anyNA(object)) {
    stop("Give at least one topic: ", paste(eq_topics(), collapse = ", "), ".", call. = FALSE)
  }
  bad <- setdiff(object, eq_topics())
  if (length(bad) > 0) {
    stop("Unknown topic(s): ", paste(bad, collapse = ", "), ". Use: ", paste(eq_topics(), collapse = ", "), ".", call. = FALSE)
  }
  eq_object(object, format)
}

#' @export
kits_equations.kitsPreview_item <- function(object, format = c("unicode", "latex"), ...) eq_object("item", format)

#' @export
kits_equations.kitsPreview_fit <- function(object, format = c("unicode", "latex"), ...) eq_object(c("item", "subj", "fit"), format)

#' @export
kits_equations.kitsPreview_subj <- function(object, format = c("unicode", "latex"), ...) eq_object(c("subj", "fit"), format)

#' @export
kits_equations.kitsPreview_norms <- function(object, format = c("unicode", "latex"), ...) eq_object("norms", format)

#' @export
kits_equations.kitsPreview_norm_screen <- function(object, format = c("unicode", "latex"), ...) eq_object("norms", format)

#' @rdname kits_equations
#' @param digits For a link: decimals of the numbers in the fitted line.
#' @export
kits_equations.kitsPreview_link <- function(object, format = c("unicode", "latex"), digits = 2, ...) {
  need_digits(digits)
  one <- function(v) length(v) == 1 && is.numeric(v) && !is.na(v)
  if (!one(object$intercept) || !one(object$slope)) {
    stop("This link has no `intercept` and `slope`.", call. = FALSE)
  }
  intercept <- fmt(object$intercept, digits)
  slope <- fmt(object$slope, digits)
  n <- if (length(object$n) == 1) object$n else NA_integer_
  err <- if (one(object$intercept_error) && one(object$slope_error)) {
    sprintf("; linking error intercept %s, slope %s", fmt(object$intercept_error, digits),
            fmt(object$slope_error, digits))
  } else {
    ""
  }
  note <- sprintf("%d points, %d dropped%s", n, length(object$dropped), err)
  fitted <- eq_section("This link (new = intercept + slope x old)",
    eq_line(sprintf("new = %s + %s \u00d7 old", intercept, slope),
            sprintf("\\text{new} = %s + %s \\times \\text{old}", intercept, slope), note))
  eq_object("link", format, list(fitted))
}

#' @export
kits_equations.default <- function(object, format = c("unicode", "latex"), ...) {
  stop("No formulas for an object of class ", class(object)[1], ". Use a kitsPreview result or a topic: ",
       paste(eq_topics(), collapse = ", "), ".", call. = FALSE)
}


#' @rdname kits_equations
#' @param x A `kitsPreview_equations` object.
#' @export
as.character.kitsPreview_equations <- function(x, ...) {
  if (x$format == "unicode") {
    out <- character()
    for (s in x$sections) {
      out <- c(out, if (length(out) > 0) "", paste0(eq_title(s), ":"))
      for (l in s$lines) {
        out <- c(out, paste0("  ", l$u), if (nzchar(l$note)) paste0("      (", l$note, ")"))
      }
    }
    refs <- eq_reference_lines(x$sections)
    return(c(out, if (length(refs) > 0) c("", refs)))
  }
  # 註解放在 \text{} 裡：符號（m_i、beta_ig）改成行內數學，其他特殊字元加反斜線
  tex_note <- function(n) {
    n <- gsub("([&%#{}])", "\\\\\\1", n)
    greek <- c("beta", "tau", "theta", "mu", "sigma", "epsilon", "delta")
    m <- gregexpr("[A-Za-z]+_[A-Za-z0-9]+", n)
    regmatches(n, m) <- lapply(regmatches(n, m), function(tok) {
      name <- sub("_.*$", "", tok)
      sub_ <- sub("^.*_", "", tok)
      name <- ifelse(name %in% greek, paste0("\\", name), name)
      paste0("$", name, "_{", sub_, "}$")
    })
    n
  }
  body <- unlist(lapply(x$sections, function(s) {
    c(sprintf("  & \\text{%s:} \\\\", tex_note(eq_title(s))),
      vapply(s$lines, function(l) sprintf("  & \\quad %s%s \\\\", l$l, if (nzchar(l$note)) sprintf(" \\quad \\text{(%s)}", tex_note(l$note)) else ""), ""))
  }))
  body[length(body)] <- sub(" \\\\\\\\$", "", body[length(body)])
  refs <- eq_reference_lines(x$sections)
  c("\\begin{aligned}", body, "\\end{aligned}", if (length(refs) > 0) paste0("% ", refs))
}

#' @rdname kits_equations
#' @export
print.kitsPreview_equations <- function(x, ...) {
  cat(as.character(x), sep = "\n")
  invisible(x)
}
