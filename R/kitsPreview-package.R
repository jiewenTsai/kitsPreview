#' kitsPreview: teaching wrappers around dexter, immer and cNORM
#'
#' kitsPreview shows how each step of the KIT developmental scale is done directly with
#' dexter, immer and cNORM. The wrappers are thin and simple: each one runs a few lines of the original package,
#' converts the result to one common table, and keeps the package's own result.
#' The aim is teaching: a reader should learn how to do each step directly with the original package.
#' The optimized package is kits (written in RTMB); kitsPreview also checks its numbers.
#'
#' Every function starts with `kits_` (kit scale), with the same names, main arguments, columns and printout as kits,
#' so the two packages can be read side by side. A few arguments differ (for example the defaults of
#' [kits_linking()], and `engine =` of [kits_item()]); the README lists them.
#' The data are a response matrix, as for `dexter::fit_enorm()`
#' (one row per person-wave, one column per item, NA means not administered),
#' or a long data.frame (`visit_id`, `item_id`, `item_score`).
#'
#' @section Names:
#' kitsPreview uses the names of kits 0.23.0:
#' * `visit_id`: one child at one visit (a person-wave), the row names of the response matrix;
#'   `subj_id`: the child (the same in every wave).
#' * item pattern (`pattern_id`): which items a person-wave answered; the standard form of a wave
#'   is the item pattern of most of its children. The raw score is `raw`, its maximum `raw_max`.
#' * A link: new value = `intercept` + `slope` x old value (Kolen and Brennan, 2014, call the slope
#'   A and the intercept B).
#'
#' dexter calls the person-wave `person_id` and the item pattern a booklet (`booklet_id`,
#' `booklet_score`). kitsPreview gives the data to dexter with dexter's names and returns kits'
#' names; only the original outputs ([kits_original()], `summary()`, `print(x, original = TRUE)`)
#' keep dexter's names, on purpose.
#' Until kitsPreview 0.22.0 the ids were `person_id` and `pid`; data and `persons` now need
#' `visit_id` and `subj_id`.
#'
#' @section Class names:
#' The results have their own class names, which start with `kitsPreview_`
#' (`kitsPreview_item`, `kitsPreview_fit`, `kitsPreview_subj`, `kitsPreview_link`, `kitsPreview_table`,
#' `kitsPreview_norms`, `kitsPreview_norm_screen`, `kitsPreview_subj_table`, `kitsPreview_inter`,
#' `kitsPreview_equations`, `kitsPreview_code`).
#' No class is shared with kits, and the methods are registered only for these classes,
#' so loading both packages in one session does not change any method of kits.
#' The errors have the classes `kitsPreview_error_missing_package`, `kitsPreview_error_engine_unsupported`
#' and `kitsPreview_error_input` (all also `kitsPreview_error`).
#' A result of kits is not a result of kitsPreview: to pass kits' item parameters to kitsPreview, give `coef(item)`.
#'
#' @section Engines:
#' * [kits_item()]: `engine = "dexter"` (the default) or `"immer"`, both by conditional maximum likelihood (CML),
#'   or `"ccml"`, immer's pairwise (composite) CML, the method of the GSED D-score.
#'   All give the same `thresholds` table, with the step parameters on the CML origin
#'   (the sum of all betas is 0), so the packages and methods can be compared directly.
#' * [kits_subj()]: dexter's WLE.
#' * [kits_linking()]: mean/sigma with screening, in base R.
#' * The calibration, scoring and norm results keep the package's own object: [kits_original()] accepts
#'   the results of [kits_item()], [kits()], [kits_subj()] and [kits_norms()] (cNORM's `cnorm.shash()`
#'   fit). `summary()` prints the tidy summary, then the package's own output;
#'   it exists only for `kitsPreview_item` and `kitsPreview_subj`.
#'   DIF and [kits_inter()] keep no original object, nor do the links (base R).
#' * [kits_code()] writes the few lines of the original package's code, then the conversion.
#' * immer is optional (Suggests). When it is missing, the error says how to install it.
#' * Engine-free: the norms (cNORM), DIF and the interaction model (dexter), the D link, the drift check,
#'   item and person fit, and the score tables.
#' * Marginal maximum likelihood (TAM) is not an engine here; see the help of [kits_item()] for why CML needs
#'   no population model.
#'
#' See `vignette("engines", package = "kitsPreview")` for the three engines on one data set.
#'
#' @section Main workflow (tier 1):
#' 1. Calibrate each cohort x domain: [kits_item()] (the PCM, by default with dexter, plus the threshold of every step).
#' 2. Cohort link (for example M36 -> M03), with the thresholds of the common items: [kits_linking()].
#' 3. Link to D, with the anchor items against the GSED difficulties: [kits_linking_d()].
#'    Several domains can share one line.
#' 4. Person scores: [kits_subj()] (dexter's WLE; it can always be computed), with the waves and
#'    ages of the person-waves in `persons`. Then theta on the reference scale =
#'    `intercept + slope * theta` with the cohort link, and D = `intercept + slope * theta` with
#'    the D link.
#'    Without links (one cohort), [kits()] calibrates and scores in one step; `plot()` gives a Wright map.
#'    Raw-score to ability table of each wave: `s |> kits_subj_table()`.
#' 5. Norms: screen the waves [kits_norm_screen()], set the age range [kits_norm_ages()],
#'    fit the continuous norm [kits_norms()] (cNORM shash; `by =` one norm per group; `plot()` gives
#'    the percentile curves), and get the same-age z [kits_norm_z()].
#' 6. Checks: item fit (kept as `item$itemfit` by [kits_item()]; [kits_itemfit()] for other data)
#'    and dexter's interaction model per wave [kits_inter()];
#'    person fit (`infit`, `outfit` of [kits_subj()]); `plot()` of a link and of a norm.
#'
#' @section Helpers (tier 2):
#' * A link step by step, or from your own points: [kits_link_steps].
#' * Raw-score table per item pattern: `kits_subj_table(item)`.
#' * A new wave of data: [kits_drift()]; then `kits_item(X_new, fixed_params = drift$fix)`.
#' * Items: [kits_age_pass()], [kits_dif()], [kits_dif_combine()], [kits_item_table()] (and its
#'   `plot()`: the growth curves with the items and the developmental ladder).
#' * Waves of two cohorts on one scale: [kits_vertical()] (and its `plot()`).
#' * Saving plots: [kits_save_plot()].
#' * Simulated data for examples: [kits_sim()].
#'
#' Printed results (tables, `coef()`, links) show 2 decimals; change it with `print(x, digits = 4)`.
#' Only the printout is rounded: the numbers in the objects keep their full precision.
#'
#' Plot text (titles, axes, legends) is in English. Item stems can be in any language, for example Chinese.
#' To change a title: `plotly::layout(p, title = "...")`.
#'
#' Every link is a straight line: new value = intercept + slope x old value (and new SE = slope x old SE).
#'
#' For a full example, see `vignette("kits-flow", package = "kitsPreview")`.
#'
#' To cite kitsPreview, see `citation("kitsPreview")`.
#' The methods come from the references below; please cite them with kitsPreview.
#' The formulas of every step: [kits_equations()]. The code: [kits_code()].
#'
#' @references
#' Adams, R. J., Wu, M. L., & Wilson, M. (2012). The Rasch rating model and the disordered threshold controversy.
#' *Educational and Psychological Measurement, 72*(4), 547-573. \doi{10.1177/0013164411432166}
#'
#' Bechger, T. M., & Maris, G. (2015). A statistical test for differential item pair functioning.
#' *Psychometrika, 80*(2), 317-340. \doi{10.1007/s11336-014-9408-y}
#'
#' Firth, D. (1993). Bias reduction of maximum likelihood estimates. *Biometrika, 80*(1), 27-38.
#' \doi{10.1093/biomet/80.1.27}
#'
#' Haberman, S. J. (2007). The interaction model. In M. von Davier & C. H. Carstensen (Eds.),
#' *Multivariate and mixture distribution Rasch models* (pp. 201-216). Springer. \doi{10.1007/978-0-387-49839-3_13}
#'
#' Jones, M. C., & Pewsey, A. (2009). Sinh-arcsinh distributions. *Biometrika, 96*(4), 761-780.
#' \doi{10.1093/biomet/asp053}
#'
#' Kolen, M. J., & Brennan, R. L. (2014). *Test equating, scaling, and linking* (3rd ed.). Springer.
#' \doi{10.1007/978-1-4939-0317-7}
#'
#' Lenhard, W., & Lenhard, A. (2021). Improvement of norm score quality via regression-based continuous norming.
#' *Educational and Psychological Measurement, 81*(2), 229-261. \doi{10.1177/0013164420928457}
#'
#' Linacre, J. M. (2002). What do infit and outfit, mean-square and standardized mean?
#' *Rasch Measurement Transactions, 16*(2), 878.
#'
#' Marco, G. L. (1977). Item characteristic curve solutions to three intractable testing problems.
#' *Journal of Educational Measurement, 14*(2), 139-160. \doi{10.1111/j.1745-3984.1977.tb00033.x}
#'
#' Maris, G., Bechger, T., Koops, J., & Partchev, I. (2026). *dexter: Data management and analysis of tests*
#' (R package version 1.8.1). \url{https://dexter-psychometrics.github.io/dexter/}
#'
#' Masters, G. N. (1982). A Rasch model for partial credit scoring. *Psychometrika, 47*(2), 149-174.
#' \doi{10.1007/BF02296272}
#'
#' Warm, T. A. (1989). Weighted likelihood estimation of ability in item response theory.
#' *Psychometrika, 54*(3), 427-450. \doi{10.1007/BF02294627}
#'
#' Weber, A. M., Rubio-Codina, M., Walker, S. P., van Buuren, S., Eekhout, I., et al. (2019).
#' The D-score: a metric for interpreting the early development of infants and toddlers across global settings.
#' *BMJ Global Health, 4*(6), e001724. \doi{10.1136/bmjgh-2019-001724}
#'
#' Wright, B. D., & Masters, G. N. (1982). *Rating scale analysis*. MESA Press.
#'
#' @keywords internal
"_PACKAGE"

# dplyr 的欄名（R CMD check 不知道它們是欄）
utils::globalVariables(c("D", "DIF", "P25", "P50", "age_m", "age_med", "cohort", "column", "domain",
                         "group", "infit", "item_id", "item_score", "outfit", "sex", "stem", "theta",
                         "thr", "wave_m", "x"))
