#' Define a CPM specification
#'
#' Create a lightweight specification object that stores the staged decisions
#' required to run connectome-based predictive modeling later with [cpm()].
#'
#' `spec()` keeps the main CPM decisions visible at the top level while
#' grouping stage-specific settings into helpers.
#'
#' `spec()` groups CPM settings into three stages:
#'
#' - [cpm_selection_cor()] configures the current correlation-based edge
#'   selection path.
#' - [cpm_construction_summary()] configures the current summary-construction
#'   path.
#' - [cpm_model_lm()] defines the outcome model fitted on CPM-derived features.
#'
#' @param selection Selection helper created by [cpm_selection_cor()].
#' @param construction Construction helper created by
#'   [cpm_construction_summary()].
#' @param model Outcome-model helper created by [cpm_model_lm()]. This stage
#'   maps CPM-derived subject-level features to the behavioral outcome.
#'
#' @examples
#' s <- spec(
#'   selection = cpm_selection_cor(
#'     method = "spearman",
#'     criterion = "absolute",
#'     level = 0.1
#'   ),
#'   construction = cpm_construction_summary(
#'     sign_mode = "net",
#'     weight_scale = 0.03
#'   ),
#'   model = cpm_model_lm()
#' )
#' s
#'
#' conmat <- matrix(rnorm(100 * 100), nrow = 100)
#' behav <- rnorm(100)
#' result <- cpm(
#'   conmat = conmat,
#'   behav = behav,
#'   spec = s,
#'   resamples = 5
#' )
#' summary(result)
#' @export
spec <- function(
  selection = cpm_selection_cor(),
  construction = cpm_construction_summary(),
  model = cpm_model_lm()
) {
  assert_spec_class(selection, "selection", "cpm_selection_spec")
  assert_spec_class(construction, "construction", "cpm_construction_spec")
  assert_spec_class(model, "model", "cpm_model_spec")

  structure(
    list(
      selection = selection,
      construction = construction,
      model = model
    ),
    class = "cpm_spec"
  )
}

#' @export
print.cpm_spec <- function(x, ...) {
  cat("CPM specification:\n")
  print_staged_settings(
    selection = x$selection,
    construction = x$construction,
    model = x$model,
    headers = list(
      selection = "  Selection:\n",
      construction = "  Construction:\n",
      model = "  Model:\n"
    )
  )
  invisible(x)
}

format_threshold_level <- function(x) {
  trimws(formatC(x, format = "fg", digits = 3))
}

print_setting_line <- function(label, value, indent = "    ", width = 22L) {
  cat(sprintf(
    "%s%-*s %s\n",
    indent,
    width,
    paste0(label, ":"),
    value
  ))
}

print_selection_settings <- function(
  selection,
  indent = "    ",
  method_label = "Method",
  criterion_label = "Criterion",
  level_label = "Level"
) {
  print_setting_line(method_label, selection$method, indent = indent)
  print_setting_line(criterion_label, selection$criterion, indent = indent)
  print_setting_line(
    level_label,
    format_threshold_level(selection$level),
    indent = indent
  )

  invisible(NULL)
}

print_construction_settings <- function(
  construction,
  indent = "    ",
  sign_mode_label = "Sign mode"
) {
  print_setting_line(
    sign_mode_label,
    construction$sign_mode,
    indent = indent
  )
  print_setting_line(
    "Edge weighting",
    if (construction$weight_scale == 0) "none" else "sigmoid",
    indent = indent
  )
  print_setting_line(
    "Weight scale",
    if (construction$weight_scale == 0) {
      "none"
    } else {
      format_threshold_level(construction$weight_scale)
    },
    indent = indent
  )
  print_setting_line(
    "Edge standardization",
    if (isTRUE(construction$standardize_edges)) "z-score" else "none",
    indent = indent
  )

  invisible(NULL)
}

print_model_settings <- function(
  model,
  indent = "    ",
  model_label = "Outcome model"
) {
  print_setting_line(
    model_label,
    switch(
      model$type,
      lm = "linear regression",
      model$type
    ),
    indent = indent
  )

  invisible(NULL)
}

print_staged_settings <- function(
  selection,
  construction,
  model,
  indent = "    ",
  headers = NULL,
  selection_labels = list(
    method = "Method",
    criterion = "Criterion",
    level = "Level"
  ),
  construction_labels = list(
    sign_mode = "Sign mode"
  ),
  model_label = "Outcome model"
) {
  if (!is.null(headers$selection)) {
    cat(headers$selection)
  }
  print_selection_settings(
    selection,
    indent = indent,
    method_label = selection_labels$method,
    criterion_label = selection_labels$criterion,
    level_label = selection_labels$level
  )
  if (!is.null(headers$construction)) {
    cat(headers$construction)
  }
  print_construction_settings(
    construction,
    indent = indent,
    sign_mode_label = construction_labels$sign_mode
  )
  if (!is.null(headers$model)) {
    cat(headers$model)
  }
  print_model_settings(
    model,
    indent = indent,
    model_label = model_label
  )

  invisible(NULL)
}

#' Define CPM correlation-based edge-selection settings
#'
#' Build the selection portion of a [spec()]. This helper configures the
#' current correlation-based edge-selection path by choosing the correlation
#' method used to relate each edge to the outcome and how the associated `level`
#' is interpreted.
#'
#' @param method Correlation method used during edge selection. `"pearson"` is
#'   the default CPM path; `"spearman"` can be useful when a monotonic but
#'   non-linear relationship is expected.
#' @param criterion How `level` is interpreted. `"p_value"` thresholds the
#'   absolute correlation by the critical value implied by `level`.
#'   `"absolute"` treats `level` as a direct absolute-correlation cutoff.
#'   `"proportion"` retains a per-sign proportion of positive and negative edges
#'   separately. Because edge counts are discrete and ties at the cutoff are
#'   retained, the realized proportion may be slightly larger than the requested
#'   `level`.
#' @param level Numeric level associated with `criterion`.
#'
#' @examples
#' cpm_selection_cor()
#' cpm_selection_cor(method = "spearman", criterion = "absolute", level = 0.1)
#' @export
cpm_selection_cor <- function(
  method = c("pearson", "spearman"),
  criterion = c("p_value", "absolute", "proportion"),
  level = 0.01
) {
  method <- match.arg(method)
  criterion <- match.arg(criterion)
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level)
  ) {
    stop("`level` must be a single finite number.", call. = FALSE)
  }
  if (level < 0 || level > 1) {
    stop("`level` must be between 0 and 1.", call. = FALSE)
  }
  if (level %in% c(0, 1)) {
    warning(
      "`level` is at a boundary value (0 or 1); selection may become degenerate.",
      call. = FALSE
    )
  }

  structure(
    list(
      type = "cor",
      method = method,
      criterion = criterion,
      level = level
    ),
    class = "cpm_selection_spec"
  )
}

#' Define CPM summary-construction settings
#'
#' Build the feature-construction portion of a [spec()]. This helper
#' configures the current summary-construction path by deciding how positive and
#' negative screened edge sets are represented, whether screened edges are
#' additionally weighted before summary construction, and whether edge
#' standardization is applied before constructing subject-level predictors.
#'
#' @param sign_mode How positive and negative screened information is
#'   represented. `"separate"` constructs `positive_summary` and
#'   `negative_summary`, fits a `joint` stream from both together, and also
#'   returns `positive` and `negative` positive-only / negative-only diagnostic
#'   streams. `"net"` constructs one
#'   `net_summary = positive_summary - negative_summary` feature and returns a
#'   single `net` stream.
#' @param weight_scale Non-negative scale controlling optional sigmoid-style
#'   edge weighting before summary construction. `0` disables additional
#'   weighting and keeps the classic hard-threshold CPM summary. Values greater
#'   than `0` apply the current sigmoid weighting scheme, with larger values
#'   producing a smoother transition around the selection cutoff.
#' @param standardize_edges Logical value indicating if edge values should be
#'   standardized within each training set before CPM feature construction. If
#'   `TRUE`, each edge is centered and scaled to unit variance using the
#'   training data, and the same transformation is applied again at prediction
#'   time. This follows the fold-local edge z-scoring approach described by
#'   Rapuano et al. (2020). Defaults to `FALSE` so the default construction
#'   stays close to the classic CPM summary workflow; set it to `TRUE`
#'   when you want this additional preprocessing step explicitly.
#'
#' @examples
#' cpm_construction_summary()
#' cpm_construction_summary(
#'   sign_mode = "net",
#'   weight_scale = 0.03
#' )
#' @export
cpm_construction_summary <- function(
  sign_mode = c("separate", "net"),
  weight_scale = 0,
  standardize_edges = FALSE
) {
  sign_mode <- match.arg(sign_mode)
  if (
    !is.numeric(weight_scale) ||
      length(weight_scale) != 1L ||
      is.na(weight_scale) ||
      !is.finite(weight_scale) ||
      weight_scale < 0
  ) {
    stop(
      "`weight_scale` must be a single non-negative number.",
      call. = FALSE
    )
  }
  if (
    !is.logical(standardize_edges) ||
      length(standardize_edges) != 1L ||
      is.na(standardize_edges)
  ) {
    stop(
      "`standardize_edges` must be either TRUE or FALSE.",
      call. = FALSE
    )
  }

  structure(
    list(
      type = "summary",
      sign_mode = sign_mode,
      weight_scale = weight_scale,
      standardize_edges = standardize_edges
    ),
    class = "cpm_construction_spec"
  )
}

#' Define the outcome model used after CPM feature construction
#'
#' Build the second-stage model used after CPM has converted selected edges
#' into subject-level predictors. `cpm_model_lm()` fits an intercept-inclusive
#' linear regression with the CPM-derived features for each prediction stream.
#'
#' @examples
#' cpm_model_lm()
#' @export
cpm_model_lm <- function() {
  structure(
    list(type = "lm"),
    class = "cpm_model_spec"
  )
}

assert_spec_class <- function(x, arg, class) {
  if (!inherits(x, class)) {
    stop(
      sprintf("`%s` must be a `%s` object.", arg, class),
      call. = FALSE
    )
  }
}
