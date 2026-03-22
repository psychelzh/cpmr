#' Define CPM correlation-based edge-selection settings
#'
#' Build the selection portion of a `cpm_spec()`. This helper configures the
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
  normalize_selection_level(
    level,
    criterion = criterion,
    warn_boundary = TRUE
  )

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
#' Build the feature-construction portion of a `cpm_spec()`. This helper
#' configures the current summary-construction path by deciding how positive and
#' negative screened edge sets are represented, whether screened edges are
#' additionally weighted before summary construction, and whether edge
#' standardization is applied before constructing subject-level predictors.
#'
#' @param polarity How positive and negative screened information is represented.
#'   `"separate"` constructs `positive_summary` and `negative_summary`, fits a
#'   `joint` stream from both together, and also returns `positive` and
#'   `negative` positive-only / negative-only diagnostic streams. `"net"`
#'   constructs one `net_summary = positive_summary - negative_summary`
#'   feature and returns a single `net` stream.
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
#'   polarity = "net",
#'   weight_scale = 0.03
#' )
#' @export
cpm_construction_summary <- function(
  polarity = c("separate", "net"),
  weight_scale = 0,
  standardize_edges = FALSE
) {
  polarity <- match.arg(polarity)
  normalize_weight_scale(weight_scale)
  normalize_standardize_edges(standardize_edges)

  structure(
    list(
      type = "summary",
      polarity = polarity,
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
