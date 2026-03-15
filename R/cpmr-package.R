#' cpmr: Tidymodels Interfaces for Connectome Predictive Modeling
#'
#' `cpmr` provides a tidymodels-facing implementation of
#' connectome-based predictive modeling (CPM). The package includes:
#'
#' - a `parsnip` model specification via [cpm_reg()];
#' - integration with `workflows`, `recipes`, and `tune`;
#' - CPM-specific metrics such as [cpm_cor()] and [cpm_spearman()];
#' - helpers to inspect stored CPM edge masks via [collect_edges()].
#'
#' The modeling engine keeps edge selection and model fitting inside each
#' resampling split so that training and assessment data remain separated.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

utils::globalVariables(c("new_data", "object"))
