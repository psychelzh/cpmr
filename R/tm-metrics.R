cpm_cor_impl <- function(truth, estimate) {
  safe_cor(truth, estimate, method = "pearson")
}

cpm_spearman_impl <- function(truth, estimate) {
  safe_cor(truth, estimate, method = "spearman")
}

#' CPM Pearson correlation metric
#'
#' @param data A data frame containing truth and estimate columns.
#' @param truth The column identifier for the true outcome.
#' @param estimate The column identifier for the predicted outcome.
#' @param na_rm Should missing values be removed before computing the metric?
#' @param case_weights Not used.
#' @param ... Unused.
#'
#' @return A tibble with a single correlation estimate.
#' @export
cpm_cor <- function(data, ...) {
  UseMethod("cpm_cor")
}

#' @export
cpm_cor <- yardstick::new_numeric_metric(cpm_cor, direction = "maximize")

#' @export
cpm_cor.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "cpm_cor",
    fn = cpm_cor_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

#' @rdname cpm_cor
#' @export
cpm_cor_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)
  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  cpm_cor_impl(truth, estimate)
}

#' CPM Spearman correlation metric
#'
#' @inheritParams cpm_cor
#' @return A tibble with a single rank-correlation estimate.
#' @export
cpm_spearman <- function(data, ...) {
  UseMethod("cpm_spearman")
}

#' @export
cpm_spearman <- yardstick::new_numeric_metric(
  cpm_spearman,
  direction = "maximize"
)

#' @export
cpm_spearman.data.frame <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "cpm_spearman",
    fn = cpm_spearman_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    na_rm = na_rm,
    case_weights = !!rlang::enquo(case_weights)
  )
}

#' @rdname cpm_spearman
#' @export
cpm_spearman_vec <- function(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)
  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)
    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  cpm_spearman_impl(truth, estimate)
}
