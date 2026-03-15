#' CPM tuning parameter: network
#'
#' @param values Allowed network values.
#'
#' @return A qualitative dials parameter object.
#' @export
cpm_network <- function(values = c("both", "pos", "neg")) {
  rlang::check_installed("dials")
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(network = "CPM network"),
    finalize = NULL
  )
}

#' CPM tuning parameter: threshold method
#'
#' @param values Allowed threshold methods.
#'
#' @return A qualitative dials parameter object.
#' @export
cpm_thresh_method <- function(values = c("alpha", "sparsity")) {
  rlang::check_installed("dials")
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(thresh_method = "CPM threshold method"),
    finalize = NULL
  )
}

#' CPM tuning parameter: bias correction
#'
#' @param values Allowed bias-correction values.
#'
#' @return A qualitative dials parameter object.
#' @export
cpm_bias_correct <- function(values = c(TRUE, FALSE)) {
  rlang::check_installed("dials")
  dials::new_qual_param(
    type = "logical",
    values = values,
    label = c(bias_correct = "Bias correction"),
    finalize = NULL
  )
}
