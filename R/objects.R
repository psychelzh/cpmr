#' cpm Fitted Object
#'
#' A `cpm` object is returned by [fit()] when fitting a [cpm_spec()] on a
#' single dataset.
#'
#' @section Structure:
#' A `cpm` object is a list with the following elements:
#' \describe{
#'   \item{`call`}{Matched call used for fitting.}
#'   \item{`spec`}{The originating `cpm_spec` object.}
#'   \item{`model`}{Trained CPM model components used by prediction.}
#'   \item{`real`}{Observed behavior values after preprocessing decisions.}
#'   \item{`pred`}{Matrix of in-sample predictions (`both`, `pos`, `neg`).}
#'   \item{`edges`}{Stored edge selection output according to
#'     `return_edges`.}
#'   \item{`params`}{Parameter list used at fit time.}
#' }
#'
#' @seealso [fit()], [summary.cpm()], [tidy.cpm()], [collect_edges()]
#' @name cpm
NULL

#' cpm_resamples Resampling Object
#'
#' A `cpm_resamples` object is returned by [fit_resamples()] and stores
#' fold-level outputs from resampling.
#'
#' @section Structure:
#' A `cpm_resamples` object is a list with the following elements:
#' \describe{
#'   \item{`spec`}{The originating `cpm_spec` object.}
#'   \item{`folds`}{List of assessment-row indices for each fold.}
#'   \item{`metrics`}{Data frame of fold-level performance metrics.}
#'   \item{`predictions`}{Data frame of observation-level predictions with
#'     fold IDs.}
#'   \item{`edges`}{Stored edge output based on `return_edges`
#'     (`NULL`/matrix/array).}
#'   \item{`params`}{Parameter list used for the resampling run.}
#' }
#'
#' @seealso [fit_resamples()], [collect_metrics()], [collect_predictions()],
#'   [collect_edges()]
#' @name cpm_resamples
NULL
