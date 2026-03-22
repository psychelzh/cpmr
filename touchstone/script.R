# See `help(run_script, package = "touchstone")` for interactive use.

workflow_default_setup <- rlang::expr({
  set.seed(20260322)
  conmat_workflow <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_workflow <- rowMeans(conmat_workflow[, 1:80, drop = FALSE]) +
    rnorm(240, sd = 0.5)

  # Warm up the first workflow so the measured benchmark is closer to
  # steady-state behavior.
  invisible(cpmr::cpm(
    conmat = conmat_workflow,
    behav = behav_workflow,
    thresh_method = "alpha",
    thresh_level = 0.01,
    kfolds = 8L,
    bias_correct = FALSE,
    return_edges = "none"
  ))
})

workflow_standardize_edges_setup <- rlang::expr({
  set.seed(20260322)
  conmat_workflow <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_workflow <- rowMeans(conmat_workflow[, 1:80, drop = FALSE]) +
    rnorm(240, sd = 0.5)

  # Use the old bias-corrected workflow as the closest predecessor to
  # edge standardization in the new API.
  invisible(cpmr::cpm(
    conmat = conmat_workflow,
    behav = behav_workflow,
    thresh_method = "alpha",
    thresh_level = 0.01,
    kfolds = 8L,
    bias_correct = TRUE,
    return_edges = "none"
  ))
})

fit_resamples_standardize_edges_setup <- rlang::expr({
  set.seed(20260322)
  conmat_res <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_res <- rowMeans(conmat_res[, 1:80, drop = FALSE]) + rnorm(240, sd = 0.5)

  build_spec <- function() {
    !!build_spec_standardized_expr
  }

  fit_spec <- build_spec()

  fit_resamples_default <- function(spec) {
    if ("cpm_selection_cor" %in% getNamespaceExports("cpmr")) {
      cpmr::fit_resamples(
        spec,
        conmat = conmat_res,
        behav = behav_res,
        resamples = 8L,
        return_edges = "none"
      )
    } else {
      cpmr::fit_resamples(
        spec,
        conmat = conmat_res,
        behav = behav_res,
        kfolds = 8L,
        return_edges = "none"
      )
    }
  }
})

touchstone::branch_install(install_dependencies = TRUE)

touchstone::benchmark_run(
  expr_before_benchmark = !!workflow_default_setup,
  workflow_default = cpmr::cpm(
    conmat = conmat_workflow,
    behav = behav_workflow,
    thresh_method = "alpha",
    thresh_level = 0.01,
    kfolds = 8L,
    bias_correct = FALSE,
    return_edges = "none"
  ),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = !!workflow_standardize_edges_setup,
  workflow_standardize_edges = cpmr::cpm(
    conmat = conmat_workflow,
    behav = behav_workflow,
    thresh_method = "alpha",
    thresh_level = 0.01,
    kfolds = 8L,
    bias_correct = TRUE,
    return_edges = "none"
  ),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = !!fit_resamples_standardize_edges_setup,
  fit_resamples_standardize_edges = fit_resamples_default(fit_spec),
  n = 3
)

touchstone::benchmark_analyze()
