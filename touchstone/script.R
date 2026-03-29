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

touchstone::benchmark_analyze()
