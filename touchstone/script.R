# See `help(run_script, package = "touchstone")` for interactive use.

workflow_default_setup <- rlang::expr({
  set.seed(20260322)
  conmat_workflow <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_workflow <- rowMeans(conmat_workflow[, 1:80, drop = FALSE]) +
    rnorm(240, sd = 0.5)

  run_workflow <- function() {
    if ("spec" %in% getNamespaceExports("cpmr")) {
      workflow_spec <- cpmr::spec(
        selection = cpmr::cpm_selection_cor(
          method = "pearson",
          criterion = "p_value",
          level = 0.01
        ),
        construction = cpmr::cpm_construction_strength(
          sign_mode = "separate",
          weight_scale = 0,
          standardize_edges = FALSE
        ),
        model = cpmr::cpm_model_lm()
      )

      return(cpmr::cpm(
        conmat = conmat_workflow,
        behav = behav_workflow,
        spec = workflow_spec,
        resamples = 8L,
        return_edges = "none"
      ))
    }

    cpmr::cpm(
      conmat = conmat_workflow,
      behav = behav_workflow,
      thresh_method = "alpha",
      thresh_level = 0.01,
      kfolds = 8L,
      bias_correct = FALSE,
      return_edges = "none"
    )
  }

  # Warm up the first workflow so the measured benchmark is closer to
  # steady-state behavior.
  invisible(run_workflow())
})

workflow_standardize_edges_setup <- rlang::expr({
  set.seed(20260322)
  conmat_workflow <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_workflow <- rowMeans(conmat_workflow[, 1:80, drop = FALSE]) +
    rnorm(240, sd = 0.5)

  run_workflow <- function() {
    if ("spec" %in% getNamespaceExports("cpmr")) {
      workflow_spec <- cpmr::spec(
        selection = cpmr::cpm_selection_cor(
          method = "pearson",
          criterion = "p_value",
          level = 0.01
        ),
        construction = cpmr::cpm_construction_strength(
          sign_mode = "separate",
          weight_scale = 0,
          standardize_edges = TRUE
        ),
        model = cpmr::cpm_model_lm()
      )

      return(cpmr::cpm(
        conmat = conmat_workflow,
        behav = behav_workflow,
        spec = workflow_spec,
        resamples = 8L,
        return_edges = "none"
      ))
    }

    cpmr::cpm(
      conmat = conmat_workflow,
      behav = behav_workflow,
      thresh_method = "alpha",
      thresh_level = 0.01,
      kfolds = 8L,
      bias_correct = TRUE,
      return_edges = "none"
    )
  }

  # Warm up the first workflow so the measured benchmark is closer to
  # steady-state behavior.
  invisible(run_workflow())
})

touchstone::branch_install(install_dependencies = TRUE)

touchstone::benchmark_run(
  expr_before_benchmark = !!workflow_default_setup,
  workflow_default = run_workflow(),
  n = 10
)

touchstone::benchmark_run(
  expr_before_benchmark = !!workflow_standardize_edges_setup,
  workflow_standardize_edges = run_workflow(),
  n = 10
)

touchstone::benchmark_analyze()
