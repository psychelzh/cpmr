# See `help(run_script, package = "touchstone")` for interactive use.

make_build_spec_expr <- function(standardize_edges = FALSE) {
  rlang::expr({
    if ("cpm_selection_cor" %in% getNamespaceExports("cpmr")) {
      cpmr::cpm_spec(
        selection = cpmr::cpm_selection_cor(
          method = "pearson",
          criterion = "p_value",
          level = 0.01
        ),
        construction = cpmr::cpm_construction_summary(
          polarity = "separate",
          weight_scale = 0,
          standardize_edges = !!standardize_edges
        ),
        model = cpmr::cpm_model_lm()
      )
    } else {
      cpmr::cpm_spec(
        thresh_method = "alpha",
        thresh_level = 0.01,
        bias_correct = !!standardize_edges
      )
    }
  })
}

build_spec_expr <- make_build_spec_expr(standardize_edges = FALSE)
build_spec_standardized_expr <- make_build_spec_expr(standardize_edges = TRUE)

build_spec_setup <- rlang::expr({
  build_spec <- function() {
    !!build_spec_expr
  }
})

fit_default_setup <- rlang::expr({
  set.seed(20260321)
  conmat_fit <- matrix(rnorm(300 * 20000), nrow = 300)
  behav_fit <- rowMeans(conmat_fit[, 1:100, drop = FALSE]) +
    rnorm(300, sd = 0.5)

  build_spec <- function() {
    !!build_spec_expr
  }

  fit_spec <- build_spec()

  # Warm up the first fit so the measured benchmark is closer to steady-state.
  invisible(cpmr::fit(fit_spec, conmat = conmat_fit, behav = behav_fit))
})

fit_standardize_edges_setup <- rlang::expr({
  set.seed(20260321)
  conmat_fit <- matrix(rnorm(300 * 20000), nrow = 300)
  behav_fit <- rowMeans(conmat_fit[, 1:100, drop = FALSE]) +
    rnorm(300, sd = 0.5)

  build_spec <- function() {
    !!build_spec_standardized_expr
  }

  fit_spec <- build_spec()

  # Warm up the first fit so the measured benchmark is closer to steady-state.
  invisible(cpmr::fit(fit_spec, conmat = conmat_fit, behav = behav_fit))
})

fit_resamples_setup <- rlang::expr({
  set.seed(20260322)
  conmat_res <- matrix(rnorm(240 * 12000), nrow = 240)
  behav_res <- rowMeans(conmat_res[, 1:80, drop = FALSE]) + rnorm(240, sd = 0.5)

  build_spec <- function() {
    !!build_spec_expr
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
  expr_before_benchmark = !!build_spec_setup,
  build_spec = build_spec(),
  n = 20
)

touchstone::benchmark_run(
  expr_before_benchmark = !!fit_default_setup,
  fit_default = {
    cpmr::fit(fit_spec, conmat = conmat_fit, behav = behav_fit)
  },
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = !!fit_standardize_edges_setup,
  fit_standardize_edges = {
    cpmr::fit(fit_spec, conmat = conmat_fit, behav = behav_fit)
  },
  n = 5
)

touchstone::benchmark_run(
  expr_before_benchmark = !!fit_resamples_setup,
  fit_resamples_default = fit_resamples_default(fit_spec),
  n = 3
)

touchstone::benchmark_analyze()
