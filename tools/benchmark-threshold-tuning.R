if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools to run this benchmark.", call. = FALSE)
}

devtools::load_all()

set.seed(20261117)
n_observations <- 100L
n_edges <- 10000L
outer_folds_count <- 5L
inner_folds_count <- 4L
selection_levels <- c(0.001, 0.01, 0.05, 0.1)
conmat <- matrix(
  stats::rnorm(n_observations * n_edges),
  nrow = n_observations
)
behav <- rowSums(conmat[, seq_len(20), drop = FALSE]) +
  stats::rnorm(n_observations, sd = 0.5)
outer_folds <- cpmr:::make_kfold_assessment_folds(
  seq_len(n_observations),
  outer_folds_count
)

run_reused <- function() {
  set.seed(2026)
  cpm(
    conmat = conmat,
    behav = behav,
    spec = spec(),
    resamples = outer_folds,
    selection_levels = selection_levels,
    inner_resamples = inner_folds_count,
    return_edges = "none"
  )
}

run_repeated <- function() {
  set.seed(2026)
  for (outer_fold in seq_along(outer_folds)) {
    rows_test <- outer_folds[[outer_fold]]
    rows_train <- setdiff(seq_len(n_observations), rows_test)
    inner_folds <- cpmr:::make_kfold_assessment_folds(
      rows_train,
      inner_folds_count
    )
    local_folds <- lapply(inner_folds, function(fold) {
      match(fold, rows_train)
    })
    scores <- vapply(
      selection_levels,
      function(level) {
        candidate_result <- cpm(
          conmat = conmat[rows_train, , drop = FALSE],
          behav = behav[rows_train],
          spec = spec(selection = cpm_selection_cor(level = level)),
          resamples = local_folds,
          return_edges = "none"
        )
        fold_scores <- vapply(
          seq_along(local_folds),
          function(fold) {
            assessment <- candidate_result$predictions$fold == fold
            errors <- candidate_result$predictions$observed[assessment] -
              candidate_result$predictions$joint[assessment]
            sqrt(mean(errors^2))
          },
          numeric(1)
        )
        mean(fold_scores)
      },
      numeric(1)
    )
    selected_spec <- spec(
      selection = cpm_selection_cor(
        level = selection_levels[[which.min(scores)]]
      )
    )
    cpmr:::fit_split(
      conmat = conmat,
      behav = behav,
      covariates = NULL,
      rows_train = rows_train,
      rows_test = rows_test,
      selection_spec = selected_spec$selection,
      construction_spec = selected_spec$construction,
      model_spec = selected_spec$model
    )
  }
}

median_elapsed <- function(fun, repetitions = 3L) {
  elapsed <- replicate(repetitions, {
    gc()
    unname(system.time(fun())[["elapsed"]])
  })
  stats::median(elapsed)
}

reused_seconds <- median_elapsed(run_reused)
repeated_seconds <- median_elapsed(run_repeated)

cat(sprintf("R: %s\n", getRversion()))
cat(sprintf(
  paste0(
    "Workload: %d observations, %d edges, %d outer folds, ",
    "%d inner folds, %d selection levels; median of 3 runs\n"
  ),
  n_observations,
  n_edges,
  outer_folds_count,
  inner_folds_count,
  length(selection_levels)
))
cat(sprintf("Cached inner associations: %.2f seconds\n", reused_seconds))
cat(sprintf("Repeated native cpm() runs: %.2f seconds\n", repeated_seconds))
cat(sprintf("Speedup: %.2fx\n", repeated_seconds / reused_seconds))
