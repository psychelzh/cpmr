simulate_cpm_problem <- function(n = 60, p = 30, seed = 1) {
  withr::local_seed(seed)

  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(x) <- paste0("edge_", seq_len(p))
  rownames(x) <- paste0("id_", seq_len(n))

  signal <- 1.2 * x[, 1] - 0.9 * x[, 2] + 0.6 * x[, 3]
  y <- signal + rnorm(n, sd = 0.4)
  names(y) <- rownames(x)

  list(
    x = x,
    y = y,
    data = data.frame(y = unname(y), x, check.names = FALSE)
  )
}

simulate_grouped_cpm_problem <- function(
  n_subjects = 24,
  repeats = 2,
  p = 20,
  seed = 1
) {
  withr::local_seed(seed)

  subject_x <- matrix(rnorm(n_subjects * p), nrow = n_subjects, ncol = p)
  colnames(subject_x) <- paste0("edge_", seq_len(p))
  subject_signal <- 1.1 *
    subject_x[, 1] -
    0.8 * subject_x[, 2] +
    0.5 * subject_x[, 3]

  subject_ids <- paste0("subj_", seq_len(n_subjects))
  sample_ids <- unlist(lapply(
    subject_ids,
    function(id) paste0(id, "_run", seq_len(repeats))
  ))
  subject_index <- rep(seq_len(n_subjects), each = repeats)

  x <- subject_x[subject_index, , drop = FALSE] +
    matrix(rnorm(n_subjects * repeats * p, sd = 0.15), ncol = p)
  colnames(x) <- colnames(subject_x)
  rownames(x) <- sample_ids

  y <- subject_signal[subject_index] + rnorm(n_subjects * repeats, sd = 0.35)
  names(y) <- sample_ids

  list(
    x = x,
    y = y,
    subject_id = factor(rep(subject_ids, each = repeats)),
    data = data.frame(
      subject_id = factor(rep(subject_ids, each = repeats)),
      y = unname(y),
      x,
      check.names = FALSE
    )
  )
}
