example_cpm_output_result <- function(resamples = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(120), ncol = 12)
  behav <- rnorm(10)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = resamples)
}
