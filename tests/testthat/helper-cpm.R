example_cpm_output_result <- function(resamples = 5L) {
  withr::local_seed(123)
  conmat <- matrix(rnorm(240), ncol = 12)
  behav <- rnorm(20)

  cpm(conmat = conmat, behav = behav, spec = spec(), resamples = resamples)
}
