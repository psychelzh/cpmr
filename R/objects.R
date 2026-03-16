new_cpm_spec <- function(params) {
  structure(
    list(params = params),
    class = "cpm_spec"
  )
}

new_cpm <- function(call, behav, pred, edges, model, spec, params) {
  structure(
    list(
      real = behav,
      pred = pred,
      edges = edges,
      model = model,
      spec = spec,
      call = call,
      params = params
    ),
    class = "cpm"
  )
}

new_cpm_resamples <- function(
  spec,
  folds,
  edges,
  metrics,
  predictions,
  params
) {
  structure(
    list(
      spec = spec,
      folds = folds,
      edges = edges,
      metrics = metrics,
      predictions = predictions,
      params = params
    ),
    class = "cpm_resamples"
  )
}
