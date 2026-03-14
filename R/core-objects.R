core_new_cpm <- function(call, folds, behav, pred, edges, model, spec, params) {
  structure(
    list(
      folds = folds,
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

core_new_cpm_resamples <- function(
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
