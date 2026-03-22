build_construction_model <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = build_summary_construction_model(
      conmat = conmat,
      edge_selection = edge_selection,
      construction_spec = construction_spec
    )
  )
}

construction_prediction_streams <- function(construction_spec) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = names(summary_stream_spec(construction_spec$polarity))
  )
}

construction_stream_features <- function(
  construction_model,
  prediction_stream,
  conmat_new = NULL
) {
  switch(
    construction_model$type,
    summary = summary_stream_features(
      construction_model = construction_model,
      prediction_stream = prediction_stream,
      conmat = conmat_new
    )
  )
}
