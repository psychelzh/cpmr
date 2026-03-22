build_construction_model <- function(
  conmat,
  edge_selection,
  construction_spec
) {
  construction_spec <- validate_construction_spec(construction_spec)

  switch(
    construction_spec$type,
    summary = summary_construction_model(
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
    summary = summary_prediction_streams(construction_spec$polarity)
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
      summary_features = summary_construction_features(
        construction_model = construction_model,
        conmat = conmat_new
      ),
      polarity = construction_model$construction$polarity,
      prediction_stream = prediction_stream
    )
  )
}
