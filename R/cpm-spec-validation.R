validate_cpm_component <- function(x, component, constructor) {
  if (!is.character(component) || length(component) != 1L || is.na(component)) {
    stop("`component` must be a single string.", call. = FALSE)
  }
  if (
    !is.character(constructor) ||
      length(constructor) != 1L ||
      is.na(constructor)
  ) {
    stop("`constructor` must be a single string.", call. = FALSE)
  }

  class <- sprintf("cpm_%s_spec", component)

  if (!inherits(x, class)) {
    stop(
      sprintf(
        "`%s` must be created by `%s()`.",
        component,
        constructor
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

validate_choice <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% choices)) {
    stop(
      sprintf(
        "%s must be one of %s.",
        arg,
        paste(sprintf('"%s"', choices), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  x
}

validate_selection_spec <- function(x) {
  validate_cpm_component(
    x,
    component = "selection",
    constructor = "cpm_selection_cor"
  )

  type <- validate_choice(x$type, choices = "cor", arg = "`selection$type`")
  method <- validate_choice(
    x$method,
    choices = c("pearson", "spearman"),
    arg = "`selection$method`"
  )
  criterion <- validate_choice(
    x$criterion,
    choices = c("p_value", "absolute", "proportion"),
    arg = "`selection$criterion`"
  )
  level <- validate_selection_level(
    x$level,
    criterion = criterion,
    arg = "`selection$level`",
    warn_boundary = FALSE
  )

  structure(
    list(
      type = type,
      method = method,
      criterion = criterion,
      level = level
    ),
    class = "cpm_selection_spec"
  )
}

validate_construction_spec <- function(x) {
  validate_cpm_component(
    x,
    component = "construction",
    constructor = "cpm_construction_summary"
  )

  type <- validate_choice(
    x$type,
    choices = "summary",
    arg = "`construction$type`"
  )
  polarity <- validate_choice(
    x$polarity,
    choices = c("separate", "net"),
    arg = "`construction$polarity`"
  )
  weight_scale <- validate_weight_scale(
    x$weight_scale,
    arg = "`construction$weight_scale`"
  )
  standardize_edges <- validate_standardize_edges(
    x$standardize_edges,
    arg = "`construction$standardize_edges`"
  )
  prediction_streams <- switch(
    type,
    summary = switch(
      polarity,
      separate = c("joint", edge_signs),
      net = "net",
      stop("`polarity` must be either \"separate\" or \"net\".", call. = FALSE)
    ),
    stop("`type` must be a supported construction type.", call. = FALSE)
  )

  structure(
    list(
      type = type,
      polarity = polarity,
      prediction_streams = prediction_streams,
      weight_scale = weight_scale,
      standardize_edges = standardize_edges
    ),
    class = "cpm_construction_spec"
  )
}

validate_model_spec <- function(x) {
  validate_cpm_component(
    x,
    component = "model",
    constructor = "cpm_model_lm"
  )

  type <- validate_choice(x$type, choices = "lm", arg = "`model$type`")

  structure(
    list(type = type),
    class = "cpm_model_spec"
  )
}

validate_selection_level <- function(
  level,
  criterion,
  arg = "`level`",
  warn_boundary = FALSE
) {
  if (
    !is.numeric(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !is.finite(level)
  ) {
    stop(sprintf("%s must be a single finite number.", arg), call. = FALSE)
  }

  switch(
    criterion,
    p_value = {
      if (level <= 0 || level > 1) {
        stop(
          sprintf(
            "%s must be in (0, 1] when `criterion = \"p_value\"`.",
            arg
          ),
          call. = FALSE
        )
      }
      if (warn_boundary && level == 1) {
        warning(
          paste(
            "`level = 1` with `criterion = \"p_value\"` effectively",
            "disables p-value filtering."
          ),
          call. = FALSE
        )
      }
    },
    absolute = ,
    proportion = {
      if (level < 0 || level > 1) {
        stop(
          sprintf(
            "%s must be between 0 and 1 when `criterion = \"%s\"`.",
            arg,
            criterion
          ),
          call. = FALSE
        )
      }
      if (warn_boundary) {
        if (criterion == "absolute" && level == 0) {
          warning(
            paste(
              "`level = 0` with `criterion = \"absolute\"` effectively",
              "disables absolute-correlation filtering."
            ),
            call. = FALSE
          )
        }
        if (criterion == "absolute" && level == 1) {
          warning(
            paste(
              "`level = 1` with `criterion = \"absolute\"` retains only",
              "perfect absolute-correlation edges."
            ),
            call. = FALSE
          )
        }
        if (criterion == "proportion" && level == 0) {
          warning(
            paste(
              "`level = 0` with `criterion = \"proportion\"` retains no",
              "edges by design."
            ),
            call. = FALSE
          )
        }
        if (criterion == "proportion" && level == 1) {
          warning(
            paste(
              "`level = 1` with `criterion = \"proportion\"` retains all",
              "eligible edges."
            ),
            call. = FALSE
          )
        }
      }
    },
    stop("`criterion` must be a supported selection criterion.", call. = FALSE)
  )

  level
}

validate_weight_scale <- function(scale, arg = "`weight_scale`") {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      is.na(scale) ||
      !is.finite(scale) ||
      scale < 0
  ) {
    stop(
      sprintf("%s must be a single non-negative number.", arg),
      call. = FALSE
    )
  }

  scale
}

validate_standardize_edges <- function(
  standardize_edges,
  arg = "`standardize_edges`"
) {
  if (
    !is.logical(standardize_edges) ||
      length(standardize_edges) != 1L ||
      is.na(standardize_edges)
  ) {
    stop(sprintf("%s must be either TRUE or FALSE.", arg), call. = FALSE)
  }

  standardize_edges
}
