# nocov start

make_birch <- function() {
  modelenv::set_new_model("birch")

  modelenv::set_model_mode("birch", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("birch", "partition", "stream")
  modelenv::set_dependency(
    model = "birch",
    mode = "partition",
    eng = "stream",
    pkg = "stream"
  )
  modelenv::set_dependency(
    model = "birch",
    mode = "partition",
    eng = "stream",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "birch",
    eng = "stream",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("data"),
      func = c(pkg = "tidyclust", fun = ".birch_fit_stream"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "birch",
    eng = "stream",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "birch",
    eng = "stream",
    exposed = "max_leaf",
    original = "max_leaf",
    func = list(pkg = "tidyclust", fun = "max_leaf"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "birch",
    eng = "stream",
    exposed = "threshold",
    original = "threshold",
    func = list(pkg = "tidyclust", fun = "threshold"),
    has_submodel = TRUE
  )

  modelenv::set_model_arg(
    model = "birch",
    eng = "stream",
    exposed = "branching_factor",
    original = "branching_factor",
    func = list(pkg = "tidyclust", fun = "branching_factor"),
    has_submodel = TRUE
  )

  modelenv::set_pred(
    model = "birch",
    eng = "stream",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".birch_predict_stream"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )
}

# nocov end
