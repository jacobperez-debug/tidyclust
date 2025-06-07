#' BIRCH (Balanced Iterative and Reduced Clustering using Hierarchies)
#'
#' @description
#'
#' `birch()` defines a model that fits clusters by incrementally building a
#' compact tree structure, using radius_threshold based clustering features.
#'
#' There are different ways to fit this model, and the method of estimation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[details_birch_stream]{stream}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"stream"`.
#' @param radius_threshold Positive double, radius radius_threshold for merging points into
#' the same cluster feature (CF) node.
#' @param branching_factor Positive integer, maximum number of child nodes in a
#' non-leaf node of the CF tree.
#' @param max_leaf Positive integer, maximum number of CF entries in a leaf node.
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' In a BIRCH model, prediction assigns a new observation to the cluster whose
#' centroid is the closest, using the final CF tree structure. The centroids are
#' computed from the cluster feature entries of the leaf node and data points
#' are assigned using euclidian distance.
#'
#' @return A `birch` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("birch")
#'
#' birch()
#' @export
birch <-
  function(mode = "partition",
           engine = "stream",
           max_leaf = 100,
           radius_threshold = 0.5,
           branching_factor = 50,
           global_method = "none",
           num_clusters = NULL,
           cut_height = NULL) {

    args <- list(
      max_leaf = enquo(max_leaf),
      radius_threshold = enquo(radius_threshold),
      branching_factor = enquo(branching_factor),
      global_method = enquo(global_method),
      num_clusters = enquo(num_clusters),
      cut_height = enquo(cut_height)
    )

    new_cluster_spec(
      "birch",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.birch <- function(x, ...) {
  cat("BIRCH Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update birch
#' @rdname tidyclust_update
#' @export
update.birch <- function(object,
                         parameters = NULL,
                         radius_threshold = NULL,
                         branching_factor = NULL,
                         max_leaf = NULL,
                         global_method = NULL,
                         num_clusters = NULL,
                         cut_height = NULL,
                         fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args, fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    radius_threshold = enquo(radius_threshold),
    branching_factor = enquo(branching_factor),
    max_leaf = enquo(max_leaf),
    global_method = enquo(global_method),
    num_clusters = enquo(num_clusters),
    cut_height = enquo(cut_height)
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  new_cluster_spec(
    "birch",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


# # ----------------------------------------------------------------------------

#' @export
check_args.birch <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (args$max_leaf < 1) {
    rlang::abort("Max leaf entries must be at least 1.")
  }
  if (args$radius_threshold < 0) {
    rlang::abort("Radius threshold cannot be less than 0.")
  }
  if (args$branching_factor < 1) {
    rlang::abort("Branching factor must be at least 1.")
  }
  if (!args$global_method %in% c("none", "k_means", "hier_clust")) {
    rlang::abort("'global_method' must be one of 'none', 'k_means', or 'hier_clust'.")
  }
  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters < 0)) {
    rlang::abort("The number of centers should be >= 0.")
  }


  invisible(object)
}

#' @export
translate_tidyclust.birch <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around birch function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `stream::DSC_BIRCH` and retains the parameters `radius_threshold`, `branching_factor`, or
#' `max_leaf` as an attribute.
#'
#' @param x matrix or data frame
#' @param radius_threshold the maximum diameter for sub-cluster
#' @param branching_factor the maximum number of CF subclusters
#' @param max_leaf the maximum number of entries in a leaf node
#'
#' @keywords internal
#' @export
.birch_fit_stream <- function(x,
                              radius_threshold = NULL,
                              branching_factor = NULL,
                              max_leaf = NULL,
                              global_method = c("none", "k_means", "hier_clust"),
                              num_clusters = NULL,
                              cut_height = NULL
                      ) {
  global_method <- match.arg(global_method)

  dmat <- as.matrix(x)

  res <- stream::DSC_BIRCH(
    threshold = radius_threshold,
    branching = branching_factor,
    maxLeaf = max_leaf
    )

  update(res, stream::DSD_Memory(dmat), n = nrow(dmat))

  attr(res, "radius_threshold") <- radius_threshold
  attr(res, "branching_factor") <- branching_factor
  attr(res, "max_leaf") <- max_leaf
  attr(res, "training_data") <- x
  attr(res, "global_method") <- global_method
  attr(res, "num_clusters") <- num_clusters
  attr(res, "cut_height") <- cut_height

  microclusters <- stream::get_microclusters(res)

  if(nrow(microclusters) > 5000) {
    warning(
      "Prediction or cluster assignment may exceed memory limits consider reducing microclusters or using global clustering."
    )
  }


  if (global_method == "k_means") {
    if (is.null(num_clusters)) {
      stop("Please specify `num_clusters` when using global_method = `k_means`.")
    }

    k_model <- k_means(num_clusters = num_clusters) %>%
      fit(~ ., data = as.data.frame(microclusters))

    attr(res, "global_model") <- k_model
  }

  if (global_method == "hier_clust") {

    hc_model <- hier_clust() %>%
      fit(~ ., data = as.data.frame(microclusters))

    attr(res, "global_model") <- hc_model
  }

  return(res)
}

