#' BIRCH (Balanced Iterative and Reduced Clustering using Hierarchies)
#'
#' @description
#'
#' This function performs scalable clustering for large datasets by constructing
#' a Clustering Feature Tree (CF Tree) with a specified threshold, branching,
#' and maxLeaf factor.
#'
#' `birch()` defines a model that fits clusters based on a distance-based
#' dendrogram
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. Possible engines are listed below. The default for this
#'   model is `"stream"`.
#' @param threshold Maximum diameter of sub-clusters stored in leaf nodes.
#' @param branching_factor Maximum number of CF entries per node.
#' @param max_leaf Maximum number of data points allowed within a leaf node.
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
           threshold = 0.5,
           branching_factor = 50) {
    args <- list(
      max_leaf = enquo(max_leaf),
      threshold = enquo(threshold),
      branching_factor = enquo(branching_factor)
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
                              threshold = NULL,
                              branching_factor = NULL,
                              max_leaf = NULL,
                              fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args, fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    threshold = enquo(threshold),
    branching_factor = enquo(branching_factor),
    max_leaf = enquo(max_leaf)
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
  if (args$threshold < 0) {
    rlang::abort("Threshold cannot be less than 0.")
  }
  if (args$branching_factor < 1) {
    rlang::abort("Branching factor must be at least 1.")
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
#' `stream::DSC_BIRCH` and retains the parameters `threshold`, `branching_factor`, or
#' `max_leaf` as an attribute.
#'
#' @param x matrix or data frame
#' @param threshold the maximum diameter for sub-cluster
#' @param branching_factor the maximum number of CF subclusters
#' @param max_leaf the maximum number of entries in a leaf node
#'
#' @keywords internal
#' @export
.birch_fit_stream <- function(x,
                                  threshold = NULL,
                                  branching_factor = NULL,
                                  max_leaf = NULL
                      ) {
  dmat <- as.matrix(x)

  res <- stream::DSC_BIRCH(
    threshold = threshold,
    branching = branching_factor,
    maxLeaf = max_leaf
    )

  data_stream <- stream::DSD_Memory(dmat)

  update(res, data_stream, n = nrow(dmat))

  attr(res, "threshold") <- threshold
  attr(res, "branching_factor") <- branching_factor
  attr(res, "max_leaf") <- max_leaf
  attr(res, "training_data") <- x

  return(res)
}

