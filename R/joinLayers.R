
#' @param by Either 'column' or 'row'. Which axis should the layers be joined
#' along with.
#'
#' @details
#' The underlying implemention is to call \code{\link{concatByRows}} or
#' \code{\link{concatByCols}} on the matrix layers according to `by`. Therefore,
#' a \code{\link{MatrixLayers}} containing matrix-like objects that has
#' implemented these two methods should work when calling `joinLayers`.
#'
#' If `by` is set to 'row' for a \code{\link{MatrixLayers}}, no intersection is
#' allowed in the row map. If `by` is 'column', no intersection is allowed in
#' the column map.
#'
#' @rdname joinLayers
#' @export
#' @method joinLayers MatrixLayers
joinLayers.MatrixLayers <- function(object, by = c("column", "row"), ...) {
  by <- match.arg(by)
  if (by == "row") {
    .valid_intersection(object@rowMap, by = "row")
    concat.func <- concatByRows
  } else if (by == "column") {
    .valid_intersection(object@colMap, by = "column")
    concat.func <- concatByCols
  } else {
    stop("Invalid 'by' for joinLayers.")
  }
  layers <- as.list(object)
  for (i in seq_along(layers)) {
    layers[[i]] <- .subset_mat_by_dimnames(
      layers[[i]],
      rownames = getMappedEntries(object@rowMap, i = i),
      colnames = getMappedEntries(object@colMap, i = i)
    )
  }
  if (length(layers) == 1) {
    return(.subset_mat_by_dimnames(
      layers[[1]],
      rownames = rownames(object@rowMap),
      colnames = rownames(object@colMap)
    ))
  }
  layers <- concat.func(layers[[1]], layers[2:length(layers)])
  .subset_mat_by_dimnames(
    layers,
    rownames = rownames(object@rowMap),
    colnames = rownames(object@colMap)
  )
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.valid_intersection <- function(logmap, by = c("row", "column")) {
  by <- match.arg(by)
  row.sums <- rowSums(logmap)
  if (any(row.sums > 1)) {
    stop("Cannot concatenate layers by ", by, " with intersected entries.")
  }
  return(invisible(NULL))
}
