
#' The MatrixLayers class
#'
#' A `MatrixLayers` works like a named list containing multiple matrix-like
#' objects (layers). Each layer must be a two-dimensional object. The potential
#' common names of rows or columns are mapped to each object using sparse
#' logical maps (`rowMap` and `colMap`), so that users can track whether an
#' entry exists in rows or columns for an object.
#'
#' @slot rowMap,colMap An `r .doc_links("nsparseMatrix")` to map row (column)
#' names for each layer.
#' @slot default An integer specifying which layer is the default.
#' @slot data A list storing all matrix-like layers. Row names of each layer
#' must exist in row names of `rowMap`, and so do column names for `colMap`.
#'
#' @name MatrixLayers-class
#' @docType class
#' @exportClass MatrixLayers
setClass(
  "MatrixLayers",
  slots = c(
    rowMap = "nsparseMatrix",
    colMap = "nsparseMatrix",
    default = "integer",
    data = "list"
  )
)

#' @importFrom methods setValidity
setValidity(
  Class = 'MatrixLayers',
  method = function(object) {
    if (length(object) == 0) {
      if (length(object@default) > 0) {
        return("Empty MatrixLayers should not contain default layer")
      }
      if (any(ncol(object@rowMap) > 0, ncol(object@colMap) > 0)) {
        return(
          "Map(s) for rows and columns should contain 0 column ",
          "for empty MatrixLayers"
        )
      }
    }
    .valid_map_layers(object)
    dimnms <- dimnames(object)
    if (!all(lengths(dimnms) == dim(object))) {
      return("Lengths of dimension names should be equal to dimension numbers.")
    }
    .get_valid_layers_names(object)
    for (i in seq_along(object@data)) {
      layer.dimnms <- .get_valid_layer_dimnames(object@data[[i]])
      .valid_mat_by_dimnames(
        mat = object@data[[i]],
        rownames = rownames(object@rowMap),
        colnames = rownames(object@colMap)
      )
    }
    return(TRUE)
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Constructor ##################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @param x A single matrix-like object or a list of multiple matrix-like
#' objects.
#' @param ... `r .dot_param`
#'
#' @examples
#' # Create some sparse matrices
#' mat1 <- Matrix::rsparsematrix(nrow = 20, ncol = 15, density = 0.75)
#' dimnames(mat1) <- list(sample(LETTERS, 20), sample(letters, 15))
#'
#' mat2 <- Matrix::rsparsematrix(nrow = 15, ncol = 12, density = 0.75)
#' dimnames(mat2) <- list(sample(LETTERS, 15), sample(letters, 12))
#'
#' mat3 <- Matrix::rsparsematrix(nrow = 25, ncol = 10, density = 0.75)
#' dimnames(mat3) <- list(sample(LETTERS, 25), sample(letters, 10))
#'
#' # Create a MatrixLayers with only one matrix
#' mats <- MatrixLayers(mat1)
#' mats
#'
#' # Create a MatrixLayers with multiple matrices
#' mats <- MatrixLayers(list(mat1, mat2, mat3))
#' mats
#'
#' mats <- MatrixLayers(list(m1 = mat1, m2 = mat2, m3 = mat3))
#' mats
#'
#' @rdname MatrixLayers-class
#' @export MatrixLayers
MatrixLayers <- function(x, ...) {
  UseMethod(generic = "MatrixLayers", object = x)
}

#' @rdname MatrixLayers-class
#' @export
#' @method MatrixLayers default
MatrixLayers.default <- function(x, ...) {
  MatrixLayers.list(list(x), ...)
}

#' @param rownames,colnames Known row names or column names. If set to `NULL`,
#' will use the union dimension names of input matrices. Otherwise, each matrix
#' will be subset according to the input `rownams` and `colnames`.
#' @param default Which matrix in the input list should be the default layer.
#' Default is the first.
#'
#' @rdname MatrixLayers-class
#' @export
#' @method MatrixLayers list
MatrixLayers.list <- function(
    x,
    rownames = NULL,
    colnames = NULL,
    default = NULL,
    ...
) {
  out <- .init_MatrixLayers(
    layers = x,
    rownames = rownames,
    colnames = colnames,
    ...
  )
  if (length(out) == 0) {
    return(out)
  }
  defaultLayer(out) <- default %||% 1L
  return(out)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions #############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S3 methods ###################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @rdname defaultLayer
#' @export
#' @method defaultLayer MatrixLayers
defaultLayer.MatrixLayers <- function(object, ...) {
  object@default
}

#' @rdname defaultLayer
#' @export
#' @method defaultLayer<- MatrixLayers
"defaultLayer<-.MatrixLayers" <- function(object, ..., value) {
  if (length(object) == 0) {
    warning(
      "No layer presented in the MatrixLayers, set 'default' to empty.",
      immediate. = TRUE, call. = FALSE
    )
    object@default <- integer(0L)
    return(object)
  }
  if (length(value) == 0) {
    stop("'default' cannot be empty for a MatrixLayers")
  }
  value <- value[1]
  if (is.character(value)) {
    new.value <- match(value, names(object))
    if (is.na(new.value)) {
      stop("Invalid layer name '", value, "'")
    }
    value <- new.value
  }
  value <- as.integer(value)
  if (value == 0 | value > length(object)) {
    stop("'defaultLayer' out of bounds")
  }
  object@default <- value
  return(object)
}

#' @rdname MatrixLayers-accessors
#' @export
#' @method getLayer MatrixLayers
getLayer.MatrixLayers <- function(object, i, ...) {
  if (is_missing(i)) {
    i <- defaultLayer(object)
  }
  if (length(i) == 0) {
    return(NULL)
  }
  .get_layer(object, i)
}

#' @rdname MatrixLayers-accessors
#' @export
#' @method setLayer MatrixLayers
setLayer.MatrixLayers <- function(object, i, new.layer, ...) {
  if (is_missing(i)) {
    i <- defaultLayer(object)
  }
  if (length(i) == 0) {
    i <- defaultLayer(object)
  }
  if (length(i) == 0) {
    if (is.null(new.layer)) {
      return(object)
    }
    i <- 1L
    object@default <- 1L
  }
  .set_layer(object, i, new.layer)
}

## Methods for R-defined generics ##############################################

#' Get or set layer data
#'
#' Getting or setting layer data for a \code{\link{MatrixLayers}} using `$.
#'
#' @param x A \code{\link{MatrixLayers}} object
#' @param pattern A regular expression. Only matching names are returned.
#'
#' @returns
#' \itemize{
#' \item `.DollarNames`: The layer name matches for `pattern`.
#' }
#'
#' @seealso [utils::.DollarNames()]
#'
#' @importFrom utils .DollarNames
#'
#' @name MatrixLayers-dollar
#' @export
#' @method .DollarNames MatrixLayers
.DollarNames.MatrixLayers <- function(x, pattern = '') {
  return(.DollarNames(x@data, pattern = pattern))
}

#' @param i Name of layer data to get or set
#'
#' @returns
#' \itemize{
#' \item `$`: A matrix-like object for layer `i`.
#' }
#'
#' @keywords internal
#'
#' @rdname MatrixLayers-dollar
#' @export
#' @method $ MatrixLayers
"$.MatrixLayers" <- function(x, i) getLayer(x, i)

#' @param value A matrix-like object to add as a new layer `i`.
#'
#' @returns
#' \itemize{
#' \item `$<-`: An updated `x` with `value` saved as layer `i`
#' }
#'
#' @keywords internal
#'
#' @rdname MatrixLayers-dollar
#' @export
#' @method $<- MatrixLayers
"$<-.MatrixLayers" <- function(x, i, value) setLayer(x, i, value)

#' Get or set layer names
#'
#' Methods to get or set layer names for a \code{\link{MatrixLayers}}.
#'
#' @param x A \code{\link{MatrixLayers}} object.
#'
#' @returns
#' \itemize{
#' \item `names`: A character vector specifying layer names.
#' }
#'
#' @name MatrixLayers-names
#' @export
#' @method names MatrixLayers
names.MatrixLayers <- function(x) {
  return(names(x@data))
}

#' @param value The new layer namse to set.
#'
#' @returns
#' \itemize{
#' \item `names<-`: An updated `x` with new layer names.
#' }
#'
#' @rdname MatrixLayers-names
#' @export
#' @method names<- MatrixLayers
"names<-.MatrixLayers" <- function(x, value) {
  colnames(x@rowMap) <- value
  colnames(x@colMap) <- value
  names(x@data) <- value
  return(x)
}

#' Number of layers
#'
#' Methods to get the number of layers in a \code{\link{MatrixLayers}}.
#'
#' @param x A \code{\link{MatrixLayers}} object.
#'
#' @returns
#' An integer indicating the number of layers.
#'
#' @export
#' @method length MatrixLayers
length.MatrixLayers <- function(x) {
  return(length(x@data))
}

#' Dimensions of MatrixLayers
#'
#' Methods to get or set the dimension names for a \code{\link{MatrixLayers}}.
#'
#' @param x A \code{\link{MatrixLayers}} object.
#'
#' @name MatrixLayers-dim
NULL

#' @returns
#' \itemize{
#' \item `dim`: A two-element integer vector indicating the numbers of rows
#' and columns for `x`.
#' }
#'
#' @rdname MatrixLayers-dim
#' @export
#' @method dim MatrixLayers
dim.MatrixLayers <- function(x) {
  return(c(nrow(x@rowMap), nrow(x@colMap)))
}

#' @returns
#' \itemize{
#' \item `dimnames`: A two-element character list indicating the row and
#' column names for `x`.
#' }
#'
#' @rdname MatrixLayers-dim
#' @export
#' @method dimnames MatrixLayers
dimnames.MatrixLayers <- function(x) {
  return(list(rownames(x@rowMap), rownames(x@colMap)))
}

#' @param value A two-element list, where the first character vector specifies
#' new row names and the second specifies new column names.
#'
#' @returns
#' \itemize{
#' \item `dimnames<-`: An updated `x` with new row and column names.
#' }
#'
#' @importFrom rlang is_bare_list
#' @importFrom fastmatch fmatch
#' @importFrom methods validObject
#'
#' @rdname MatrixLayers-dim
#' @export
#' @method dimnames<- MatrixLayers
"dimnames<-.MatrixLayers" <- function(x, value) {
  msg <- "Invalid 'dimnames' given for a MatrixLayers. "
  if (!is_bare_list(value, n = 2L)) {
    stop(msg, call. = FALSE)
  }
  if (!all(lengths(value) == dim(x))) {
    stop(msg, call. = FALSE)
  }
  old.dimnames <- dimnames(x)
  rownames(x@rowMap) <- value[[1L]]
  rownames(x@colMap) <- value[[2L]]
  for (i in seq_along(x@data)) {
    rownames(x@data[[i]]) <- value[[1L]][fmatch(
      rownames(x@data[[i]]),
      old.dimnames[[1L]]
    )]
    colnames(x@data[[i]]) <- value[[2L]][fmatch(
      colnames(x@data[[i]]),
      old.dimnames[[2L]]
    )]
  }
  validObject(x)
  return(x)
}

#' Subset a MatrixLayers
#'
#' Methods to get or set subset of a \code{\link{MatrixLayers}}.
#'
#' @param x A \code{\link{MatrixLayers}} object
#' @param ... `r .dot_param`
#'
#' @name MatrixLayers-subset
NULL

#' @param i,j Indices specifying elements to extract or replace.
#' @param drop Whether or not to drop the empty layers.
#'
#' @returns
#' \itemize{
#' \item `[`: A subset `x` with selected rows and columns, along with extracted
#' layers.
#' }
#'
#' @rdname MatrixLayers-subset
#' @export
#' @method [ MatrixLayers
"[.MatrixLayers" <- function(x, i = missing_arg(), j = missing_arg(), ..., drop = FALSE) {
  miss.i <- is_missing(i)
  miss.j <- is_missing(j)
  if (all(miss.i, miss.j)) {
    return(x)
  }
  if (!miss.i) {
    x@rowMap <- x@rowMap[i, , drop = FALSE]
  }
  if (!miss.j) {
    x@colMap <- x@colMap[j, , drop = FALSE]
  }
  if (any(nrow(x@rowMap) == 0, nrow(x@colMap) == 0)) {
    if (drop) {
      x@rowMap <- x@rowMap[, integer(), drop = FALSE]
      x@colMap <- x@colMap[, integer(), drop = FALSE]
      x@data <- list()
      x@default <- integer(0L)
      return(x)
    }
  }
  keep <- logical(length(x))
  for (i in seq_along(x@data)) {
    x@data[[i]] <- .subset_mat_by_dimnames(
      x@data[[i]],
      getMappedEntries(x@rowMap, i),
      getMappedEntries(x@colMap, i)
    )
    if (prod(dim(x@data[[i]])) > 0) {
      keep[i] <- TRUE
    }
  }
  if (!drop) {
    return(x)
  }
  old.default <- x@default
  x@data <- x@data[keep]
  x@rowMap <- x@rowMap[, keep, drop = FALSE]
  x@colMap <- x@colMap[, keep, drop = FALSE]
  keep.idx <- which(keep)
  x@default <- match(old.default, keep.idx)
  return(x)
}

#' @returns
#' \itemize{
#' \item `[[`: The selected matrix-like object as layer `i`, whose row names and
#' column names have the same orders as the layer maps.
#' }
#'
#' @rdname MatrixLayers-subset
#' @export
#' @method [[ MatrixLayers
"[[.MatrixLayers" <- function(x, i, j, ..., drop = FALSE) {
  .get_layer(x, i)
}

#' @export
#' @method as.list MatrixLayers
as.list.MatrixLayers <- function(x, ...) {
  return(x@data)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# S4 methods ###################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @param value A new matrix-like object to be set into layer `i`.
#'
#' @returns
#' \itemize{
#' \item `[[<-`: An updated `x` with `value` set to layer `i`.
#' }
#'
#' @rdname MatrixLayers-subset
#' @export
setMethod(
  f = '[[<-',
  signature = c(
    x = 'MatrixLayers',
    i = 'integer',
    j = 'missing',
    value = 'ANY'
  ),
  definition = function(x, i, ..., value) {
    setLayer(x, i, new.layer = value)
  }
)

#' @rdname MatrixLayers-subset
#' @export
setMethod(
  f = '[[<-',
  signature = c(
    x = 'MatrixLayers',
    i = 'numeric',
    j = 'missing',
    value = 'ANY'
  ),
  definition = function(x, i, ..., value) {
    i <- as.integer(i)
    setLayer(x, i, new.layer = value)
  }
)

#' @rdname MatrixLayers-subset
#' @export
setMethod(
  f = '[[<-',
  signature = c(
    x = 'MatrixLayers',
    i = 'character',
    j = 'missing',
    value = 'ANY'
  ),
  definition = function(x, i, ..., value) {
    setLayer(x, i, new.layer = value)
  }
)

#' @importFrom utils head
setMethod(
  f = "show",
  signature = "MatrixLayers",
  definition = function(object) {
    cat(
      "A", nrow(object), "x", ncol(object), "MatrixLayers with",
      length(object), "layers\n"
    )
    if (nrow(object) > 0) {
      cat(
        "Row names", paste0("(", nrow(object), "):"),
        paste(head(rownames(object)), collapse = ", "), "...\n"
      )
    }
    if (ncol(object) > 0) {
      cat(
        "Column names", paste0("(", ncol(object), "):"),
        paste(head(colnames(object)), collapse = ", "), "...\n"
      )
    }
    if (length(object) > 0) {
      nms <- names(object)
      if (length(nms) == 0) {
        nms <- seq_len(length(object))
      }
      cat("Default layer:", nms[defaultLayer(object)], "\n")
      cat("Layers", paste0("(", length(object), ")"), ":\n")
      for (i in seq_along(object@data)) {
        cat(
          paste0(" ", nms[i], ":"),
          "A", nrow(object@data[[i]]), "x", ncol(object@data[[i]]),
          class(object@data[[i]])[1], "\n"
        )
      }
    }
    return(invisible(x = NULL))
  }
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.valid_mat_by_dimnames <- function(mat, rownames, colnames) {
  check.rows <- all(rownames(mat) == rownames)
  check.cols <- all(colnames(mat) == colnames)
  if (any(!check.rows, !check.cols)) {
    stop("Invalid dimension names for the input matrix-like object.")
  }
  invisible(NULL)
}

.valid_map_layers <- function(object) {
  n.layers <- length(object)
  if (any(ncol(object@rowMap) != n.layers, ncol(object@colMap) != n.layers)) {
    stop("Unmatched layers and maps in a MatrixLayers.")
  }
  return(invisible(NULL))
}

.get_valid_layers_names <- function(object) {
  err.msg <- "Maps contain invalid column names for a MatrixLayers."
  nms <- names(object)
  nms.rowmap <- colnames(object@rowMap)
  nms.colmap <- colnames(object@colMap)
  if (length(nms) == 0) {
    if (all(length(nms.rowmap) == 0, length(nms.colmap) == 0)) {
      return(nms)
    }
    stop(err.msg)
  }
  check.rowmap <- all(nms == nms.rowmap)
  check.colmap <- all(nms == nms.colmap)
  if (any(!check.colmap, check.rowmap)) {
    stop(err.msg)
  }
  return(nms)
}

.get_valid_layer_dimnames <- function(mat) {
  dimnms <- dimnames(mat)
  if (any(lengths(dimnms) == 0)) {
    stop("The matrix-like object must contain both row and column names.")
  }
  for (i in seq_along(dimnms)) {
    if (anyDuplicated(dimnms[[i]])) {
      stop("The matrix-like object contains duplicated dimension names.")
    }
  }
  return(dimnms)
}

.empty_MatrixLayers <- function() {
  new(
    "MatrixLayers",
    rowMap = sparseLogMap(character()),
    colMap = sparseLogMap(character())
  )
}

.init_MatrixLayers <- function(layers, rownames = NULL, colnames = NULL, ...) {
  ncol <- length(layers)
  out <- .empty_MatrixLayers()
  if (ncol == 0) {
    return(out)
  }
  layer.names <- names(layers)
  all.rownms <- all.colnms <- list()
  for (i in seq_along(layers)) {
    layer.dimnms <- .get_valid_layer_dimnames(layers[[i]])
    all.rownms[[i]] <- layer.dimnms[[1]]
    all.colnms[[i]] <- layer.dimnms[[2]]
    if (length(rownames) > 0) {
      all.rownms[[i]] <- intersect(rownames, all.rownms[[i]])
    }
    if (length(colnames) > 0) {
      all.colnms[[i]] <- intersect(colnames, all.colnms[[i]])
    }
    layers[[i]] <- .subset_mat_by_dimnames(
      layers[[i]],
      rownames = all.rownms[[i]],
      colnames = all.colnms[[i]]
    )
  }
  all.rownms <- all.rownms %>%
    unlist() %>%
    unique()
  all.colnms <- all.colnms %>%
    unlist() %>%
    unique()
  rowmap <- sparseLogMap(all.rownms, ncol = ncol, names = layer.names)
  colmap <- sparseLogMap(all.colnms, ncol = ncol, names = layer.names)
  for (i in seq_along(layers)) {
    rowmap <- setMappedEntries(rowmap, i, rownames(layers[[i]]))
    colmap <- setMappedEntries(colmap, i, colnames(layers[[i]]))
  }
  out@rowMap <- rowmap
  out@colMap <- colmap
  out@data <- layers
  return(out)
}

#' @importMethodsFrom MatrixExtra [
.subset_mat_by_dimnames <- function(mat, rownames, colnames) {
  check.rows <- length(rownames) > 0
  check.cols <- length(colnames) > 0
  if (!check.rows) {
    if (!check.cols) {
      return(mat[integer(), integer(), drop = FALSE])
    }
    return(mat[integer(), colnames, drop = FALSE])
  }
  if (!check.cols) {
    return(mat[rownames, integer(), drop = FALSE])
  }
  check.rows <- all(suppressWarnings(rownames(mat) == rownames))
  check.cols <- all(suppressWarnings(colnames(mat) == colnames))
  if (check.rows) {
    if (!check.cols) {
      mat <- mat[, colnames, drop = FALSE]
    }
    return(mat)
  }
  if (check.cols) {
    return(mat[rownames, , drop = FALSE])
  }
  mat[rownames, colnames, drop = FALSE]
}

.get_layer <- function(object, i) {
  rownms <- getMappedEntries(object@rowMap, i)
  colnms <- getMappedEntries(object@colMap, i)
  layer <- as.list(object)[[i]]
  .subset_mat_by_dimnames(layer, rownms, colnms)
}

.set_layer <- function(object, i, new.layer) {
  if (is.null(new.layer)) {
    object@rowMap <- setMappedEntries(object@rowMap, i, NULL)
    object@rowMap <- setMappedEntries(object@rowMap, i, NULL)
    object@data[[i]] <- new.layer
    if (length(object) == 0) {
      object@default <- integer()
    }
    return(object)
  }
  layer.dimnms <- .get_valid_layer_dimnames(new.layer)
  object@rowMap <- setMappedEntries(object@rowMap, i, layer.dimnms[[1]])
  object@colMap <- setMappedEntries(object@colMap, i, layer.dimnms[[2]])
  new.layer <- .subset_mat_by_dimnames(
    new.layer,
    rownames = getMappedEntries(object@rowMap, i),
    colnames = getMappedEntries(object@colMap, i)
  )
  object@data[[i]] <- new.layer
  return(object)
}
