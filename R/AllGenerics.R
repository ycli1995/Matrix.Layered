
# Sparse logical map ###########################################################

#' Create a sparse logical map
#'
#' Create a container for storing mappings of values using sparse logical
#' matrices. Each row specifies whether a value (row name) is present in which
#' observations (columns).
#'
#' @param x A character vector
#' @param ... `r .dot_param`
#'
#' @returns
#' An `r .doc_links("nsparseMatrix")` with row number equal to length of `x`,
#' representing whether each value (row) in `x` exists in each observation
#' (colum).
#'
#' @examples
#' # Create a sparse logical map
#' map <- sparseLogMap(LETTERS)
#'
#' @export sparseLogMap
sparseLogMap <- function(x, ...) {
  UseMethod(generic = "sparseLogMap", object = x)
}

#' Helper functions for sparse logical map.
#'
#' Manipulation functions for sparse logical map, including getting or setting
#' elements, selecting intersected values and drop empty mappings.
#'
#' @examples
#' # Create a logical map for known observations
#' obs <- letters[1:4]
#' map <- sparseLogMap(LETTERS, ncol = length(obs), names = obs)
#' map
#'
#' # Set entries to a new column
#' map <- setMappedEntries(map, "e", sample(LETTERS, 10))
#' map
#'
#' # Set 'c' to NULL
#' map <- setMappedEntries(map, "c", sample(LETTERS, 10))
#' map
#'
#' map <- setMappedEntries(map, "c", NULL)
#' map
#'
#' # Set entries to an existing column
#' map <- setMappedEntries(map, "a", sample(LETTERS, 10))
#' map
#'
#' # Get entries that exist in one observation
#' getMappedEntries(map, 1)
#' getMappedEntries(map, "b")
#'
#' # Which entries are intesected along all observation
#' whichIntersect(map)
#'
#' # Drop entries that doesn't exist in any observation
#' dropEmpty(map)
#'
#' # Also drop empty observations
#' dropEmpty(map, drop.columns = TRUE)
#'
#' @name logmap-helpers
NULL

#' @param x A sparse logical map. Typically an `r .doc_links("nsparseMatrix")`.
#' @param ... `r .dot_param`
#'
#' @returns
#' \itemize{
#' \item `dropEmpty`: An updated sparse logical map with empty entries (and
#' observations) dropped.
#' }
#'
#' @rdname logmap-helpers
#' @export dropEmpty
dropEmpty <- function(x, ...) {
  UseMethod(generic = "dropEmpty", object = x)
}

#' @returns
#' \itemize{
#' \item `whichIntersect`: An integer vector specifying which values exist in
#' more than one observations.
#' }
#'
#' @rdname logmap-helpers
#' @export whichIntersect
whichIntersect <- function(x, ...) {
  UseMethod(generic = "whichIntersect", object = x)
}

#' @param i Get or set `value` to which observation. Must be either an integer
#' or a character (observation name).
#'
#' @returns
#' \itemize{
#' \item `getMappedEntries`: A character vector specifying values that exist in
#' observation `i`.
#' }
#'
#' @rdname logmap-helpers
#' @export getMappedEntries
getMappedEntries <- function(x, i, ...) {
  UseMethod(generic = "getMappedEntries", object = x)
}

#' @param value Values to record in the map for `i`. Must be one of
#' the followings:
#' \itemize{
#' \item A character vector: Set these entries to `TRUE` for `i`.
#' \item `NULL`: Set all values to `FALSE` for `i`.
#' }
#'
#' @returns
#' \itemize{
#' \item `setMappedEntries`: An updated sparse logical map with mappings for
#' observation `i` modified.
#' }
#'
#' @rdname logmap-helpers
#' @export setMappedEntries
setMappedEntries <- function(x, i, value, ...) {
  UseMethod(generic = "setMappedEntries", object = x)
}

# Matrix layers ################################################################

#' Get or set layer data
#'
#' Methods to get or set layer data for a \code{\link{MatrixLayers}}.
#'
#' @param object A \code{\link{MatrixLayers}} object.
#' @param ... `r .dot_param`
#'
#' @name MatrixLayers-accessors
NULL

#' @param i A single character or integer to specify the layer to get or set.
#'
#' @returns
#' \itemize{
#' \item `getLayer`: A matrix-like object for layer `i`.
#' }
#'
#' @rdname MatrixLayers-accessors
#' @export getLayer
getLayer <- function(object, i, ...) {
  UseMethod(generic = "getLayer", object = object)
}

#' @param new.layer A matrix-like object to add as a new layer `i`.
#'
#' @returns
#' \itemize{
#' \item `setLayer`: An updated `object` with `value` saved as layer `i`
#' }
#'
#' @rdname MatrixLayers-accessors
#' @export setLayer
setLayer <- function(object, i, new.layer, ...) {
  UseMethod(generic = "setLayer", object = object)
}

#' @param layers A vector of characters or integers to specify which layers to
#' get or set.
#'
#' @returns
#' \itemize{
#' \item `getLayers`: A list of fetched layers.
#' }
#'
#' @rdname MatrixLayers-accessors
#' @export getLayers
getLayers <- function(object, layers, ...) {
  UseMethod(generic = "getLayers", object = object)
}

#' @param new.layers A list of matrix-like objects to set. Must has the same
#' length as `layers`
#'
#' @returns
#' \itemize{
#' \item `setLayers`: An updated `object` with `layers` are set or replaced with
#' `new.layers`.
#' }
#'
#' @rdname MatrixLayers-accessors
#' @export setLayers
setLayers <- function(object, layers, new.layers, ...) {
  UseMethod(generic = "setLayers", object = object)
}

#' Get or set the default layer
#'
#' Methods to get or set the index of default layer for a
#' \code{\link{MatrixLayers}}.
#'
#' @param object A \code{\link{MatrixLayers}} object.
#' @param ... `r .dot_param`
#'
#' @name defaultLayer
NULL

#' @returns
#' \itemize{
#' \item `defaultLayer`: A single integer specifying which layer is the default.
#' }
#'
#' @rdname defaultLayer
#' @export defaultLayer
defaultLayer <- function(object, ...) {
  UseMethod(generic = "defaultLayer", object = object)
}

#' @param value A character or an integer specifying which layer should be the
#' new default. For a character, layers in `x` must contain names.
#'
#' @returns
#' \itemize{
#' \item `defaultLayer<-`: `object` with updated default layer.
#' }
#'
#' @rdname defaultLayer
#' @export defaultLayer<-
"defaultLayer<-" <- function(object, ..., value) {
  UseMethod(generic = "defaultLayer<-", object = object)
}

#' Join matrix layers
#'
#' Join matrix layers together, by either rows or columns.
#'
#' @param object A \code{\link{MatrixLayers}} object.
#' @param ... `.dot_param`
#'
#' @returns
#' A joined matrix-like object, whose row names and column names are ordered
#' according to the dimension maps.
#'
#' @export joinLayers
joinLayers <- function(object, ...) {
  UseMethod(generic = "joinLayers", object = object)
}

# Concatenate matrices #########################################################

#' Concatenate matrices
#'
#' Methods to concatenate matrix-like objects by rows or columns.
#'
#' @param x A matrix-like object
#' @param y A single or a list of matrix-like object(s) to be merged.
#' @param ... `r .dot_param`
#'
#' @details
#' When concatenate matrices by rows, the union of original column names will be
#' used. Any duplicated row names among matrices will be enforced unique by
#' adding suffixes with \code{\link{make.unique}}.
#'
#' When concatenate matrices by columns, use the opposite way.
#'
#' @returns
#' A concatenated matrix-like object.
#'
#' @name concat-matrix
NULL

#' @rdname concat-matrix
#' @export concatByRows
concatByRows <- function(x, y, ...) {
  UseMethod(generic = "concatByRows", object = x)
}

#' @rdname concat-matrix
#' @export concatByCols
concatByCols <- function(x, y, ...) {
  UseMethod(generic = "concatByCols", object = x)
}
