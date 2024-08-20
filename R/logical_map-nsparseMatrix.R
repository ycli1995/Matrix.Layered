
#' @param ncol Number of column (observation) for the output matrix.
#' @param names Names of observations. Must have `ncol` elements.
#'
#' @examples
#' # Create a logical map for known observations
#' obs <- letters[1:4]
#' map <- sparseLogMap(LETTERS, ncol = length(obs), names = obs)
#' map
#'
#' @importFrom Matrix Matrix
#' @importClassesFrom Matrix nsparseMatrix
#'
#' @rdname sparseLogMap
#' @export
#' @method sparseLogMap character
sparseLogMap.character <- function(x, ncol = 0, names = NULL, ...) {
  out <- Matrix(data = 0, nrow = length(x), ncol = ncol, sparse = TRUE) %>%
    as("nsparseMatrix")
  colnames(out) <- names
  if (length(x) == 0) {
    return(out)
  }
  rownames(out) <- x
  return(out)
}

#' @param drop.columns Whether or not to also drop the empty observations.
#'
#' @importFrom Matrix rowSums colSums
#'
#' @rdname logmap-helpers
#' @export
#' @method dropEmpty nsparseMatrix
dropEmpty.nsparseMatrix <- function(x, drop.columns = FALSE, ...) {
  row.sum <- rowSums(x)
  fidx <- which(row.sum == 0)
  if (length(fidx) > 0) {
    x <- x[-fidx, , drop = FALSE]
  }
  if (!drop.columns) {
    return(x)
  }
  col.sum <- colSums(x)
  fidx <- which(col.sum == 0)
  if (length(fidx) > 0) {
    x <- x[, -fidx, drop = FALSE]
  }
  x
}

#' @rdname logmap-helpers
#' @export
#' @method whichIntersect nsparseMatrix
whichIntersect.nsparseMatrix <- function(x, ...) {
  which(rowSums(x) > 1)
}

#' @importMethodsFrom Matrix which
#'
#' @rdname logmap-helpers
#' @export
#' @method getMappedEntries nsparseMatrix
getMappedEntries.nsparseMatrix <- function(x, i, ...) {
  .valid_logical_map(x)
  rownames(x)[which(x[, i, drop = TRUE])]
}

#' @rdname logmap-helpers
#' @export
#' @method setMappedEntries nsparseMatrix
setMappedEntries.nsparseMatrix <- function(x, i, value, ...) {
  i <- i[1]
  if (is.character(i)) {
    .valid_colnames_logical_map(x)
    idx <- match(i, colnames(x))
    if (is.na(idx)) {
      idx <- ncol(x) + 1L
    }
    return(.set_map_entries(x, idx, value, new.name = i))
  }
  i <- as.integer(i)
  if (length(colnames(x)) > 0 & i > ncol(x)) {
    stop(
      "Must use character 'i' to add new entries for a logical map with ",
      "column names."
    )
  }
  .set_map_entries(x, i, value)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.valid_logical_map <- function(x) {
  if (all(dim(x) == 0)) {
    return(invisible(NULL))
  }
  if (nrow(x) > 0) {
    # Check rownames
    if (length(rownames(x)) == 0) {
      stop("Row names must be supplied for a logical map.")
    }
    if (any(nzchar(rownames(x)) == 0)) {
      stop("Row names cannot be empty strings for a logical map.")
    }
    if (anyDuplicated(rownames(x))) {
      stop("Duplicate row names not allowed")
    }
  }
  if (length(colnames(x)) > 0) {
    if (any(nzchar(colnames(x)) == 0)) {
      stop("column names cannot be empty strings")
    }
    if (anyDuplicated(colnames(x))) {
      stop("Duplicate column names not allowed")
    }
  }
  return(invisible(NULL))
}

.valid_colnames_logical_map <- function(x) {
  if (length(colnames(x)) == 0 & ncol(x) > 0) {
    stop("The logical map doesn't have column names")
  }
  invisible(NULL)
}

#' @importFrom stats na.omit na.action
.set_map_entries <- function(x, i, value, new.name = NULL) {
  if (length(value) > 0) {
    stopifnot(is.character(value))
    stopifnot(!anyDuplicated(value))
  }
  .valid_logical_map(x)
  idx <- match(value, rownames(x)) %>%
    na.omit()
  new.mat <- logical(nrow(x))
  new.mat[idx] <- TRUE
  if (i <= ncol(x)) {
    x[, i] <- new.mat
  } else {
    if (i > ncol(x) + 1) {
      stop("subscript out of bounds for i = ", i)
    }
    new.mat <- as(new.mat, "nsparseMatrix")
    colnames(new.mat) <- new.name
    x <- cbind(x, new.mat)
  }
  not.found <- value[na.action(idx)]
  if (length(not.found) == 0) {
    return(x)
  }
  empty.mat <- Matrix(
    data = 0,
    nrow = length(not.found),
    ncol = ncol(x),
    sparse = TRUE
  ) %>%
    as("nsparseMatrix")
  rownames(empty.mat) <- not.found
  colnames(empty.mat) <- colnames(x)
  not.found <- logical(length(not.found))
  empty.mat[, i] <- TRUE
  x <- rbind(x, empty.mat)
  return(x)
}

