
#' @rdname concat-matrix
#' @export
#' @method concatByRows matrix
concatByRows.matrix <- function(x, y, ...) {
  mat.list <- c(list(x), y)
  .concat_matrix(mat.list = mat.list, by = "row")
}

#' @rdname concat-matrix
#' @export
#' @method concatByCols matrix
concatByCols.matrix <- function(x, y, ...) {
  mat.list <- c(list(x), y)
  .concat_matrix(mat.list = mat.list, by = "column")
}

#' @importFrom SeuratObject RowMergeSparseMatrices
#'
#' @rdname concat-matrix
#' @export
#' @method concatByRows CsparseMatrix
concatByRows.CsparseMatrix <- function(x, y, ...) {
  RowMergeSparseMatrices(mat1 = x, mat2 = y)
}

#' @rdname concat-matrix
#' @export
#' @method concatByCols CsparseMatrix
concatByCols.CsparseMatrix <- function(x, y, ...) {
  all.mat <- c(list(x), y)
  all.colnames <- all.rownames <- vector("list", length = length(all.mat))
  for (i in seq_along(all.mat)) {
    all.rownames[[i]] <- rownames(all.mat[[i]])
    all.colnames[[i]] <- colnames(all.mat[[i]])
  }
  use.rbind <- all(duplicated(all.colnames)[2:length(all.colnames)])
  if (is_true(use.rbind)) {
    new.mat <- do.call(rbind, all.mat)
  } else {
    all.names <- unlist(all.colnames) %>% unique()
    new.mat <- col_merge_dgCMatrices_rcpp(
      mat_list = all.mat,
      mat_colnames = all.colnames,
      all_colnames = all.names
    )
    colnames(new.mat) <- make.unique(unlist(all.colnames))
  }
  rownames(new.mat) <- make.unique(unlist(all.rownames))
  return(new.mat)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Helper functions #############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Internal #####################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.concat_matrix <- function(mat.list, by = c("row", "column")) {
  by <- match.arg(by)
  for (i in seq_along(mat.list)) {
    all.rownames[[i]] <- rownames(mat.list[[i]])
    all.colnames[[i]] <- colnames(mat.list[[i]])
  }
  if (by == "row") {
    unique.colnames <- unlist(all.colnames) %>% unique()
    unique.rownames <- unlist(all.rownames) %>% make.unique()
  } else {
    unique.colnames <- unlist(all.colnames) %>% make.unique()
    unique.rownames <- unlist(all.rownames) %>% unique()
  }
  m <- matrix(0, nrow = length(unique.rownames), ncol = length(unique.colnames))
  rownames(m) <- unique.rownames
  colnames(m) <- unique.colnames
  i1 <- 0
  if (by == "row") {
    for (i in seq_along(mat.list)) {
      i1 <- i1 + 1
      i2 <- i1 + nrow(mat.list[[i]])
      m[i1:i2, all.colnames[[i]]] <- as.matrix(mat.list[[i]])
      i1 <- i2
    }
  } else {
    for (i in seq_along(mat.list)) {
      i1 <- i1 + 1
      i2 <- i1 + ncol(mat.list[[i]])
      m[all.rownames[[i]], i1:i2] <- as.matrix(mat.list[[i]])
      i1 <- i2
    }
  }
  return(m)
}
