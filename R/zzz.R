
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Roxygen2 calls ###############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.doc_links <- function(nm) {
  pkg <- .pkg_map[nm]
  paste0("\\code{\\link[", pkg, "]{", nm, "}}")
}

.dot_param <- "Arguments passed to other metheds."
.val_param <- "An object of a class specified in the S4 method signature."
.vb_param <- "Print progress."

.pkg_map <- c(
  "nsparseMatrix" = "Matrix:nsparseMatrix-class"
)





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Package ######################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @importFrom methods as callNextMethod new setAs setClass setMethod
#' setValidity
#' @importFrom rlang %||% is_empty is_false is_missing is_true missing_arg
#' @importFrom magrittr %>%
#' @importFrom easy.utils verboseMsg
#' @importFrom Rcpp evalCpp
#' @importMethodsFrom Matrix colSums rowSums t
#' @useDynLib Matrix.Layered
#' @keywords internal
"_PACKAGE"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Hooks ########################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.onLoad <- function(libname, pkgname) {
  options(MatrixExtra.quick_show = FALSE)
  return(invisible(NULL))
}


