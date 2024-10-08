% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MatrixLayers-class.R
\name{MatrixLayers-subset}
\alias{MatrixLayers-subset}
\alias{[.MatrixLayers}
\alias{[[.MatrixLayers}
\alias{[[<-,MatrixLayers,integer,missing,ANY-method}
\alias{[[<-,MatrixLayers,numeric,missing,ANY-method}
\alias{[[<-,MatrixLayers,character,missing,ANY-method}
\title{Subset a MatrixLayers}
\usage{
\method{[}{MatrixLayers}(x, i = missing_arg(), j = missing_arg(), ..., drop = FALSE)

\method{[[}{MatrixLayers}(x, i, j, ..., drop = FALSE)

\S4method{[[}{MatrixLayers,integer,missing,ANY}(x, i, j, ...) <- value

\S4method{[[}{MatrixLayers,numeric,missing,ANY}(x, i, j, ...) <- value

\S4method{[[}{MatrixLayers,character,missing,ANY}(x, i, j, ...) <- value
}
\arguments{
\item{x}{A \code{\link{MatrixLayers}} object}

\item{i, j}{Indices specifying elements to extract or replace.}

\item{...}{Arguments passed to other metheds.}

\item{drop}{Whether or not to drop the empty layers.}

\item{value}{A new matrix-like object to be set into layer \code{i}.}
}
\value{
\itemize{
\item \code{[}: A subset \code{x} with selected rows and columns, along with extracted
layers.
}

\itemize{
\item \code{[[}: The selected matrix-like object as layer \code{i}, whose row names and
column names have the same orders as the layer maps.
}

\itemize{
\item \verb{[[<-}: An updated \code{x} with \code{value} set to layer \code{i}.
}
}
\description{
Methods to get or set subset of a \code{\link{MatrixLayers}}.
}
