% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_demo_dataset.R
\name{load_demo_dataset}
\alias{load_demo_dataset}
\title{Load one of the demo datasets}
\usage{
load_demo_dataset(dataset = NULL)
}
\arguments{
\item{dataset}{The code of one of the possible datasets. Run \code{load_demo_dataset()} to get all the possible datasets.}
}
\value{
A \code{rap} object, selected from a group of possible demos. Run \code{load_demo_dataset()} to get all the possible datasets.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Returns a demo \code{rap} object. Run \code{load_demo_dataset()} to get all the possible datasets.
}
\examples{
# vload_demo_dataset()
# my_rap = load_demo_dataset("FAS")
# show_rap(my_rap)

}
