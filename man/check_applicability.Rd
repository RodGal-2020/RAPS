% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_applicability.R
\name{check_applicability}
\alias{check_applicability}
\title{Check if a rule can be applied}
\usage{
check_applicability(rap, rule_info, verbose = 0)
}
\arguments{
\item{rap}{A \code{rap}.}

\item{rule_info}{A row of \code{rap$Rules} for a given \code{rap} object, \emph{i,e}, everything required to know a certain rule.}

\item{verbose}{The verbosity, between 0 and 5.}
}
\value{
Nothing, only prints a message if everything is OK, and stops the execution otherwise.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Used inside application functions, this checks if a given rule can be applied. The parameter may be a little obscure, as it is used in a very specific moment of the workflow.
}
