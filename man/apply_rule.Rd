% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_rule.R
\name{apply_rule}
\alias{apply_rule}
\title{Apply a rule to a given P system}
\usage{
apply_rule(rap, rule_id, verbose = FALSE, debug = FALSE, keep_residue = FALSE)
}
\arguments{
\item{rap}{A rap object.}

\item{rule_id}{The id of the rule to be applied.}

\item{verbose}{The verbosity, between 0 and 5.}

\item{debug}{Useful if you want to debug the execution of the function.}

\item{keep_residue}{Do you want to delete the objects with multiplicity 0 at the end of the application?}
}
\value{
A new rap object, the result of applying the given rule.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Apply a rule to a given P system given as a \code{rap} object.
}
\section{TODO}{

\itemize{
\item Include examples
}
}

\seealso{
\code{apply_rules} to apply more than one rule at the same time, \code{apply_rule_menv} to apply one rule in a multienvironmental P system.
}
