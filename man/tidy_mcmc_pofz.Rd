% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eca-internal-funcs.R
\name{tidy_mcmc_pofz}
\alias{tidy_mcmc_pofz}
\title{A helper function to tidy up the PofZ-like output from the gsi_mcmc functions}
\usage{
tidy_mcmc_pofz(input, pname, car_tib, mix_indiv_tib)
}
\arguments{
\item{input}{The output to tidy (i.e.. out$mean$PofZ)}

\item{pname}{the name that you want the parameter to be called in the output}

\item{car_tib}{a tibble with repunit and collection in the order they appear in the output}

\item{mix_indiv_tib}{a tibble with the individuals in the order they appear in the output}
}
\description{
This makes a tidy data frame of stuff, and also changes things back to
factors, if the levels are provided.
}
\keyword{internal}
