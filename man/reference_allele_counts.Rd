% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_conversion.R
\name{reference_allele_counts}
\alias{reference_allele_counts}
\title{Tabulate occurrences of all observed alleles in reference genetic data}
\usage{
reference_allele_counts(D, pop_level = "collection")
}
\arguments{
\item{D}{A data frame containing, at minimum, a column of sample group identifiers named
"collection", a column designating each row as "reference" or "mixture", named "sample_type",
and (from tcf2long output) locus, gene copy, and observed alleles. If higher-level reporting
unit counts are desired, must have a column of reporting unit identifiers named "repunit"}

\item{pop_level}{a character vector expressing the population level for which allele counts
should be tabulated. Set to "collection" for collection/underlying sample group (default),
or "repunit" for reporting unit/overlying sample groups}
}
\value{
\code{reference_allele_counts} returns a long-format dataframe, with count data for
each collection, locus, and allele. Counts are only drawn from "reference" samples; alleles
unique to the "mixture" samples will still appear in the list, but will have 0s for all groups.
}
\description{
Takes the first output of \code{tcf2long}, along with two columns named "collection" and "sample_type",
and returns a data frame of allele counts for each locus within each reference population.
Alleles to be counted are identified from both reference and mixture populations.
}
\details{
The "collection" column should be a key assigning samples to the desired groups,
e.g. collection site, run time, year.
The "sample_type" column must contain either "reference" or "mixture" for each sample.
}
\examples{
## count alleles in alewife reference populations
example(tcf2long)  # gets variable ale_long
ale_rac <- reference_allele_counts(ale_long$long)

}
\keyword{internal}
