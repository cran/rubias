

#' Write a mixture data frame to gsi_sim format baseline and repunits file
#'
#' Note, this is only intended to work with integer-valued alleles, at the moment.
#' It was just written for testing and verifying that things are working correctly.
#'
#' @param mix mixture data frame
#' @param gen_start_col column in which the genetic data start
#' @param mixprefix path to write the mixture file to.  The mixture collection name + .txt will
#' be appended to this.  This path can include directories if they exist.  An example
#' would be "./my_gsi_data/mixture". This is a required argument.
#' @export
#' @examples
#' # this writes to file prefix "mixfile" in a temporary directory
#' dd <- tempdir()
#' prefix <- file.path(dd, "mixfile")
#'
#' # print that
#' prefix
#'
#' # note that in practice you will probably want to specify
#' # your own directory...
#'
#' # run the function
#' write_gsi_sim_mixture(chinook_mix, 5, prefix)
#'
#' # see where those files live:
#' dir(dd, pattern = "mixfile*", full.names = TRUE)
write_gsi_sim_mixture <- function(mix, gen_start_col, mixprefix) {

  locus_names <- names(mix)[seq(gen_start_col, ncol(mix), by = 2)]
  loccols <- names(mix)[gen_start_col:ncol(mix)]

  # then write the full mixture
  mix[,loccols][is.na(mix[,loccols])] <- 0  # set missing alleles to 0 rather than NA
  mix_list <- split(mix, mix$collection)

  dump <- lapply(names(mix_list), function(x) {

    mixout <- paste(mixprefix, "-", x, ".txt", sep = "")
    cat(nrow(mix), (ncol(mix) - gen_start_col + 1) / 2, "\n", file = mixout)  # number of indivs and loci on top line
    cat(locus_names, sep = "\n", file = mixout, append = TRUE)
    cat("POP", x, "\n", file = mixout, append = TRUE)
    write.table(mix_list[[x]][, c("indiv", loccols)], sep = "  ", file = mixout, append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  })

}
