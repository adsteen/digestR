##' Finds significant trends
##'
##' @description This is a bit of a placeholder function - later I will come up with a more statistically valid way of finding true trends
##' @param d data frame for which slopes have been calculated
##' @param indep.var independent variable, as a character
##' @param dep.var
##' @param alpha alpha value; defaults to 0.05
##' @export

find_signif <- function(d, alpha) {

  # Test that incoming parameter d is a data frame and has appropriate columns
  #  (meaning pfam, and/or fxn, plus trend and pval)

  # Test that d is a data frame
  if(!("data.frame" %in% class(d))) {
    stop("d must be a data frame")
  }

  # Test that d contains appropriately named columns
  if(!indep.var %in% names(d)) {
    stop(paste("There is no column in d named", indep.var))
  }

  if(!dep.var %in% names(d)) {
    stop(paste("There is no column in d named", dep.var))
  }

  # Determine which trends are significant
  signif <- d[(d$pval < 0.05 & !is.na(d$pval)) , ]
  signif_pfams <- unique(signif$pfam)

  # Return a subset of the original data frame, containing only the data points that are part of a significant trend
  d_signif <- d[d$pfam %in% signif_pfams, ]

  #if(nrow(d_signif) == 0) {
  #  warning(paste("There were no significant trends in ", dep.var, " with respect to ", indep.var))
  #}

  d_signif
}
