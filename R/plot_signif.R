##' Plots significant reads: THIS IS BROKEN
##'
##' @param d a data frame created by read_digestR
##' @param indep.var character
##' @param dep.var character
##' @param variable variable by which to to facet the data
##' @param alpha significance cutoff
##' @export
plot_signif <- function(d, indep.var="depth", dep.var="frac.reads", variable="pfam", alpha=0.05) {

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

  # Identify pfams with significant changes
  sig <- find_signif(d, indep.var=indep.var, dep.var=dep.var, alpha=alpha)

  # Make plot
  p <- ggplot(sig, aes_string(x=indep.var, y=dep.var)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) #+
    #scale_x_reverse() +
    #expand_limits(y=0) +
    #coord_flip() +


  if(indep.var=="depth") {
    p <- p + scale_x_reverse() +
      expand_limits(y=0) +
      coord_flip() +
      facet_wrap(as.formula(paste("~", variable)))
  } else {
    p <- p + facet_wrap(as.formula(paste("~", variable)), scales="free")
  }

  p
}
