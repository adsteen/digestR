##' Quantifies linear trends in pfam or fxn with independant variable
##'
##' @param d
##' @param indep.var default "depth"
##' @param param default "frac.reads" should rename "dep.var"
##' @param variable default "pfam", variable by which to subset the data
##' @param prog show a progress bar for the ddply call
##' @param elim.zeroes eliminate pfams which show up as zer0
##' @export

find_trend <- function(d, indep.var="depth", param="frac.reads", variable="pfam", prog="none") {

  # Print notification of progress bar
  if(prog != "none") {
    print("Calculating linear fits")
  }

  # Calculate linear fits

#   # BY DEFAULT ELIMINATE ZEROES
#   # DEBUGGING: DO I REALLY WANT TO DO THIS?
#   if(elim.zeroes & sum(d[ , param]==0) > 0){ # only do this if asked to, AND if there are any zeroes to remove
#     d <- d[-which(d$param==0), ] # remember if which returns zero-length integer, it all goes ot hell - Advanced R has a fix for this
#   }

  fits <- ddply(d, variable, lm_stats, indep.var, param, .progress=prog)

  if(prog != "none") {
    print("Merging fit data with raw data")
  }
  fm <- merge(d, fits, by="pfam")

  fm

}
