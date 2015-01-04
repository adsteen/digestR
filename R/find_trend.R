##' Quantifies linear trends in pfam or fxn with independant variable
##'
##' @export

find_trend <- function(d, indep.var="depth", param="frac.reads", disc="pfam", prog="none") {

  fits <- ddply(d, disc, lm_stats, indep.var, param, .progress="none")

  fm <- merge(d, fits, by="pfam")

  fm

}
