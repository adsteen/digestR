##' Quantifies linear trends in pfam or fxn with independant variable
##'
##' @export

find_trend <- function(d, indep.var="depth", param="abs.reads", disc="pfam") {

  fits <- ddply(d, disc, lm_stats, indep.var, param)

}
