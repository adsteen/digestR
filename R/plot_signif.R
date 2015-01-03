##' Plots significant reads
##'
plot_signif <- function(d, alpha=0.05) {

  signif <- trends[(trends$pval < 0.05 & !is.na(trends$pval)) , ]
  signif_pfams <- unique(signif$pfam)

  p <- ggplot(d[d$pfam %in% signif_pfams, ], aes(x=depth, y=frac.reads)) +
    geom_point() +
    geom_smooth(method="lm", se=FALSE) +
    scale_x_reverse() +
    expand_limits(y=0) +
    coord_flip() +
    facet_wrap(~pfam)

}
