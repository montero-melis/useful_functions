# function to plot random effects taken from
# https://stackoverflow.com/questions/13847936/plot-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot-how-to-mak

library("ggplot2")

plot_ranefs <- function (re) {  # re = object of class ranef.mer

	f <- function (x) {
		pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf  <- data.frame(
    	y   = unlist(x)[ord],
    	ci  = 1.96 * se[ord],
    	nQQ = rep(qnorm(ppoints(nrow(x))), ncol(x)),
      ID  = factor(rep(rownames(x), ncol(x))[ord], levels = rownames(x)[ord]),
      ind = gl(ncol(x), nrow(x), labels = names(x))
      )
    p <- ggplot(pDf, aes(nQQ, y)) +
        	facet_wrap(~ ind, scales = "free") +
        	xlab("Standard normal quantiles") +
        	ylab("Random effect quantiles")
    p <- p +
      theme(legend.position = "none") +
      geom_hline(yintercept = 0) +
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci),
                    width = 0, colour = "black") +
      geom_point(size = 1.5, colour = "blue") +
      coord_flip()
    return(p)
  }

  lapply(re, f)
}
