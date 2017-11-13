#' Confident interval for a mcpois object
#' @export

confint.mcpois <- function(mcpois.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    mcmat <- sapply(mcpois.object$dists, unlist)

    ci <- t(apply(mcmat, 1, quantile, a))
    return(ci)
}
