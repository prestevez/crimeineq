#' Confidence interval for a CCR object
#' @export

confint.CCR <- function(CCR.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    if(CCR.object$equality != "poisson")
        stop("confint method only enabled for poisson equality CCR at the moment")

    dists <- confint(CCR.object$poisdists)

    cidists <- sapply(dists, function(x) ccr_primitive(obs = CCR.object$obs,
                                                       exp = x))
    ci <- sapply(cidists, mean)
    names(ci) <- names(dists)

    return(ci)

}
