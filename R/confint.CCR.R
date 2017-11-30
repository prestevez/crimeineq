#' Confidence interval for a CCR object
#' @export

confint.CCR <- function(CCR.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    if(CCR.object$equality != "poisson")
        stop("confint method only enabled for poisson equality CCR at the moment")

    # dists <- confint(CCR.object$poisdists)
    #
    # cidists <- list(ccr_primitive(obs = CCR.object$obs, exp = dists[[1]]))
    #
    # cidists[[2]] <- ccr_primitive(obs = CCR.object$obs, exp = dists[[2]])

    dists <- CCR.object$poisdists$dists
    obs <- CCR.object$obs

    ccrlist <- lapply(dists, function(x) ccr_primitive(obs = obs, exp = dists))
    ccrlistmeans <- sapply(ccrlist, mean)

    ci <- quantile(ccrlistmeans, a)

    return(ci)

}
