#' Confidence interval for a CCR object
#' @export

confint.CCR <- function(CCR.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    if(CCR.object$equality != "poisson")
        stop("confint method only enabled for poisson equality CCR at the moment")
#
#     dists <- CCR.object$expdist
#     obs <- CCR.object$obs
#
#     ccrlist <- lapply(dists, function(x) ccr_primitive(obs = obs, exp = x))
#     ccrlistmeans <- sapply(ccrlist, mean)

    ci <- quantile(CCR.object$ccrvec, a)

    return(ci)

}
