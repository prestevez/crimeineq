#' Confident interval for a mcpois object
#' @export

confint.mcpois <- function(mcpois.object, conf.level = 0.95)
{
    ci_check(conf.level)

    a <- (1 - conf.level)/2
    a <- c(a, 1 - a)

    ginis <- sapply(mcpois.object$dists, ineq::Gini)

    criticalginis <- quantile(ginis, a)

    whichdists <- sapply(criticalginis, function(x) closest(ginis, x))

    ci <- list(mcpois.object$dists[[whichdists[1]]])
    ci[[2]] <- mcpois.object$dists[[whichdists[2]]]
    names(ci) <- names(whichdists)
    str(ci)

    return(ci)
}
