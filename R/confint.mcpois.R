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

    ci <- mcpois.object$dists[[whichdists]]
    print(ci)
    names(ci) <- names(whichdists)

    return(ci)
}
