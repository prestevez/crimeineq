#' Critical values for a normally distributed variance
#' @export

criticalv <- function(avar, conf.level = 0.95,
                      alternative = c("two.sided", "lower", "upper"))
{
    ci_check(conf.level)
    se <- sqrt(avar)
    if(alternative[1] == "lower")
    {
        sig <- conf.level
    }
    if(alternative[1] == "upper")
    {
        sig <- 1 - conf.level
    }
    if(alternative[1] == "two.sided")
    {
        sig <- 1 - conf.level
        sig <- c(sig/2, 1 - sig/2 )
    }

    critical <- se * qnorm(sig)
    cinames <- paste0(sig * 100, "%")

    names(critical) <- cinames
    return(critical)
}
