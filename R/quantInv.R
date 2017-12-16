#' quantInv primitive

quantInv <- function(distr, value)
{
    if(value == 1)
    {
        1 - ((length(distr[distr == 1]) - 1) / (length(distr) -1 ))
    }
    if(value <= 0 | value > 1)
        stop("'value' outside accepted range (0,1]")
    else ecdf(distr)(value)
}
