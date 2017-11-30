#' quantInv primitive

quantInv <- function(distr, value)
{
    if(value == 1)
    {
        1 - ((length(distr[distr == 1]) - 1) / (length(distr) -1 ))
    }
    else ecdf(distr)(value)
}
