#' Print method for a CCR object
#' @export

print.CCR <- function(CCR.object)
{
    results <- "Cumulative Concentration Ratio ("
    results <- paste0(results, CCR.object$equality, " equality)\n")

    cat(results)
    print(CCR.object$ccr)

}
