#' Print method for a CCR object
#' @export

print.CCR <- function(CCR.object)
{
    results <- "CCR ("
    results <- paste0(results, CCR.object$equality, " equality)\n")

    cat(results)
    print(CCR.object$ccr)

}
