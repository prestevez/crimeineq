#' Print method for a CCR object
#' @export

print.mcpois <- function(CCR.object)
{
    results <- "Cumulative Concentration Ratio ("
    results <- paste0(CCR.object$equality, " equality)\n")

    cat(results)
    print(CCR.object$ccr)

}
