#' Print method for a mcpois object
#' @export

print.mcpois <- function(mcpois.object)
{
    results <- "Monte Carlo simulation with "
    results <- paste0(results, mcpois.object$reps, " reps.\n")
    results <- paste0(results, "of a Poisson process with rate ", mcpois.object$lambda, "\n")
    results <- paste0("and length ",  mcpois.object$n, ".")

    cat(results)
    print(str(head(mcpois.object$dists)))

}
