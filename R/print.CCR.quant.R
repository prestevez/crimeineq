#' Print method for CCR.quant objects
#' @export

print.CCR.quant <- function(CCR.quant.object)
{
    msg <- "CCR ("
    msg <- paste0(msg, CCR.quant.object$q*100, "% events)\n")

    cat(msg)
    print(CCR.quant.object$estimate)
}

