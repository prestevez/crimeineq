#' Add text of CCR quantile
#' @export
#'

text.CCR.quant <- function(CCR.quant.object, pos1 = 4, pos2 = 4,
                           label1 = NULL, label2 = NULL, ...)
{
    q <- CCR.quant.object$q
    qinv <- CCR.quant.object$qinv
    est <- CCR.quant.object$estimate
    qest <- q/est

    if(is.null(label1))
    {
        label1 <- substitute(paste(Y[i], "=", q, "%"), list(q = q * 100))
    }

    if(is.null(label2))
    {
        label2 <- substitute(paste(Y*minute[paste(Y[i], "=", q, "%")], "=", qest, "%"),
                             list(qest = round(qest * 100, 2), q = q * 100))
    }

    text(x = qinv, y = q, label1, pos = pos1, ...)
    text(x = qinv, y = qest, label2, pos = pos2, ...)

}
