#' Add text of CCR quantile
#' @export
#'

text.CCR.quant <- function(CCR.quant.object, pos1 = 4, pos2 = 4, ...)
{
    q <- CCR.quant.object$q
    qinv <- CCR.quant.object$qinv

    est <- CCR.quant.object$estimate
    qest <- q/est

    label <- substitute(paste(Y[i], "=", q, "%"), list(q = q * 100))
    text(x = qinv, y = q, label, pos = pos1, ...)

    label2 <- substitute(paste(Y*minute[Y[i]], "=", qest, "%"), list(qest = round(qest * 100, 2)))
    text(x = qinv, y = qest, label2, pos = pos2, ...)

}
