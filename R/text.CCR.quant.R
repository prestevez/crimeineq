#' Add text of CCR quantile
#' @export
#'

text.CCR.quant <- function(CCR.quant.object, pos1 = 4, pos2 = 1, ...)
{
    q <- CCR.quant.object$q
    est <- CCR.quant.object$estimate
    qest <- q/est

    label <- substitute(paste(Y[i], "=", q, "%"), list(q = q * 100))
    text(x = qest, y = q, label, pos = pos1, ...)

    label2 <- substitute(paste(Y*minute[Y[i]], "=", qest, "%"), list(qest = round(qest * 100, 2)))
    text(x = qest, y = qest, label2, pos = pos2, ...)

}
