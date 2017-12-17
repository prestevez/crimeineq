#' Add text of CCR quantile
#' @export
#'

text.CCR.quant <- function(CCR.quant.object, pos = 4, ...)
{
    q <- CCR.quant.object$q
    est <- CCR.quant.object$estimate
    qest <- q/est

    label <- substitute(paste(Y[i], "=", q, "%"), list(q = q * 100))
    text(x = qest, y = q, label, pos = pos, ...)

    label2 <- substitute(paste(Y*minutes[Y[i]], "=", qest, "%"), list(qest = qest * 100))
    text(x = qest, y = qest, label2, pos = pos, ...)

}
