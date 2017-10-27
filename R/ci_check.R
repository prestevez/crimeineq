ci_check <- function(ci)
{
    if(length(ci) > 1)
        stop("conf.level accepts only one value.")
    if(ci >= 1| ci <= 0)
        stop("conf.level should be a value between 0 and 1 exclusive.")
}
