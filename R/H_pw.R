#' Computes Cumulative Hazard at Duration t under a Piecewise Exponential Model
#' 
#' Computes cumulative hazard at duration t from piecewise-constant rates. 
#' 
#' 
#' @param t Duration at which cumulative hazard is required. It may be a vector of durations. 
#' @param breakpoints Breakpoints: values of time at which piecewise-constant
#' rates change.
#' @param rates Piecewise-constant rates
#' @return Cumulative hazard at duration t
#' @seealso functions pw_root() and r_pw_exp(): Function H_pw() is called by pw_root(), which is called by r_pw_exp().
#' 
#' @examples
#' # Example 1
#' breakpoints <- c(0, 10, 20, 30, 60)
#' rates <- c(0.01,0.02,0.04,0.15)
#' z <- VirtualPop::H_pw(t=0:40, breakpoints=breakpoints, rates=rates)
#'
#' # Example 2
#' utils::data(rates,package="VirtualPop")
#' ages <- as.numeric(rownames(rates$ASDR))
#' breakpoints <- c(ages,120)
#' zz <- VirtualPop::H_pw(t=ages, breakpoints=breakpoints, rates=rates$ASDR[,1])
#' 
#' 
#' @export H_pw
H_pw <-
function (t, breakpoints, rates) 
{
    lent <- length(t)
    cumhaz <- vector(mode = "numeric", length = lent)
    for (i in 1:lent) {
        int <- findInterval(t[i], breakpoints, all.inside = TRUE)
        z <- t[i] - breakpoints[1:int]
        exposure <- c(-diff(z), z[int])
        kk <- rates[1:int] * exposure
        cumhaz[i] <- sum(kk)
    }
    return(cumhaz)
}
