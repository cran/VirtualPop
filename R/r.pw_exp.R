#' Draws Waiting Times from a Piecewise-Exponential Distribution.
#' 
#' The function produces n realizations of a piecewise-exponentially distributed random waiting time.
#' 
#' 
#' @param n Number of random draws
#' @param breakpoints Breakpoints in piecewise-exponential distribution
#' @param rates Piecewise-constant rates
#' @return Vector of waiting times, drawn randomly from a piecewise-exponential survival
#' function.
#' @examples
#' 
#' 
#' breakpoints <- c(0, 10, 20, 30, 60)
#' rates <- c(0.01,0.02,0.04,0.15)
#' pw_sample <- VirtualPop::r.pw_exp (n=10, breakpoints, rates=rates)
#' 
#' 
#' @export r.pw_exp
r.pw_exp <-
function (n, breakpoints, rates) 
{
    success = TRUE
    i <- 1
    u <- runif(n)
    interval = c(breakpoints[1], breakpoints[length(breakpoints)])
    x_Dg <- vector(mode = "numeric", length = n)
    while (success == TRUE & i <= n) {
        xx <- base::tryCatch(uniroot(f = pw_root, interval = interval, 
            breakpoints, rates, uu = u[i])$root, error = function(e) z = 5000)
        if (xx == 5000) {
            success <- TRUE
            u[i] <- runif(1)
        } else {
            x_Dg[i] <- xx
            i = i + 1
        }
        x_Dg[i]
    }
    x_Dg
    return(x_Dg)
}
