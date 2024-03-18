#' The Function for which the Root is Sought.
#' 
#' The function pw_root() specifies the mathematical function g(t). 
#' The equation to be solved is g(t)=0, with g(t) the cumulative hazard function of the piecewise exponential distribution + 
#' log(u) with u a random draw from standard uniform distribution (see vignette "Piecewise_exponential", Section 2.2.4).
#' 
#' pw_root is an argument of the function uniroot() of base R (argument "f"). It is required by uniroot(). 
#' The function uniroot() is called by
#' r.pw_exp(). See also Functions H_pw() and r.pw_exp().
#' 
#' @param t Vector of durations for which the equation g(t)=0 should be solved.
#' @param breakpoints Breakpoints
#' @param rates Piecewise-constant rates
#' @param uu Random draw from standard uniform distribution.
#' @return Vector of differences between cumulative hazard and -log(uu) for
#' different values of t.
#'
#' @examples
#' breakpoints <- c(0, 10, 20, 30, 60)
#' rates <- c(0.01,0.02,0.04,0.15)
#' z <- VirtualPop::pw_root (t= c(10,18.3,23.6,54.7),breakpoints,rates,uu=0.43)
#' 
#' @export pw_root
pw_root <-
function (t, breakpoints, rates, uu) 
{
    aa <- VirtualPop::H_pw(t, breakpoints, rates) + log(uu)
    return(aa)
}
