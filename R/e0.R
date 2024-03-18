#' Mean Ages at Death and Probabilities of Surviving to Selected Ages, by Sex
#'
#' Computes (a) Life expectancy at birth, (b) Probability of surviving at age
#' 65, and (c) Probability of surviving at age 85
#' 
#' @param d The name of the database. If missing, dLH is used if it exists.
#' @return \item{e0}{Mean ages at death} \item{Prob65}{Probability of surviving
#' at age 65} \item{Prob85}{Probability of surviving at age 85}
#' @examples
#' 
#' 
#' utils::data(dLH,package="VirtualPop")
#' e0(d=dLH)
#' 
#' 
#' @export e0
e0 <- function (d)
{ 
d <- base::subset(d,d$gen==1)
 e0 <-  aggregate(x=d$x_D,by=list(age=d$sex),mean)

# Prob of surviving at age 65, by sex
dd <- base::subset (d,d$x_D>=65)
aggregate(x=d$ID,by=list(age=d$sex),function(x) length(x))
table (d$sex)
table (dd$sex)
p65 <- table (dd$sex) / table (d$sex)

dd <- base::subset (d,d$x_D>=85)
p85 <- table (dd$sex) / table (d$sex)

# Other approach
mean(d$x_D[d$gen==1 & d$sex=="Female"])
mean(d$x_D[d$gen==1 & d$sex=="Male"])

return (list (e0=e0,
              Prob65=p65,
              Prob85=p85))
}
