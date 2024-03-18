#' Generates Individual Lifespan(s)
#'
#' Uses age-specific death rates to simulate length of life. The function generates age(s) at
#' death and date(s) of death. The function uses the function rpexp() of the msm package and uniroot() of base R
#'
#'
#' @param data Data frame with individual data. If the object "data" includes date of 
#' birth (bdated; decimal date), then the date of death is computed.
#' @param ASDR Age-specific death rates
#' @param mort Presence or absence of mortality. This parameter is optional. Default is TRUE. If mortality is (should be) absent, mort=FALSE.
#' @return \item{LS}{Data frame with age(s) at death and date(s) of death}
#' 
#' @examples
#'
#' utils::data(dLH,package="VirtualPop")
#' utils::data(rates,package="VirtualPop")
#' d <- VirtualPop::Lifespan (dLH[1:5,1:5],ASDR=rates$ASDR)
#'
#' @export Lifespan
#' 
Lifespan <-
function (data, ASDR,mort=NULL)
{ if (is.null(mort))  mort <- TRUE
  z <- table(data$sex)
  nmales <- z[1]
  nfemales <- z[2]
  data$ddated <- NA
  data$x_D <- NA
  ages <- c(0:110)
  data$x_D[data$sex == "Male"] <- msm::rpexp(n = nmales, rate = ASDR[,
        "Males"], t = ages)
  data$x_D[data$sex == "Female"] <- msm::rpexp(n = nfemales,
        rate = ASDR[, "Females"], t = ages)
  aggregate(data$x_D, by = list(data$sex), FUN = "mean")
  data$ddated <- data$bdated + data$x_D
    
  if (!mort)
    { data$x_D <- 120
      # Date of death (calendar date)
      data$ddated <- data$bdated + data$x_D
    } 
  LS <- data
  return(LS)
}
