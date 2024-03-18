
#' @title Period rates
#'
#' @description Data consisting of period rates of mortality by age and sex and fertility by age and parity, USA 2021
#' 
#' @name rates
#' @docType data
#' @usage data(rates,package="VirtualPop")
#' @format A list of three objects. \describe{
#' \item{ASDR}{Mortality rates} 
#' \item{ASFR}{Fertility rates}
#' \item{ratesM}{Multistate transition rates} }
#' The dataset has three attributes:
#' \itemize{
#'  \item Country
#'  \item Type of rates: period rates or cohort rates
#'  \item Calendar year for which period death rates are used to complete cohort experience in case of incomplete mortality experience (reference year).
#' }
#' @source The data are downloaded from the Human Mortality Database (HMD) and
#' the Human Fertility Database (HFD). Country: USA. Year: 2021
NULL

#' @title Cohort rates
#'
#' @description Cohort rates of mortality by age and sex and fertility by age and parity, USA birth cohort 1964
#' 
#' @name ratesC
#' @docType data
#' @usage data(ratesC,package="VirtualPop")
#' @format A list of three objects. \describe{
#' \item{ASDR}{Mortality rates} 
#' \item{ASFR}{Fertility rates}
#' \item{ratesM}{Multistate transition rates} 
#' }
#' The object returned by the function has five attributes:
#' \itemize{
#'  \item Country
#'  \item type: Type of data (period data or cohort data)
#'  \item cohort: Birth cohort (year of birth)
#'  \item year: Calendar year for which period death rates are used to complete cohort experience in case of incomplete mortality experience (reference year).
#'  \item start_pASDR: Lowest age for which cohort data are missing. The mortality rates of that age and higher ages are borrowed from period data collected in the reference year.
#' }
#' 
#' @source The data are downloaded from the Human Mortality Database (HMD) and
#' the Human Fertility Database (HFD). Country: USA. Cohort: 1964
NULL

#' @title Individual fertility histories based on period data and in the presence of mortality (USA 2021)
#'
#' @description Fertility histories based on period data and in the presence of mortality. 
#' The histories are simulated from age-specific death rates and conditional fertility rates of USA 2021. 
#' 
#' @name dLH
#' @aliases dLH
#' @docType data
#' @usage data(dLH,package="VirtualPop")
#' @format A data frame with data about 7,000 individuals (2000 in initial cohort). \describe{
#' \item{ID}{Identification number} 	
#' \item{gen}{Generation}
#' \item{cohort}{Birth cohort (year of birth)}
#' \item{sex}{Sex. A factor with levels Males and Females}
#' \item{bdated}{Date of birth (decimal date)} 
#' \item{ddated}{Date of death (decimal date)} 
#' \item{x_D}{Age at death (decimal number)} 	
#' \item{IDmother}{ID of mother} 	
#' \item{IDfather}{ID of father}
#' \item{jch}{Child's line number in the nuclear family (household)}
#' \item{IDpartner}{ID of partner}
#' \item{udated}{Date of union formation}
#' \item{nch}{Number of children ever born to the individual}}	
#' The object has four attributes:
#' \itemize{
#'  \item Country
#'  \item type: Type of data used to produce the histories (period data or cohort data)
#'  \item refyear: Calendar year for which period data are used. If cohort data are used, refyear is missing (NA)
#'  \item cohort: Year of birth of cohort for which the data are used. If period data are used, cohort is missing (NA)
#' }
#' 
#' @source The virtual population is produced from period mortality rates by age and period
#' fertility rates by age and parity from the United States 2021. The data are from the Human
#' Mortality Database (HMD) and the Human Fertility Database (HFD). 
NULL

#' @importFrom stats aggregate rbinom runif uniroot
NULL
