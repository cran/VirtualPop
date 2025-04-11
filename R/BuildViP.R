#' Builds a Virtual Population in a Single Step
#' 
#' Builds a virtual population from mortality and fertility rates retrieved from the Human Mortality Database (HMD) and the Human Fertility Database (HFD) in a single step.
#' 
#' @param user User name (e-mail address)
#' @param pw_HMD Password Human Mortality Database
#' @param pw_HFD Password Human Fertility Database
#' @param countrycode Code of country selected
#' @param cohort Birth cohort (for virtual population based on cohort data)
#' @param refyear Reference year (for virtual population based on period data)
#' @param ncohort Size of initial cohort
#' @param ngen Number of generations
#' @param mort Presence or absence of mortality (optional). Default: mortality is present (mort=TRUE). If mortality is absent, mort=FALSE.
#' 
#' @return dLH Dataframe with virtual population (one row per individual) (See description of dLH object).
#' @examples
#' ## Registration is required to be able to download data from the HMD and HFD
#' ## HMD: https://www.mortality.org
#' ## HFD: https://www.humanfertility.org
#' \dontrun{
#' # Period data
#' dLH <- BuildViP(user,pw_HMD,pw_HFD,
#'                     countrycode="USA",
#'                     refyear=2021,
#'                     ncohort=1000,
#'                     ngen=4)
#' # Cohort data
#' dLHc <- BuildViP(user,pw_HMD,pw_HFD,
#'                     countrycode="USA",
#'                     cohort=1964,
#'                     ncohort=1000,
#'                     ngen=4)
#' }
#' @export BuildViP

BuildViP <- function (user=NULL,pw_HMD=NULL,pw_HFD=NULL,
                      countrycode,cohort=NULL,refyear=NULL,
                      ncohort,ngen,mort=TRUE)
{ if (is.null(user)) stop("BuildViP: user name missing")
  if (is.null(pw_HMD)) stop("BuildViP: pw_HMD missing")
  if (is.null(pw_HFD)) stop("BuildViP: pw_HFD missing")
  
  countriesHMD <- HMDHFDplus::getHMDcountries()
  countriesHFD <- HMDHFDplus::getHFDcountries()
  if (!countrycode%in%countriesHMD$CNTRY) 
       stop("BuildViP: Country missing in HMD  or wrong country code")
  if (!countrycode%in%countriesHFD$CNTRY) 
       stop("BuildViP: Country missing in HFD  or wrong country code")
  
  if (is.null(cohort) & !is.null(refyear))
   { data_raw <- VirtualPop::GetData (country=countrycode,user,pw_HMD,pw_HFD)
    yearsHMD <- unique(data_raw$LTf$Year)
    yearsHFD <- unique(data_raw$fert_rates$Year)
    refyear0 <- min(max(yearsHMD),max(yearsHFD))
    if (exists("refyear") & !is.null(refyear) & refyear>refyear0) refyear <- refyear0
    fert_rates <- HMDHFDplus::readHFDweb(CNTRY=countrycode,item="mi",
                    username=user,password=pw_HFD,fixup=TRUE)
    data_raw$fert_rates <- fert_rates
    rates <- VirtualPop::GetRates(data=data_raw,refyear=refyear)
    } else  
   { if (is.null(refyear) & !is.null(cohort))
     {  rates <- VirtualPop::GetRatesC(country=countrycode,
                     user,pw_HMD,pw_HFD,refcohort=cohort)
     } else stop("Check arguments cohort and refyear of BuildViP()")
   }
  message ("Reading HMD and HFD data completed. Rates computed")
  
  # Generate an ngen-generation virtual population 
  #      using period rates of USA in reference year
  dLH <- VirtualPop::GetGenerations (rates,ncohort=ncohort,ngen=ngen,mort) 
  
  # Complete nch for males 
  z <- dLH$nch[match(dLH$IDpartner,dLH$ID)]
  dLH$nch[dLH$sex=="Male"] <- z[dLH$sex=="Male"]
  
  return(dLH)
}