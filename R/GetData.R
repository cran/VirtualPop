#' Reads Data from the HMD and HFD into R
#' 
#' Reads data from the HMD and HFD into R. The function uses the readHMDweb() and the readHFDweb() functions of the HMDHFDplus package. 
#' 
#' 
#' @param country Code of the selected country. The code must be one of the country codes of HMD and HFD.
#' @param user email address of the user, used at registration with the HMD and HFD. It
#' is assumed that the same email address is used for both HMD and HFD.
#' @param pw_HMD Password to access HMD, provided at registration. 
#' @param pw_HFD Password to access HFD, provided at registration
#' @return \item{data_raw}{A list object with four elements:}
#' \item{country}{Country}
#' \item{LTf}{Life table for female population for all years available in the HMD}
#' \item{LTm}{Life table for male population for all years available in the HMD}
#' \item{fert_rates}{Conditional fertility rates for all years available in the HFD}
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' data_raw <- GetData(country="USA",user,pw_HMD,pw_HFD)
#' }
#' 
#' @export GetData
#' 
GetData <- function(country,user,pw_HMD,pw_HFD)
{
# ==============  Part A Extract data from HMD and HFD  ==================

# help function to list the available countries
countries <- HMDHFDplus::getHMDcountries()

# ====================  Get data from HMD and HFD: data_raw  =================
#country <- "USA"
refyear_data <- "all"
dataLH <- NULL
rates <- NULL
# =============  Read life tables and get death rates  ============
message (paste ("Extract data from HMD and HFD for ",country,sep=""))
df <- HMDHFDplus::readHMDweb(CNTRY=country,item="fltper_1x1",username=user,password=pw_HMD,fixup=TRUE)
dm <- HMDHFDplus::readHMDweb(CNTRY=country,item="mltper_1x1",username=user,password=pw_HMD,fixup=TRUE)

# ============  Read conditional age-specific fertility rates ===============
fert_rates <- HMDHFDplus::readHFDweb(CNTRY=country,item="mi",username=user,password=pw_HFD,fixup=TRUE)

# ============  Create raw data file  =================
data_raw <- list (country=country,LTf=df,LTm=dm,fert_rates=fert_rates)


return  (data_raw)
}
