#' Retrieves Cohort Data from the HMD and HFD and Obtains Cohort Rates
#' 
#' Retrieves cohort data from the HMD and HFD and produces cohort rates 
#' (death rates by age and sex and conditional fertility rates by age and parity). 
#' The function combines the steps of (a) data retrieval and (b) extraction of mortality and fertility rates. 
#' 
#' 
#' @param country Code of the country selected. The code must be one of the country codes of HMD and HFD.
#' @param user Name of the user, used at registration with the HMD and HFD. It
#' is assumed that the same name is used for both HMD and HFD.
#' @param pw_HMD Password to access HMD, provided at registration.
#' @param pw_HFD Password to access HFD, provided at registration
#' @param refcohort Year of birth of cohort for which the data are used for the simulation.
#' @return A list object with three elements: 
#' \item{ASDR}{Age-specific death rates by sex for selected birth cohort} 
#' \item{ASFR}{Age-specific fertility rates by parity for selected birth cohort}
#' \item{ratesM}{Matrix of transition rates in format required for mulitstate modelling}
#' The object returned by the function has five attributes:
#' \item{country}{Country}
#' \item{type}{Type of data (period data or cohort data)}
#' \item{cohort}{Birth cohort (year of birth}
#' \item{refyear}{Calendar year for which period death rates are used to complete cohort experience in case of incomplete mortality experience (reference year).}
#' \item{start_pASDR}{Lowest age for which cohort data are missing. The mortality rates of that age and higher ages are borrowed from period data collected in the reference year.}
#' 
#' @export GetRatesC
#' 
#' @examples
#' \dontrun{
#' ratesC  <- GetRatesC(country="USA",user,pw_HMD,pw_HFD,refcohort)
#' }
#' 
#' 
GetRatesC <- function(country,user,pw_HMD,pw_HFD,refcohort)
{
# refyear: determined in this function: most recent year with period data
# ====================  Get data from HMD and HFD: data_raw  =================
rates <- NULL
# +++++++++++==++++++++++   Death rates  +++++++++++++++++++
# =============  Read cohort death rates for all years ============
message (paste ("Extract cohort rates from HMD and HFD for ",country,sep=""))
dr <- HMDHFDplus::readHMDweb(CNTRY=country,item="cMx_1x1",username=user,
                             password=pw_HMD,fixup=TRUE)
# Check existence of refcohort in HMD
jj <- unique(dr$Year)
if (!refcohort%in%jj) 
              { stop ("HMD: refcohort (",refcohort,") outside of range from "
            ,jj[1]," to ",max(jj))
              }

# ============  Death rates of selected birth cohort  =========
drm <- subset(dr,dr$Year==refcohort)[,c("Age","Male")]
#  Year = cohort
drf <- subset(dr,dr$Year==refcohort)[,c("Age","Female")]

# =======  Read PERIOD life tables for all years and get death rates  =======
df <- HMDHFDplus::readHMDweb(CNTRY=country,item="fltper_1x1",
                             username=user,password=pw_HMD,fixup=TRUE)
dm <- HMDHFDplus::readHMDweb(CNTRY=country,item="mltper_1x1",
                             username=user,password=pw_HMD,fixup=TRUE)
# =======  Select PERIOD death rates of most recent year available   =======
yearmort <- unique(df$Year)
refyear <- max(yearmort)
ages <- df$Age[dm$Year==refyear]
drates <- array(dim=c(length(ages),2),
                dimnames=list (Ages=ages,Sex=c("Males","Females")))
# for (iy in 1:length(yearmort))
{drates[,1] <- dm$mx[dm$Year==refyear]
drates[,2] <- df$mx[df$Year==refyear]
}
# =========  Complete cohort death rates with period rates  ======
ixL <-  which(is.na(drm$Male))[1] # First missing cohort rate
ixH <- nrow(drm)
drm$Male[ixL:ixH] <- drates[ixL:ixH,"Males"]       #  ,irefyearmort]
drf$Female[ixL:ixH] <- drates[ixL:ixH,"Females"]   # ,irefyearmort]

# +++++++++++==++++++++++   Fertility rates  +++++++++++++++++++
# ==== Read cohort fertility tables for all years ====
fertTables <- HMDHFDplus::readHFDweb(CNTRY=country,item="cft",username=user,
                                     password=pw_HFD,fixup=TRUE)
icohorts <- unique(fertTables$Cohort)
# ======  Select fertility rates for selected birth cohort  =======
if (!refcohort%in%icohorts) stop("No fertility data (cft) on selected cohort")
cfr <- subset(fertTables,fertTables$Cohort==refcohort)
# Ages (see GetRates.r)
j2 <- grep("+",cfr$x,fixed=TRUE)
if (length(j2)>0)
{ z <- unlist(strsplit(cfr$x[j2],"+",fixed=TRUE))
cfr$x[j2] <- z
}
#  Remove-
j3 <- grep("-",cfr$x,fixed=TRUE)
if (length(j3)>0)
{ z <- unlist(strsplit(cfr$x[j3],"-",fixed=TRUE))
cfr$x[j3] <- z
}
ages12 <- as.numeric(cfr$x)
ages <- c(0:(min(ages12-1)),ages12)
dcfr <- array(0,dim=c(length(ages),5),
              dimnames=list (Ages=ages,Parity=paste("parity",1:5,sep="")))
dcfr[13:nrow(dcfr),] <- data.matrix(cfr[,c("m1x","m2x","m3x","m4x","m5px")])
dcfr[is.na(dcfr)] <- 0

# =============  Create list object "rates": ASDR, ASFR, ratesM  ============
rates <- list ()
rates$ASDR <- data.frame(Males=drm$Male,Females=drf$Female)
rownames(rates$ASDR) <- drm$Age
rates$ASFR <- dcfr

# ============ Transition matrix for multistate modelling  ==========
GetRatesM <- function(rates)
{ nn <- 7
  namstates <- c(paste("par",0:(nn-1),sep=""))
  ratesM <- array(data=0,dim=c(56,nn,nn),
          dimnames=list(Age=0:55,Destination=namstates,Origin=namstates))
  ratesM[,2,1] <- -rates$ASFR[,1]
  ratesM[,3,2] <- -rates$ASFR[,2]
  ratesM[,4,3] <- -rates$ASFR[,3]
  ratesM[,5,4] <- -rates$ASFR[,4]
  ratesM[,6,5] <- -rates$ASFR[,5]
  ratesM[,7,6] <- -rates$ASFR[,5]
  
  ratesM[,1,1] <- rates$ASFR[,1]
  ratesM[,2,2] <- rates$ASFR[,2]
  ratesM[,3,3] <- rates$ASFR[,3]
  ratesM[,4,4] <- rates$ASFR[,4]
  ratesM[,5,5] <- rates$ASFR[,5]
  ratesM[,6,6] <- rates$ASFR[,5]
  rates$ratesM <- ratesM
}

# ============ Transition matrix for multistate modelling  ==========
# Period mortality and fertility rates
ratesM <- GetRatesM(rates)
rates$ratesM <- ratesM

attr(rates,"country") <- country
attr(rates,"type") <- "cohort"
attr(rates,"cohort") <- refcohort
attr(rates,"refyear") <- refyear
attr(rates,"start_pASDR") <- ixL

return  (rates)
}
