#' Retrieves Period Mortality and Fertility Rates from HMD and HFD for a Selected Country and Selected Year
#' 
#' The rates are retrieved from the life tables and fertility tables included in the raw data downloaded from the HMD and HFD. 
#' 
#' @param data data (the object data_raw, produced by the GetData() function.)
#' @param refyear Reference year, which is the year of period data
#' @return A list object with three elements:
#' \item{ASDR}{Age-specific death rates, by sex for reference year} 
#' \item{ASFR}{Age-specific birth rates by birth order for reference year}
#' \item{ratesM}{Matrix of transition rates in format required for mulitstate modelling}
#' The object returned by the function has three attributes:
#' \item{country}{Country}
#' \item{type}{Type of data (period data or cohort data)}
#' \item{year}{Calendar year for which period death rates are used to complete cohort experience in case of incomplete mortality experience (reference year).}
#' 
#' @examples
#' \dontrun{
#' # Not run because passwords needed
#' # Input data: data_raw produced by GetData().
#' rates <- GetRates(data=data_raw,refyear=2021)
#' }
#' 
#' @export GetRates
GetRates <- function(data,refyear)
{
 yearmort <- unique(data$LTm$Year)
 yearfert <- unique(data$fert_rates$Year)

 # Check existence of refyear
 jj <- unique(data$fert_rates$Year)
 if (!refyear%in%jj) 
   stop("refyear (",refyear,") outside of range from ",jj[1]," to ",max(jj))
 
country <- data$country
df <- data$LTf
dm <- data$LTm
db <- data$LTcom
fert_rates <- data$fert_rates

# ============  Mortality rates for all years  =============
# Remove +
j <- grep("+",df$Age,fixed=TRUE)
if (length(j)>0) 
  { z<- unlist(strsplit(df$Age[j],"+",fixed=TRUE))
    df$Age[j] <- z
    dm$Age[j] <- z }

yearmort <- unique(df$Year)
ages <- df$Age[dm$Year==refyear]
drates <- array(dim=c(length(ages),2,length(yearmort)),
    dimnames=list (Ages=ages,Sex=c("Males","Females"),Year=yearmort))
for (iy in 1:length(yearmort))
  {drates[,1,iy] <- dm$mx[dm$Year==yearmort[iy]]
   drates[,2,iy] <- df$mx[df$Year==yearmort[iy]]
  }


# ============  conditional age-specific fertility rates ===============
j2 <- grep("+",fert_rates$Age,fixed=TRUE)
if (length(j2)>0)
  { z <- unlist(strsplit(fert_rates$Age[j2],"+",fixed=TRUE))
   fert_rates$Age[j2] <- z
  }
#  Remove-
j3 <- grep("-",fert_rates$Age,fixed=TRUE)
if (length(j3)>0)
  { z <- unlist(strsplit(fert_rates$Age[j3],"-",fixed=TRUE))
    fert_rates$Age[j3] <- z
  }

yearfert <- unique (fert_rates$Year)
ages12 <- as.numeric(fert_rates$Age[fert_rates$Year==refyear])
ages <- c(0:(min(ages12-1)),ages12)
dfrates <- array(dim=c(length(ages),5,length(yearfert)),
    dimnames=list (Ages=ages,Parity=paste("parity",1:5,sep=""),Year=yearfert))

for (iy in 1:length(yearfert))
  { jj <- which (colnames(fert_rates)%in%c("Year","Age","OpenInterval"))
    ASFR0 <- subset(fert_rates[,-jj], fert_rates$Year==yearfert[iy])
    rownames(ASFR0)<- fert_rates$Age[fert_rates$Year==yearfert[iy]]
    minage_fert <- min(rownames(ASFR0))
    maxage_fert <- max(rownames(ASFR0))
    ASFR0[is.na(ASFR0) | ASFR0=="." ] <- "0"
    # Fertility rates is 0 for ages 0 to 11
    ASFR00 <- cbind (m1x=rep(0,12),m2x=rep(0,12),m3x=rep(0,12),m4x=rep(0,12),m5px=rep(0,12))
    rownames(ASFR00) <- 0:11
    # Non-zero fertility rates
    # zz <- transform(ASFR0,m1x = as.numeric(m1x),m2x=as.numeric(m2x),m3x=as.numeric(m3x),
    #                m4x=as.numeric(m4x),m5px=as.numeric(m5px))
    ASFR <-rbind (ASFR00,ASFR0)
    numASFR <- t(apply (ASFR,1,function(x) as.numeric(x)))
    dfrates[,,iy] <- numASFR
  }

ratesCountryAll <- list (ASDR=drates,ASFR=dfrates)
attr(ratesCountryAll,"country") <- country

# ===========  Get rates for reference year  ==========
irefyearmort <- which (dimnames(ratesCountryAll$ASDR)$Year==refyear)
irefyearfert <- which (dimnames(ratesCountryAll$ASFR)$Year==refyear)
rates <- list ()
rates$ASDR <- ratesCountryAll$ASDR[,,irefyearmort]
rates$ASFR <- ratesCountryAll$ASFR[,,irefyearfert]

GetRatesM <- function(rates)
{# ============ Transition matrix for multistate modelling  ==========
nn <- 7
namstates <- c(paste("par",0:(nn-1),sep=""))
ratesM <- array(data=0,dim=c(56,nn,nn),dimnames=list(Age=0:55,Destination=namstates,Origin=namstates))
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
attr(rates,"type")    <- "period"
attr(rates,"year")    <- refyear


return (rates)
}
