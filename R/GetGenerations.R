#' Builds a Multi-Generation Virtual Population from demographic parameters
#' 
#' Builds a virtual population from mortality rates by age and sex, and fertility
#' rates by age of mother and parity.
#' 
#' 
#' @param rates List object with death rates (ASDR) and birth rates (ASFR). Produced by function VirtualPop::GetRates(). Rates of USA 2021 are distributed with the VirtualPop package.
#' @param ncohort Size of hypothetical birth cohort (first generation)
#' @param ngen Number of generations to be simulated. No upper limit. 
#' @param mort Presence or absence of mortality. This parameter is optional. Default is TRUE. If mortality is absent, mort=FALSE.
#' 
#' @return \item{dataAllgen}{The database of simulated individual lifespans and
#' fertility histories (all generations).}
#' The object dataAllgen has four attributes:
#' \item{country}{The country}
#' \item{type}{The type of data (period data or cohort data).}
#' \item{refyear}{The calendar year for which the period data are used (reference year).}
#' \item{cohort}{The birth cohort (if applicable).}
#' 
#' @examples
#' 
#' 
#' utils::data(rates,package = "VirtualPop")
#' dLH <- VirtualPop::GetGenerations (rates=rates,ncohort=1000,ngen=4)
#' 
#' 
#' @export GetGenerations
#' 
GetGenerations <- function (rates,ncohort=NULL,ngen=NULL,mort=NULL)
{
if (is.null(ncohort) | ncohort==0) stop ("Please specify ncohort")
if (is.null(ngen)) ngen <- 4
  
country <- attr(rates,"country")
type <- attr(rates,"type")
if (is.null (attr(rates,"type"))) 
   { refyear <- attr(rates,"year") 
     type = "period"} else
   { if (type=="period") refyear <- attr(rates,"year") else refyear=attr(rates,"refyear")}
if(type=="cohort") 
  { cohort <- attr(rates,"cohort") } else 
  { cohort <- refyear
  }

# First generation
generation <- 1


# ===========    sex   ===========
sex <- rbinom(ncohort,1,prob=1/2.05)
sex <- factor (sex,levels=c(0,1),labels=c("Male","Female"),ordered=TRUE)
nmales <- unname(table (sex)[1])
nfemales <- unname(table (sex)[2])

# ==========   Create dataframe  ================
data <- data.frame(ID=1:ncohort,
                   gen=rep(1,ncohort),
                   cohort=rep(cohort,ncohort),
                   sex=sex,
                   bdated=NA,
                   ddated=NA,
                   x_D=NA,
                   IDmother=NA,
                   IDfather=NA,
                   jch=NA,
                   IDpartner=NA,
                   udated = NA,
                   nch=rep(0,ncohort)) 
# =====  Allocate date of birth in reference year (F) and refyear-2 (M) ====
data$bdated[data$sex=="Male"] <- cohort+runif(nmales) 
data$bdated[data$sex=="Female"] <- cohort+runif(nfemales)

# ========= simulate age at death using age-specific death rates  =========
# Age at death
data <- VirtualPop::Lifespan (data,ASDR=rates$ASDR,mort)

dataMF <- data

# =====  First generation: simulate fertility careers (children)  ========
# Create object with life histories of children of initial pop
dataAllgen <- NULL
for (igen in 1:ngen)
 { dataMF2 <- VirtualPop::PartnerSearch (dLH=dataMF)
   datach <- VirtualPop::Children (dat0=dataMF2,rates,mort=mort)
   dataMF <- datach$dch
   dataAllgen <- rbind (dataAllgen,datach$data)
}   
# Add info on children of women of generation ngen (=> is generation ngen+1)
dataAllgen <- rbind (dataAllgen,datach$dch)

attr(dataAllgen,"country") <- country
attr(dataAllgen,"type") <- attr(rates,"type")
attr(dataAllgen,"refyear") <- refyear
if (type=="cohort") 
      attr(dataAllgen, "cohort")= attr(rates,"cohort") else
        attr(dataAllgen, "cohort")= NA

return(dataAllgen)
}
