#' Simple Partner Search Simulation
#' 
#' In this simple model, a partner is an individual of a different sex selected at random among members of the same generation.
#' The function is called by GetGenerations().
#' 
#' @param dLH Database
#' @return \item{dLH}{Updated version of database (dLH), which includes, for each individual 
#' without a partner and able to find a partner, the ID of the partner.}
#' 
#' @examples
#' 
#' utils::data(dLH,package="VirtualPop")
#' d <- VirtualPop::PartnerSearch(dLH=dLH)
#' 
#' @export PartnerSearch
PartnerSearch <- function (dLH)
{
# Number of generations in dLH
igen <- unique(dLH$gen)
ng <- length(igen)
# =============  For each generation  ============

for (i in 1:ng)
{
# IDs of members of generation i
id0 <- dLH$ID[dLH$gen==igen[i]]
dLH2 <- subset(dLH,dLH$gen==igen[i])
# Are all indiv of generation i without a partner?
tf <- all(is.na(dLH2$IDpartner))
# If FALSE (ie most have partner): skip
if (!tf) next
# Number of males and females
nf <- length(dLH2$ID[dLH2$sex=="Female"])
nm <- length(dLH2$ID[dLH2$sex=="Male"]) 
# Number of males  may exceed number of females
nsample <- min(nf,nm)
if (nsample < nf) # Number of couples = number of males
{id <- sample(dLH2$ID[dLH2$sex=="Female" & is.na(dLH2$IDpartner)],nsample,replace=FALSE)
 dLH2$IDpartner[dLH2$sex=="Male" & is.na(dLH2$IDpartner)] <- id 
 xx <- subset (dLH2$ID,dLH2$sex=="Male" & !is.na(dLH2$IDpartner))
 partners <- cbind (female=id,male=xx)
} else # Number of couples = number of females (<- excess males)
  {id <- sample(dLH2$ID[dLH2$sex=="Male" & is.na(dLH2$IDpartner)],nsample,replace=FALSE)
   dLH2$IDpartner[dLH2$sex=="Female" & is.na(dLH2$IDpartner)] <- id 
   # Create matrix with IDs of males and IDs of their partners
   xx <- subset (dLH2$ID,dLH2$sex=="Female" & !is.na(dLH2$IDpartner))
   partners <- cbind (male=id,female=xx)
  }
# Allocate partners to indiv in generation igen[i] (reciprocity)
dLH2$IDpartner[match(partners[,1],dLH2$ID)] <-partners[,2]
# Store the partner IDs in dLH 
dLH$IDpartner[dLH$gen==igen[i]] <- dLH2$IDpartner
}
return(dLH)
}
