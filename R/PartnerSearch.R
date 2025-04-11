#' Simple Partner Search Simulation
#' 
#' In this updated partner search model, a partner is an individual of a different sex selected 
#' at random among members of the same generation.
#' The function is called by GetGenerations().
#' 
#' @param idego IDs of egos in search for partner
#' @param d Database (eg dLH)
#' @return \item{d}{Updated version of database (d), which includes, for each individual 
#' without a partner and able to find a partner, the ID of the partner.}
#' \item{dp}{Data related to partner search (dataframe)}
#' 
#' @examples
#' utils::data(dLH,package="VirtualPop")
#' dp <- VirtualPop::PartnerSearch(idego=dLH$ID,d=dLH)
#' 
#' @export PartnerSearch

PartnerSearch <- function (idego,d)
{  n <- length(idego)
   dp <- data.frame(idego=idego,
                    agedifCond=abs(rnorm(n,mean=2,sd=3)),
                    agedifReal=rep(NA,n),
                    ncand=rep(NA,n),
                    IDpartner=rep(NA,n))
   # Ego has individual preferences
   # The search algorithm identifies a candidate who meets ego's 
   # preferences (conditions)
   #
   # agedifCond: Max age difference between ego and acceptable partner (for all egos)
   
   for (ii in idego) 
   { i <- match(ii,d$ID) 
     j <- match(ii,dp$idego)
   # print(c(ii,i))}
     # If ego (in row i with ID dp$ID[i]) has a partner, skip:
      if (!is.na(dp$IDpartner[i])) next  
     # Candidate is of opposite sex 
      condition1 <- d$sex!=d$sex[i]
     # Ego cannot partner with ego (included in condition1)
     # Ego cannot partner with indiv with same mother
         # if mother is not known, siblings not known
      condition2 <- is.na(d$IDmother) | d$IDmother!=d$IDmother[i]
     # Exclude cousins and nieces (not used) 
     #   (maternal and paternal grandmothers must be known)
      # condition3 <- !is.na(dp$IDmother[dp$IDmother[i]]) &
      #              !is.na(dp$IDmother[dp$IDfather[i]]) &
      #              dp$IDmother[dp$IDmother] != dp$IDmother[dp$IDmother[i]] &
      #              dp$IDmother[dp$IDfather] != dp$IDmother[dp$IDfather[i]]
      conditions <- condition1 & condition2 

      # Age difference between partners should be less than a
      # number drawn from N(mean=2,sd=3)
      # a. Identify candidates (get their IDs: idCand)
      agedif <- abs(d$bdated - d$bdated[i]) < dp$agedifCond[j]
      idCand <- d$ID[conditions & agedif]
        # Number of alter who meet the conditions
      dp$ncand[j] <- length(idCand)
      # b. Select a candidate: ID is j (ego is i)
      if (length(idCand)==0) next else
      {if (length(idCand)==1)  dp$IDpartner[j] <- idCand else       #  idm[!is.na(idm)] else
         if (length(idCand)>1) dp$IDpartner[j] <-  sample(idCand,1,replace=FALSE)}
      # c. Add to d
      d$IDpartner[i] <- dp$IDpartner[j]
      # d. Ego becomes partner of indiv with ID idpartner[i]
      d$IDpartner[match(d$IDpartner[i],d$ID)] <- dp$idego[j]
   }

   # Age difference between partner and ego
   dp$bdated_ego <- d$bdated[match(dp$idego,d$ID)]
   dp$bdated_p <- d$bdated[match(dp$IDpartner,d$ID)]
   dp$agedifReal <- dp$bdated_p-dp$bdated_ego
   mean(dp$agedifReal,na.rm=TRUE)  # 0
   sd(dp$agedifReal,na.rm=TRUE)
   
return(list(dp=dp,
            d=d))
}
# Partner of mother of ego is the father of ego
#   dLH$IDfather <- dLH$IDpartner[dLH$IDmother]
#   dLH$IDfather <- dLH$IDpartner[match(dLH$IDmother,dLH$ID)]   # same