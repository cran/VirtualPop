#' Generates Individual Fertility Histories
#' 
#' Builds individual fertility histories from conditional fertility rates. Children() uses the function Sim_bio().
#' 
#' 
#' @param dat0 Data frame with data on individual members of the virtual population (dLH format)
#' @param rates Mortality and fertility rates. The object 'rates' is produced by the function Getrates(). 
#' @param mort Presence or absence of mortality (optional). Default: mortality is present (mort=TRUE). If mortality is absent, set mort=FALSE.
#' @return List object with two components: 
#' \item{data}{Data frame with updated information on members of the virtual population}
#' \item{dch}{Data frame with information on children}
#'
#' @export Children
#' @examples
#' # The example generates data on children of the first 10 female members of 
#' # the first generation of the virtual population.
#' utils::data(dLH,package="VirtualPop")
#' utils::data(rates,package="VirtualPop")
#' dat0 <- dLH[dLH$sex=="Female" & dLH$gen==1,][1:10,]
#' out <- VirtualPop::Children(dat0=dat0,rates=rates)
#'
Children <- function (dat0,rates,mort=NULL)
{ 
# Generates individual fertility careers
# AND add info to dat0
# AND produces dch, a new data structure with data on children: 
# for each child: date of birth, sex, date of death, age at death, 
# and ID of mother (and father)

refyear <- attr(rates,"year")
country <- attr(rates,"country")

nfemales <- length(dat0$sex[dat0$sex=="Female"])
nmales <- length(dat0$sex[dat0$sex=="Male"])
namsex <- c("Male","Female")
previousgen <- dat0$gen[nrow(dat0)]

a <- Sys.time()
dch <- NULL
idseq <- 0
for (i in 1:nrow(dat0))
 { if (dat0$sex[i]=="Male") next
   if (is.na(dat0$IDpartner[i])) next
    # Hierarchical: fertility career of a single female (uses msm::rpexp)
   # Fertility career starts at 0 and ends at death or death of of partner
   jj <- dat0$IDpartner[dat0$ID[i]]
   if (is.na(jj)) end <- dat0$x_D[i] else end <- min(dat0$x_D[i],dat0$x_D[which(dat0$IDpartner%in%jj)])
   # Create input data for simulation of single fertility career
   popsim <- data.frame(ID=dat0$ID[i],born=dat0$bdated[i],start=0,end=end,st_start="par0")
   # Simulate single fertility career
   ch <- Sim_bio (datsim=popsim,ratesM=rates$ratesM) # see main_datar.R
   # Store results in dataframe
   nch <- ch$nstates-1
   dat0$nch[i] <- nch
   if (!is.null(nch) & nch>0)
   { d <- dat0[1:nch,]
     idseq <- c(idseq, max(idseq) + 1:nch)
     d$gen <- previousgen+1
     d$bdated <- dat0$bdated[i]+ch$ages_trans
     # Birth order
     d$jch <- 1:nch
     # ID of mother
     d$IDmother <- rep(dat0$ID[i],nch)
     d$sex <- rep(NA,nch)
     d$IDpartner <-  rep(NA,nch)
     d$x_D <- rep(NA,nch)
     d$ddated <- rep(NA,nch)
     d$nch <- rep(NA,nch)
     dch <- rbind(dch,d)
   }
}
if (is.null(dch)) dch <- NA else
{ dch$ID <- max(dat0$ID) + idseq[-1]
 dch$sex <- factor (rbinom(n=nrow(dch),size=1,prob=0.5)+1,levels=c(1,2),labels=c("Male","Female"))

  # Age at death
  dch <- Lifespan (data=dch,ASDR=rates$ASDR,mort)
}
aa <- list (data=dat0,
            dch = dch)
 return (aa)
}

