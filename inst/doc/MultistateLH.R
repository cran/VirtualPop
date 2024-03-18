## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
M <- matrix(c(   0.0,0.10,0.05,        
                 0.07,0.0,0.03,
                 0.02,0.05,0.0),nrow=3,byrow=TRUE)
namstates <- c("A","B","C")
dimnames(M) <- list(origin=namstates,destination=namstates)
diag(M) <- -rowSums(M)
MM <- -M
out <- knitr::kable(MM,
  caption = "Transition rate matrix",
  format = 'latex', booktabs = T,linesep = "")
y <- kableExtra::kable_styling(out,latex_options="HOLD_position")
kableExtra::add_header_above(y,c("From"=1,
                          "To"=3))

## ----comment=""---------------------------------------------------------------
set.seed(33)
bio <- msm::sim.msm (qmatrix=-MM,mintime=20,maxtime=40,start=1)
bio

## -----------------------------------------------------------------------------
rates <- NULL
data(rates,package="VirtualPop")

## ----comment=""---------------------------------------------------------------
rates$ratesM[26:29,,]

## ----comment=""---------------------------------------------------------------
popsim <- data.frame (ID=3,
           born=1990.445,
           start=0,
           end=55,
           st_start="par0")
set.seed(31)
ch <- suppressWarnings(VirtualPop::Sim_bio (datsim=popsim,
                                            ratesM=rates$ratesM))
ch

## -----------------------------------------------------------------------------
z <- format(lubridate::date_decimal(1990.445+ch$ages_trans[1]),  
                      "%a %b %d %Y" )

## -----------------------------------------------------------------------------
rates <- NULL
data(rates,package="VirtualPop")

## -----------------------------------------------------------------------------
cohort <- 2021
ncohort <- 1000
ID <- 1:ncohort
sex <- rbinom(ncohort,1,prob=1/2.05)
sex <- factor (sex,levels=c(0,1),labels=c("Male","Female"),ordered=TRUE)
# Population size by sex
nmales <- length(sex[sex=="Male"])
nfemales <- length(sex[sex=="Female"])
gen <- rep(1,ncohort) # generation 1
# Decimal date of birth
bdated <- cohort+runif(ncohort)
# Create data frame
d <- data.frame (ID=ID,
                 gen=gen,
                 cohort=cohort,
                 sex=sex,
                 bdated=bdated,
                 ddated=NA,
                 x_D=NA,
                 IDmother=NA,
                 IDfather=NA,
                 jch=NA,
                 IDpartner=NA,
                 udates=NA,
                 nch=NA)
# Ages at death, obtained by sampling a peicewise-exponential distribution, 
# using the rpexp function of the msm package
ages <- as.numeric(rownames(rates$ASDR))
d$x_D[d$sex=="Male"] <- msm::rpexp(n=nmales,rate=rates$ASDR[,"Males"],
                                   t=ages)
d$x_D[d$sex=="Female"] <- msm::rpexp(n=nfemales,rate=rates$ASDR[,"Females"],
                                     t=ages)
# Decimal data of death
d$ddated <- d$bdated+d$x_D

## ----results="hide"-----------------------------------------------------------
d <- VirtualPop::PartnerSearch(dLH=d)

## -----------------------------------------------------------------------------
out <- knitr::kable(head(d),
  caption = "Data for selected individuals",
  format = 'latex', booktabs = T,linesep = "")
kableExtra::kable_styling(out,latex_options=c("scale_down", "HOLD_position"))

## ----warning=FALSE------------------------------------------------------------
dch1 <- VirtualPop::Children(dat0=d,rates=rates)

## -----------------------------------------------------------------------------
dch1$dch$IDfather <- dch1$data$IDpartner[dch1$dch$IDmother]

## -----------------------------------------------------------------------------
out <- knitr::kable(head(dch1$dch),
  caption = "Data for selected children of members of initial cohort",
  format = 'latex', booktabs = T,linesep = "")
kableExtra::kable_styling(out,latex_options=c("scale_down","HOLD_position"))

## ----results="hide"-----------------------------------------------------------
d2 <- VirtualPop::PartnerSearch (dLH=dch1$dch)

## ----warning=FALSE------------------------------------------------------------
dch2 <-  VirtualPop::Children(dat0=d2,rates=rates)

## ----results="hide",warning=FALSE---------------------------------------------
d3 <- VirtualPop::PartnerSearch (dLH=dch2$dch)
dch3 <-  VirtualPop::Children(dat0=d3,rates=rates)
d4 <- VirtualPop::PartnerSearch (dLH=dch3$dch)
dch4 <-  VirtualPop::Children(dat0=d4,rates=rates)
d4 <- dch4$data[,1:which (colnames(dch4$data)=="nch")]

## -----------------------------------------------------------------------------
dLH2 <- rbind(dch1$data,dch2$data,dch3$data,dch4$data)

