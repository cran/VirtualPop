## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center")
knitr::opts_chunk$set(fig.pos = "H", out.extra = "") 
## No scientific notation
options(scipen=999)

## ----echo=FALSE---------------------------------------------------------------
knit_print.data.frame = function(x, ...) {
  res = paste(c("", "", knitr::kable(x)), collapse = "\n")
  knitr::asis_output(res)
}
registerS3method(
  "knit_print", "data.frame", knit_print.data.frame,
  envir = asNamespace("knitr")
)

## -----------------------------------------------------------------------------
countriesHMD <- HMDHFDplus::getHMDcountries()
countriesHFD <- HMDHFDplus::getHFDcountries()
# print (countries[,c(1,3)],n=nrow(countries))

## ----eval=FALSE---------------------------------------------------------------
#  install.packages ("VirtualPop")

## -----------------------------------------------------------------------------
library (VirtualPop)

## ----eval=FALSE---------------------------------------------------------------
#  utils::browseVignettes("VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  utils::vignette (topic="Tutorial",package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  data(package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  library (HMDHFDplus)
#  version <- packageVersion("HMDHFDplus") # 2.0.3

## ----eval=FALSE---------------------------------------------------------------
#  tools::package_dependencies(recursive = TRUE)$VirtualPop

## ----echo=FALSE---------------------------------------------------------------
utils::data(dLH,package="VirtualPop")
ncohort <- length(dLH$ID[dLH$gen==1])

## ----BuildViP,eval=FALSE------------------------------------------------------
#  # Period data
#  dLH <- tryCatch(VirtualPop::BuildViP(user,pw_HMD,pw_HFD,
#                               countrycode="USA",
#                               refyear=2021,
#                               ncohort=1000,
#                               ngen=2),
#                   error=function(cond) {message("Error reading HFD data. Download data and read data locally (see Tutorial Section 4.2).")})
#  
#  
#  # Specify pathSave, the folder to save dLH.
#  pathSave <- "Name of folder to save dLH"
#  fileSave <-paste0("dLH_",attr(dLH,"country"),attr(dLH,"refyear"),"_",
#                    max(dLH$gen)-1,"_",length(dLH$ID[dLH$gen==1]),".rda")
#  save(dLH,file=paste0(pathSave,fileSave))
#  
#  # Period data; no mortality
#  dLHnm <- tryCatch(VirtualPop::BuildViP(user,pw_HMD,pw_HFD,
#                               countrycode="USA",
#                               refyear=2021,
#                               ncohort=1000,
#                               ngen=2,
#                               mort=FALSE),
#                                     error=function(cond) {message("Error reading HFD data. Download data and read data locally (see Tutorial Section 4.2).")})
#  
#  # Cohort data
#  dLHc <- tryCatch(VirtualPop::BuildViP (user,pw_HMD,pw_HFD,
#                            countrycode="USA",
#                            cohort=1964,
#                            ncohort=1000,
#                            ngen=2,
#                            mort=TRUE),
#                                    error=function(cond) {message("Error reading HFD data. Download data and read data locally (see Tutorial Section 4.2).")})
#  
#  fileSave <-paste0("dLH_",attr(dLH,"country"),attr(dLH,"refyear"),"_",
#                    max(dLH$gen)-1,"_",length(dLH$ID[dLH$gen==1]))
#  save(dLH,file=paste0(pathSave,fileSave))

## -----------------------------------------------------------------------------
path <- "/users/frans/VirtualPop_data/"

## ----eval=FALSE---------------------------------------------------------------
#  dm <- utils::read.table(paste0(path,"mltper_1x1.txt"),skip=2,header=TRUE)
#  df <- utils::read.table(paste0(path,"fltper_1x1.txt"),skip=2,header=TRUE)
#  fert_rates <- utils::read.table(paste0(path,"USAmi.txt"),skip=2,header=TRUE)

## ----echo=FALSE---------------------------------------------------------------
countrycode <- "USA"

## ----eval=FALSE---------------------------------------------------------------
#  countrycode <- "USA"
#  data_raw <- list (country=countrycode,LTf=df,LTm=dm,fert_rates=fert_rates)
#  attr(data_raw,"country") <- countrycode
#  data_raw <- data_raw

## ----eval=FALSE---------------------------------------------------------------
#  path1 <- "Define the path to the folder to save the data locally"
#  save (data_raw,file=paste(path1,"data_raw",countrycode,".RData",sep=""))

## ----eval=FALSE---------------------------------------------------------------
#  path <- "Define the path to the data on your computer"
#  cmortrates <- utils::read.table(paste0(path,"cMx_1x1.txt"),skip=2,header=TRUE)
#  cfertTables <- utils::read.table(paste0(path,"cft.txt"),skip=2,header=TRUE)

## -----------------------------------------------------------------------------
user <- "your email address"
pw_HMD <- "password for HMD" ## (password for new (2022) HMD website)
pw_HFD <- "password for HFD"

## ----eval=FALSE---------------------------------------------------------------
#  data_raw <- VirtualPop::GetData (country="USA",user,pw_HMD,pw_HFD)

## ----eval=FALSE---------------------------------------------------------------
#  df <- HMDHFDplus::readHMDweb(CNTRY=countrycode,item="fltper_1x1",username=user,
#           password=pw_HMD,fixup=TRUE)
#  dm <- HMDHFDplus::readHMDweb(CNTRY=countrycode,item="mltper_1x1",username=user,
#          password=pw_HMD,fixup=TRUE)
#  fert_rates <- HMDHFDplus::readHFDweb(CNTRY=countrycode,item="mi",username=user,
#          password=pw_HFD,fixup=TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  countrycode <- "USA"
#  cmrates <- HMDHFDplus::readHMDweb(CNTRY=countrycode,item="cMx_1x1",
#          username=user, password=pw_HMD,fixup=TRUE)
#  cfertTables <- HMDHFDplus::readHFDweb(CNTRY=countrycode,item="cft",
#          username=user, password=pw_HFD,fixup=TRUE)
#  # List cohorts for which data are downloaded
#  cohortsHMD <- unique(cmrates$Year)
#  cohortsHFD <- unique(cfertTables$Cohort)

## -----------------------------------------------------------------------------
refyear <- 2021

## ----eval=FALSE---------------------------------------------------------------
#  rates <- VirtualPop::GetRates(data=data_raw,refyear=refyear)

## ----eval=FALSE---------------------------------------------------------------
#  save (rates,file=paste(path,"rates",countrycode,"_",refyear,".RData",sep=""))

## -----------------------------------------------------------------------------
utils::data(rates,package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  utils::vignette (topic="Multistate",package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  ratesC  <- VirtualPop::GetRatesC(country="USA",user,pw_HMD,pw_HFD,refcohort=1964)
#  save (rates,file=paste(path,"ratesC",countrycode,"_",refyear,".RData",sep=""))

## -----------------------------------------------------------------------------
utils::data(ratesC,package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  utils::vignette (topic="Piecewise_exponential",package="VirtualPop")

## -----------------------------------------------------------------------------
ncohort <- 1000
refyear <- attr(rates,"year")
sex <- rbinom(ncohort,1,prob=1/2.05)
sex <- factor (sex,levels=c(0,1),labels=c("Male","Female"),ordered=TRUE)
## Decimal date of birth
bdated <- refyear+runif(ncohort)
dLS <- data.frame(ID=1:ncohort,sex=sex,bdated=bdated)
dLS <- VirtualPop::Lifespan (data=dLS,ASDR=rates$ASDR)

## -----------------------------------------------------------------------------
dLSnm <- VirtualPop::Lifespan (data=dLS,ASDR=rates$ASDR,mort=FALSE)

## ----warning=FALSE------------------------------------------------------------
a <- stats::aggregate(dLS$x_D,by=list(dLS$sex),mean)
a2 <- stats::aggregate(dLS$x_D,by=list(dLS$sex),median)
## Lifespan variation: standard deviation of age at death
b <- stats::aggregate(dLS$x_D,by=list(dLS$sex),sd)
z <- t(data.frame(mean=round(a$x,2),median=round(a2$x,2),sd=round(b$x,2)))
colnames(z) <- a[,1]
out <- knitr::kable(z,format = "simple",
             caption = "Mean and variability of ages at death, by sex")
kableExtra::kable_styling(out,latex_options="HOLD_position")

## ----fig.align="center", fig.cap=paste0("Simulated ages at death, ",countrycode,", ",refyear), out.width=400,out.extra='angle=0'----
dLS$x_D[dLS$x_D>110] <- 110
binwidth <- (max(dLS$x_D,na.rm=TRUE)-min(dLS$x_D,na.rm=TRUE))/60
require (ggplot2)
p <- ggplot() +
  geom_histogram(data=dLS,aes(x=x_D,color=sex,fill=sex,y=after_stat(density)), 
                 alpha=0.5,position="dodge",binwidth=binwidth) +
  geom_density(data =dLS,aes(x_D, colour = sex), alpha = .2) 
p <- p + scale_color_manual(values=c("red", "blue"))
p <- p + scale_fill_hue(c=45, l=80)
xmin <- 0
xmax <- 110
p <- p + xlab("Age")+ylab("Density")
p <- p +  scale_x_continuous(breaks=seq(xmin,xmax,by=10)) 
p <- p + scale_y_continuous (breaks=seq(0,0.04,by=0.005)) 
p <- p + theme(legend.position = c(0.1, 0.85))
p

## ----comment=""---------------------------------------------------------------
set.seed(32)
popsim <- data.frame(ID=1,born=1999.351,start=0,end=85,st_start="par0")
ch <- VirtualPop::Sim_bio (datsim=popsim,ratesM=rates$ratesM) 
paste0("Simulation of single fertility career, ",countrycode,", ",refyear)
ch

## ----comment=""---------------------------------------------------------------
ncohort <- 1 
sex <- rbinom(ncohort,1,prob=1/2.05)
sex <- factor (sex,levels=c(0,1),labels=c("Male","Female"),ordered=TRUE)
dLS <- data.frame(ID=1,sex=sex,bdated=1999.351)
dLS <- VirtualPop::Lifespan(data=dLS,ASDR=rates$ASDR)
popsim <- data.frame(ID=1,born=1999.351,start=0,end=dLS$x_D,st_start="par0")
set.seed(32)
ch <- VirtualPop::Sim_bio (datsim=popsim,ratesM=rates$ratesM) 
paste0("Simulation of single fertility career, ",countrycode,", ",refyear)
ch

## -----------------------------------------------------------------------------
n <- 3
bdated <- c(1999.350,2010.650,2015.340)
popsim <-data.frame(ID=1:3,born=bdated,start=rep(0,n),end=rep(85,n),
  st_start=c("par0","par0","par2"))
ch <- list()
for (i in 1:n)
  { ch[[i]] <- VirtualPop::Sim_bio (datsim=popsim[i,],ratesM=rates$ratesM)
}

## -----------------------------------------------------------------------------
# Create a dataframe with the data for the simulation
ncohort <- 1000
bdated <- 2000+runif(ncohort)
data <- data.frame(ID=1:ncohort,sex=sex,bdated=bdated)
data <- VirtualPop::Lifespan (data=data,ASDR=rates$ASDR)
# Generate fertility histories in the presence of mortality
popsim <-data.frame(ID=1:ncohort,born=bdated,start=rep(0,ncohort),end=data$x_D,st_start=rep("par0",ncohort))
ch <- list()
for (i in 1:ncohort)
  { ch[[i]] <- VirtualPop::Sim_bio (datsim=popsim[i,],ratesM=rates$ratesM)
}

## ----comment=""---------------------------------------------------------------
# Age at first birth
ageFbirth  <- sapply(ch,function(x)
  { j <- x$ages_trans[1]
})
ageFbirth <- ageFbirth [ageFbirth >0]
## Mean and median ages at first birth
mean(ageFbirth)
median(ageFbirth)
sd(ageFbirth)

## ----message=FALSE,fig.align="center", fig.cap=paste0("Simulated ages at motherhood, ",countrycode,", ",refyear), out.width=400,out.extra='angle=0'----
require (ggplot2)
d <- data.frame(age=ageFbirth)
p <- ggplot (data=d,aes(age))
p <- p + geom_histogram(aes(age,after_stat(density)),alpha=0.5,position="identity",bins=50)  
p <- p + geom_density(alpha=0.2,col="red")
p

## ----echo=FALSE---------------------------------------------------------------
country <- "USA"

## -----------------------------------------------------------------------------
ncohort <- 1000
dLH <- GetGenerations (rates,ncohort=ncohort,ngen=5) 

## ----getdLH-------------------------------------------------------------------
utils::data(dLH,package="VirtualPop")

## ----eval=FALSE---------------------------------------------------------------
#  path <- paste0("Folder to save the virtual population (dLH).",
#               "To be defined by the user.")
#  ngen <- max(dLH$gen)-1
#  save (dLH,file=paste0(path,"dLH_",countrycode,"_",refyear,"_",ngen,"_",".RData"))

## ----eval=FALSE---------------------------------------------------------------
#  load(file=paste0(path,"dLH_",countrycode,"_",refyear,"_",ngen,"_",".RData"))

## ----tab1,echo=FALSE----------------------------------------------------------
country <- "the United States of America"
countrycode <- "USA"
refyear <- 2021
ncohort <- length(dLH$ID[dLH$gen==1])
ngen <- max(dLH$gen)
z <-  as.data.frame.matrix(table (Gen=dLH$gen,
                                  Gender=dLH$sex),rownames.force=TRUE)
tab <- data.frame(Gen=1:ngen,z)
tab$Total <- apply(tab[,-1],1,sum)

## -----------------------------------------------------------------------------
dLHc <- VirtualPop::GetGenerations (rates=ratesC,ncohort=1000,ngen=5) 

## ----eval=FALSE---------------------------------------------------------------
#  refyear <- attr(dLHc,"refyear")
#  cohort <- attr(dLHc,"cohort")
#  save (dLHc,file=paste0(path,"dLHc_",countrycode=countrycode,"_",
#                         cohort,"_",refyear,".rda"))

## ----echo=FALSE---------------------------------------------------------------
dLH <- dLHc
refyear <- attr(dLHc,"refyear")
cohort <- attr(dLHc,"cohort")
ncohort <- length(dLH$ID[dLH$gen==1])
ngen <- max(dLH$gen)-1
z <-  as.data.frame.matrix(table (Gen=dLH$gen,
                                  Gender=dLH$sex),rownames.force=TRUE)
tab <- data.frame(Gen=1:(ngen+1),z)
tab$Total <- apply(tab[,-1],1,sum)
# Get number of couples
tab$Couples <- c(as.vector(table (dLH$gen[!is.na(dLH$IDpartner)]))/2,NA)
tab <- rbind(tab,apply(tab,2,function(x) sum(x,na.rm=TRUE)))
tab[ngen+2,1] <- "Total"
ee <- data.frame(From=rep(NA,(ngen+1),To=rep(NA,(ngen+1))))
for (j in 1:(ngen+1))
   {ee[j,1] <- format(lubridate::date_decimal(min(dLH$bdated[dLH$gen%in%c(j)])),
                    "%d-%m-%Y")
    ee[j,2] <- format(lubridate::date_decimal(max(dLH$bdated[dLH$gen%in%c(j)])),
                    "%d-%m-%Y")
  }    
z1 <- t(sapply(1:max(dLH$gen),function(x) c(min(dLH$ID[dLH$gen==x]),max(dLH$ID[dLH$gen==x]))))
colnames(z1) <-c("From","To")
tab$ID <- rbind(z1,c(1,max(z1)))
tab$Bdate <- cbind(From=c(ee[,1],ee[1,1]),To=c(ee[,2],ee[ngen,2]))
tabc <- do.call(data.frame, tab)

# Display table
out <- knitr::kable(tabc, align=rep("r",5),
             caption = paste0(" Virtual population ",countrycode,
                              " based on cohort rates (cohort ",cohort,")"),
  format = 'latex', booktabs = T,linesep = "")
kableExtra::kable_styling(out,latex_options="HOLD_position")

## ----comment=NA,echo=FALSE----------------------------------------------------
set.seed(44)
kk <- c(sample(dLH$ID[dLH$sex=="Female"],3),sample(dLH$ID[dLH$sex=="Male"],1))
df <- dLH[dLH$ID%in%kk,]
df <- df[,1:13]
knitr::kable(t(df), caption = "The dLH fataframe",
         format="latex",booktabs=TRUE,linesep = "")

## -----------------------------------------------------------------------------
date <- format(lubridate::date_decimal(2020.409), "%d %B %Y")

## ----eval=FALSE---------------------------------------------------------------
#  path <- "" # or different path
#  foreign::write.dta(dLH, paste (path,"dLH.dta",sep=""))

## ----eval=FALSE---------------------------------------------------------------
#  foreign::write.foreign(dLH,
#          paste (path,"dLH.txt",sep=""),
#          paste (path,"dLH.sps",sep=""), package="SPSS")

