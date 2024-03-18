## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center")
knitr::opts_chunk$set(fig.pos = "H", out.extra = "") 
## No scientific notation
options(scipen=999)

## -----------------------------------------------------------------------------
Rate_pw <- function (t,breakpoints,rates)
{ int <- findInterval(t,breakpoints,all.inside=TRUE)
  z <- rates[int]
  sojournInt <- t-breakpoints[int]
  h <- data.frame(t=t,rate=z,interval=int,startInt=breakpoints[int],
                  sojourn=sojournInt)
  return(h)
}

## ----comment=""---------------------------------------------------------------
breakpoints <- c(0, 10, 20, 30, 60)
rates <- c(0.01,0.02,0.04,0.15)
Rate_pw(t=18.3,breakpoints,rates)

## -----------------------------------------------------------------------------
res <- Rate_pw(t=c(10,18.3,23.6,54.7),breakpoints,rates)  
out <- knitr::kable(res,
  caption = "Simulated waiting times: piecewise-exponential distribution",
  format = 'latex', booktabs = T,linesep = "")
kableExtra::kable_styling(out,latex_options="HOLD_position")

## ----fig1,fig.cap="Piecewise-constant hazard rates",fig.align="center"--------
max <- max(breakpoints)
xx <- seq(0,max, by=0.1)
## xx<-  18:25 # 25 is maximum (24 is last time interval)
h <- Rate_pw(xx, breakpoints,rates=rates)
require (ggplot2)
p <- ggplot (data=h,aes(x=t,y=rate))
p <- p + geom_line(aes(group=interval)) 
p <- p + xlab("Time")+ylab("Hazard hazard")
p <- p +  scale_x_continuous(breaks=seq(0,60,by=10)) 
p <- p + scale_y_continuous (breaks=seq(0,0.16,by=0.04)) 
p

## ----comment=""---------------------------------------------------------------
z <- VirtualPop::H_pw(t=c(10,18.3,23.6,54.7),breakpoints,rates)  
print(z)

## ----fig.align="center", fig.cap="Cumulative hazard",out.extra='angle=0',message=FALSE----
t <- 0:40
H <- VirtualPop::H_pw(t=t,breakpoints,rates) 
data <- data.frame(x=0:40,y=H)
require (ggplot2)
p <- ggplot (data=data,aes(x=x,y=y))
p <- p + geom_line()  
yy <- -log(1-0.35)
z <- which.min(abs(H-yy))
p <- p + geom_segment(aes(x = 0, y = yy, xend = t[z], yend = yy),
                      arrow = arrow(length = unit(0.5, "cm")),
                      linetype="dashed")
p <- p + geom_segment(aes(x = t[z], y = yy, xend = t[z], yend = 0),
                      arrow = arrow(length = unit(0.5, "cm")),
                      linetype="dashed")
p <- p + xlab("Time")+ylab("Cumulative hazard")
p <- p + annotate(geom="text", x=0, y=yy+0.05, 
                  label=as.character (round(yy,4)))
p <- p + annotate(geom="text", x=t[z]+1.4, y=0, label="23.27")
p

## ----fig.align="center", fig.cap="Survival function",out.extra='angle=0',message=FALSE----
t <- 0:40
H <- VirtualPop::H_pw(t=t,breakpoints,rates) 
data <- data.frame(x=0:40,y=exp(-H))
require (ggplot2)
p <- ggplot (data=data,aes(x=x,y=y))
p <- p + geom_line()  
yy <- 0.65
z <- which.min(abs(exp(-H)-yy))
p <- p + geom_segment(aes(x = 0, y = yy, xend = t[z], yend = yy),
                      arrow = arrow(length = unit(0.5, "cm")),linetype="dashed")
p <- p + geom_segment(aes(x = t[z], y = yy, xend = t[z], yend = 0),
                      arrow = arrow(length = unit(0.5, "cm")),linetype="dashed")
p <- p + xlab("Time")+ylab("Cumulative hazard")
p <- p + annotate(geom="text", x=0, y=yy+0.05, label=as.character (round(yy,4)))
p <- p + annotate(geom="text", x=t[z]+1.4, y=0, label="23.27")
p

## ----comment=""---------------------------------------------------------------
pw_root <- function(t, breakpoints,rates, uu){
    aa <- VirtualPop::H_pw(t, breakpoints,rates) + log(uu)
    # Cum hazard rate should be equal to - log(u), 
    # with u a value of survival function
    return(aa)
}
pw_root (t= c(10,18.3,23.6,54.7),breakpoints,rates,uu=0.43)

## ----comment=""---------------------------------------------------------------
interval=c(breakpoints[1],breakpoints[length(breakpoints)])
uniroot(f=pw_root,interval=interval,breakpoints,rates,uu=0.65)

## -----------------------------------------------------------------------------
pw_sample <- VirtualPop::r.pw_exp (n=1000, breakpoints, rates=rates)

## -----------------------------------------------------------------------------
msm::qpexp((1-0.65),rates,breakpoints[-length(breakpoints)])

## -----------------------------------------------------------------------------
nsample <- 10000
pw.sample.msm <- msm::rpexp (n=nsample,
                rate=rates,
                t=breakpoints[-length(breakpoints)])
m <- mean(pw.sample.msm)
s <- sd(pw.sample.msm)

## ----2_pwc3,warning=FALSE,message=FALSE,fig.cap="Frequency distribution of waiting times (times to event)"----
require (msm)
# x <- msm::rpexp(n=nsample,rates,breakpoints[-length(breakpoints)])
my.lower <- breakpoints[-length(breakpoints)]
my.upper <- breakpoints[-1]
hist (pw.sample.msm,breaks=100,freq=FALSE,xlim=c(0,max(my.upper)),las=1,
      ylim=c(0,0.07),
      xlab="Duration",cex.axis=0.8,cex.main=0.8,main="")
curve (dpexp(x,rates,breakpoints[-length(breakpoints)]),
       add=TRUE,lwd=2,col="red")
box()

## ----message=FALSE------------------------------------------------------------
# Get the data object "rates" from the VirtualPop package
rates <- NULL
data(rates,package="VirtualPop")
# paramters
countrycode <- attr(rates,"country")
refyear <- attr(rates,"year")
z <- rownames(rates$ASDR)
ages <- as.numeric(z)
breakpoints <- c(ages,120)
# rates
ratesm <- rates$ASDR[,1]
ratesf <- rates$ASDR[,2]
# Create dataframe with individual ages at death
nsample <- 2000 # sample size
d <- data.frame(sex=sample(x=c(1,2),size=nsample,replace=TRUE,prob=c(0.5,0.5)))
d$sex <-factor(d$sex,levels=c(1,2),labels=c("Male","Female"))
nmales <- length(d$sex[d$sex=="Male"])
nfemales <- length(d$sex[d$sex=="Female"])
d$x_D <- NA
d$x_D[d$sex=="Male"] <- VirtualPop::r.pw_exp (n=nmales, breakpoints, 
                                              rates=ratesm)
d$x_D[d$sex=="Female"] <- VirtualPop::r.pw_exp (n=nfemales, breakpoints, 
                                                rates=ratesf)

## -----------------------------------------------------------------------------
nsample <- 20000
dd <- data.frame(ID=1:nsample)
dd$bdated <- 2000
dd$sex <- sample(x=c(1,2),size=nsample,replace=TRUE,prob=c(0.5,0.5))
dd$sex <- factor(dd$sex,levels=c(1,2),labels=c("Male","Female"))
dd <- VirtualPop::Lifespan(data=dd, ASDR=rates$ASDR, mort = NULL) 

## ----fig.align="center", fig.cap=paste0("Simulated ages at death, ",countrycode,", ",refyear), out.extra='angle=0'----

dd$x_D[dd$x_D>110] <- 110
binwidth <- (max(dd$x_D,na.rm=TRUE)-min(dd$x_D,na.rm=TRUE))/60
require (ggplot2)
p <- ggplot() +
  geom_histogram(data=dd,aes(x=x_D,color=sex,fill=sex,y=after_stat(density)), 
                 alpha=0.5,position="dodge",binwidth=binwidth)
xmin <- 0
xmax <- 110
p <- p + xlab("Age")+ylab("Density")
p <- p +  scale_x_continuous(breaks=seq(xmin,xmax,by=10)) 
p <- p + scale_y_continuous (breaks=seq(0,0.04,by=0.005)) 
p <- p + theme(legend.position = c(0.1, 0.85))
p

## ----test15-------------------------------------------------------------------
nsample <- 1000
breakpoints <- c(0, 10, 20, 30, 60)
rates <- c(0.01,0.02,0.04,0.15)
pw_sample <- VirtualPop::r.pw_exp (n=nsample, breakpoints, rates=rates)
KM <- survival::survfit(survival::Surv(pw_sample)~1)

## ----2_test16,fig.cap="Piecewise-constant survival function"------------------
max <- max(breakpoints)
x <- seq(0,max, by=0.1)
# Cumulative hazard
H <- VirtualPop::H_pw(x, breakpoints,rates)
dd <- data.frame(x=x,y=exp(-H))
# Plot KM estimator
p <- survminer::ggsurvplot(KM,data=data.frame(pw_sample),conf.int = TRUE, 
                           ggtheme = theme_bw())
p <- p$plot + geom_step(data=dd,mapping=aes(x=x,y=y),linetype="dashed",
                        color="blue")
p <- p + xlab("Duration") +  ylab("Survival function")
p <- p + geom_text(x=45, y=0.8, label="KM estimate of survival function",
                   color="red")
p <- p + geom_text(x=45, y=0.7, label="Survival function",color="blue")
p <- p + theme(legend.position="none")
p

## ----2_test17-----------------------------------------------------------------
breakp2 <- breakpoints[2:(length(breakpoints)-1)]
x <- seq(0,max, by=0.2)
## waiting time = pw.sample  or
pw <- msm::rpexp(n=nsample,rate=rates,t=breakpoints[-length(breakpoints)])
pw_rates <- eha::piecewise (enter=rep(0,nsample),exit=pw,
                            event=rep(1,nsample),cutpoints=breakp2)
aa <-data.frame(From=breakpoints[-length(breakpoints)],
                To=breakpoints[-1],
                Events=pw_rates$events,
                Exposure=round(pw_rates$exposure,0),
                Rates_estimated=round(pw_rates$intensity,4),
                Rates_input=rates)
out2 <- knitr::kable(aa,
caption = paste0("Computation of hazard rates in piecewise-exponential",
     "model with simulated data"),
  format = 'latex', booktabs = T,linesep = "")
kableExtra::kable_styling(out2,latex_options="HOLD_position")

