% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sim_bio.R
\name{Sim_bio}
\alias{Sim_bio}
\title{Generic Function to Generate Single Life History}
\usage{
Sim_bio(datsim, ratesM)
}
\arguments{
\item{datsim}{Dataframe with, for each individual, ID, date of birth, starting 
and ending times (ages) of the simulation, and the state occupied at the start 
of the simulation (see vignette "Tutorial").}

\item{ratesM}{Multistate transition rates in standard (multistate) format}
}
\value{
\item{age_startSim}{Age at start of simulation}
\item{age_endSim}{Age at end of simulation} \item{nstates}{Number of states}
\item{path}{path: sequence of states occupied} \item{ages_trans}{Ages at
transition}
}
\description{
The function generates a single life history from age-specific transition rates (rates$ratesM) and an initial state. 
RatesM is an object with the rates in the proper format for multistate modelling. The user supplies the 
starting age and ending age of the simulation.
}
\details{
The function is called from the function VirtualPop::Children(). It uses the rpexp() function of the msm package.
}
\examples{

  # Fertily history is simulated from starting age to ending age
  # Individual starts in state "par0"
  utils::data(rates,package="VirtualPop")
  popsim <- data.frame(ID=1,born=2000.450,start=0,end=80,st_start="par0")
  ch <- VirtualPop::Sim_bio (datsim=popsim,ratesM=rates$ratesM) 

}
