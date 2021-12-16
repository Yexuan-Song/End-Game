#Codes written by Elisha B. Are for simulations and Figures shown in the article: 
#COVID-19 endgame: from pandemic to endemic? 
#vaccination, reopening and evolution in a well-vaccinated population,
#submitted to The Journal of the royal society interface. 
#Dec 14, 2021
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#sets directory to source file location  
rm(list=ls())
library(lubridate)
library(dplyr)
#install.packages("deSolve")
library(deSolve)
library(ggplot2)
library(lubridate)
#install.packages("ggpubr")
library(ggpubr)
library("CanCovidData")


#ode model equation 
seir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    mult = ifelse(time < 40,
                  0.67,1  #     
    )
    
    mult1 = ifelse(time > 272,
                  0.9,1 
    )
    
    #c_t is R0 multiplier 
    
    c_t = (decTrans + (1-decTrans)/(1+exp(slope1*(time-lock-delay))) +
             ((1/((incrTrans) + (1-incrTrans)/(1+exp(slope2*(time-open-delay)))))-1))
    
    N <- S+E+I+R+V #total population 
    dS <- mu*N-S*I*R0*gamma*c_t*mult*mult1 /N - S*nu*ve + w1*R +  V*w1 -mu*S - f  
    dV <- (S)*nu*ve  - V*w1- mu*V 
    dE <- S*I*R0*gamma*c_t* mult*mult1 /N - sigma*E - mu*E + f 
    dI <- sigma*E  - I*gamma -mu*I 
    dR <- I*gamma - w1*R - mu*R
    return(list(c(dS, dV, dE, dI, dR)))
  })
}


#load BC daily reported cases 
bcpub <- CanCovidData::get_british_columbia_case_data()
bcpub$Reported_Date <- lubridate::ymd(bcpub$`Reported Date`)
bcpub$thiscase <- 1
dat <- group_by(bcpub, Reported_Date) %>%
  summarise(cases = sum(thiscase)) %>%
  filter(Reported_Date >= ymd("2020-02-27") || Reported_Date <= ymd("2021-11-10"))
dat <- dat[order(dat$Reported_Date), ]
dat$day <- seq(1, nrow(dat))
dat$value <- dat$cases
BC_dat <- dat

start_date <- ymd("2021-02-02")
BC_dat <- BC_dat %>% filter(Reported_Date >= start_date) 
sigma <- 1/3
asfrac <- 0.8 # 80% ascertainment rate 
times <- seq(0,length(BC_dat$value)+15, by = 1) #length(BC_dat$value)
out <- NULL
inf <- 75660*(1/(0.3*asfrac)) # approx 75660 recovered up till Feb 02, 2021
vacc <- 172998 # vax as of  Feb 02, 2021


BC_dat

plot(BC_dat$cases)

rep_incid <- filter(BC_dat , Reported_Date == start_date)$value #reported incidence on Feb 02, 2021

incd0 <- rep_incid/(asfrac*sigma) #approx true incidence on Feb 02, 2021

init <- c(S = 5.071e6-(inf+vacc+incd0+1652), V=vacc, E = incd0, I=1652, R= inf) #initial condition 


parameters <- c(sigma=1/3, # incubation period (3 days) 
                gamma=1/(6), #recovery rate 
                nu =0.007, #vax rate: 0.7% per day 
                mu=1/(82*365), 
                f= 100, #importations 
                w1=1/(2*360), # waning rate
                ve=0.85, # efficacy 
                R0=1.89, #reproduction number  
                #R0 mult parameters 
                
                decTrans =0.01, 
                slope1=0.05, #slope of decline
                delay = 15,
                slope2 = 3,
                lock = 82, 
                open = 154,
                incrTrans <- 0.39
                
)

ve=0.85 # vax efficacy 
nu=0.007 # vax rate (0.7% per day)

output <- as.data.frame(ode(y = init, times = times, func = seir, parms = parameters))
output <- output %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output <- output %>% mutate(date=seq.Date(ymd("2021-02-02"),ymd("2021-02-01")+length(times), 1))

#R0_mult is between (0.04,1.6)
#check R0_mult ###########

decTrans <- 0.01
incrTrans <- 0.39
slope1 <- 0.05
delay <- 15
slope2 <- 4
lock <- 82
open <- 155

check_R0_mult <- (decTrans + (1-decTrans)/(1+exp(slope1*(times-lock-delay))) +
                    ((1/((incrTrans) + (1-incrTrans)/(1+exp(slope2*(times-open-delay)))))-1))
plot(times,check_R0_mult)

