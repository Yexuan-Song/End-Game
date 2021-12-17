#Codes written by Elisha B. Are for simulations and Figures shown in the article: 
#COVID-19 endgame: from pandemic to endemic? 
#vaccination, reopening and evolution in a well-vaccinated population,
#Dec 14, 2021

source("analysis/Simpler_Model_setup.R") #setup model fit to data 
####################### Supplementary Figure #######################
###################### Stop vaccination boosters ######################3


seir2 <- function(time, state, parameters) { 
  with(as.list(c(state, parameters)), {
    vax_stop <- ifelse(time > 100,
                       0,1) #switch vaccination on/off
    
    N <- S+E+I+R+V #total population 
    dS <- mu*N-S*I*R0*gamma /N - S*nu*ve*vax_stop + w1*R +  V*w1 -mu*S - f  
    dV <- (S)*nu*ve*vax_stop  - V*w1- mu*V 
    dE <- S*I*R0*gamma /N - sigma*E - mu*E + f 
    dI <- sigma*E  - I*gamma -mu*I 
    dR <- I*gamma - w1*R - mu*R
    return(list(c(dS, dV, dE, dI, dR)))
  })
}


times <- seq(0, 500, by = 1)

#re-initializing ode 
init_scen1 <- c(S = last(output$S), V=last(output$V), E = last(output$E), I=last(output$I), R= last(output$R))



##############Varying diff levels of reopening ##################


#first scenario, R=3 (Fig 4A)
parameters_scen1 <- c(sigma=1/3, # incubation period (3 days) 
                      gamma=1/(6), #recovery rate 
                      nu =0.007, #vaccination rate: 0.7% per day 
                      mu=1/(82*365), 
                      f= 100, #importations 
                      w1=1/(2*360), # waning rate
                      ve=0.80, # efficacy 
                      R0=3.0   #reproduction number 
)


#second scenario, R=4 (Fig 4A)
parameters_scen2 <- c(sigma=1/3, # incubation period (3 days) 
                      gamma=1/(6), #recovery rate 
                      nu =0.007, #vax rate: 0.7% per day 
                      mu=1/(82*365), 
                      f= 100, #importations 
                      w1=1/(2*360), # waning rate
                      ve=0.80, # efficacy 
                      #R0 mult parameters 
                      R0=4
)

#Third scenario, R=5 (Fig 4A)
parameters_scen3 <- c(sigma=1/3, # incubation period (3 days) 
                      gamma=1/(6), #recovery rate 
                      nu =0.007, #vax rate: 0.7% per day 
                      mu=1/(82*365), 
                      f= 100, #importations 
                      w1=1/(2*360), # waning rate
                      ve=0.80, # efficacy 
                      #R0 mult parameters 
                      R0=5
)


#solve model equation numerically  
output_scen1 <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen1))
output_scen1 <- output_scen1 %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen1<- output_scen1 %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))

output_scen2 <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen2))
output_scen2 <- output_scen2 %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen2<- output_scen2 %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))


output_scen3 <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen3))
output_scen3 <- output_scen3 %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen3<- output_scen3 %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))




#plot (Fig S2A)


cols <- c("R=3" = "#009E73", "R=4" ="#377EB8", "R=5" ="#D55E00")

g2_var_reop <- ggplot() + geom_line(aes(x=output$date,y=output$incid), size =1.8) +
  geom_line(aes(x=output_scen1$date,y=output_scen1$incid,colour="R=3"), size =1.8) +
  geom_line(aes(x=output_scen2$date,y=output_scen2$incid,colour="R=4"), size =1.8) +
  geom_line(aes(x=output_scen3$date,y=output_scen3$incid,colour="R=5"), size =1.8) +
  geom_point(aes(x=BC_dat$Reported_Date, y=BC_dat$cases/50.71),size=1.8, alpha=0.45, colour="grey48") +
  ylab("Daily number of cases per 100K") + xlab("Date") + theme_bw() +
  scale_x_date(date_labels = "%b-%d-%y", breaks = "5 months" ) +theme_pub  + 
  scale_y_continuous(limits=c(0, max(output_scen3$incid) * 1.4)) +
  labs(colour="",title="Various levels of reopening") +# geom_vline(xintercept=ymd("2021-08-07"), linetype="dashed", 
  #                    color = "gray78", size=2) + 
  scale_color_manual(values = cols) + geom_vline(xintercept=ymd(last(BC_dat$Reported_Date)+15), linetype="dotdash", 
                                                 color = "gray55", size=1.8) +
  geom_vline(xintercept=ymd("2022-03-26"), linetype="dashed", 
             color = "orange", size=2) 



##############varying waning rates ##################


#first scenario  D = 1 years(Fig 4B)
parameters_scen1_w <- c(sigma=1/3, # incubation period (3 days) 
                        gamma=1/(6), #recovery rate 
                        nu =0.007, #vax rate: 0.7% per day 
                        mu=1/(82*365), 
                        f= 100, #importations 
                        w1=1/(1*360), # waning rate
                        ve=0.80, # efficacy 
                        #R0 mult parameters 
                        R0=3.5
)

#second  scenario D = 2 years (Fig 4B)
parameters_scen2_w <- c(sigma=1/3, # incubation period (3 days) 
                        gamma=1/(6), #recovery rate 
                        nu =0.007, #vax rate: 0.7% per day 
                        mu=1/(82*365), 
                        f= 100, #importations 
                        w1=1/(2*360), # waning rate
                        ve=0.80, # efficacy 
                        #R0 mult parameters 
                        R0=3.5
)


#third scenario  D = 3 years(Fig 4B)
parameters_scen3_w <- c(sigma=1/3, # incubation period (3 days) 
                        gamma=1/(6), #recovery rate 
                        nu =0.007, #vax rate: 0.7% per day 
                        mu=1/(82*365), 
                        f= 100, #importations 
                        w1=1/(3*360), # waning rate
                        ve=0.80, # efficacy 
                        #R0 mult parameters 
                        R0=3.5
)



#solve model equation numerically   

output_scen1_w <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen1_w))
output_scen1_w <- output_scen1_w %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen1_w<- output_scen1_w %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))

output_scen2_w <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen2_w))
output_scen2_w <- output_scen2_w %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen2_w<- output_scen2_w %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))


output_scen3_w <- as.data.frame(ode(y = init_scen1, times = times, func = seir2, parms =parameters_scen3_w))
output_scen3_w <- output_scen3_w %>% mutate(incid=E*sigma*asfrac/50.71) %>% mutate(tot_vax=cumsum(ve*nu*S))# per 100K 
output_scen3_w <- output_scen3_w %>% mutate(date=seq.Date(ymd(last(output$date)),ymd(last(output$date))-1+length(times), 1))




#plot (Fig S2B)

cols <- c("D = 3" = "#009E73", "D = 2" ="#377EB8", "D = 1 year" ="#D55E00")

g_var_w <- ggplot() + geom_line(aes(x=output$date,y=output$incid), size =1.8) +
  geom_line(aes(x=output_scen1_w$date,y=output_scen1_w$incid,colour="D = 1 year"), size =1.8) +
  geom_line(aes(x=output_scen2_w$date,y=output_scen2_w$incid,colour="D = 2"), size =1.8) +
  geom_line(aes(x=output_scen3_w$date,y=output_scen3_w$incid,colour="D = 3"), size =1.8) +
  geom_point(aes(x=BC_dat$Reported_Date, y=BC_dat$cases/50.71),size=1.8, alpha=0.45, colour="grey48") +
  ylab("") + xlab("Date") + theme_bw() +
  scale_x_date(date_labels = "%b-%d-%y", breaks = "5 months" ) + theme_pub + 
  scale_y_continuous(limits=c(0, max(output_scen1_w$incid) * 1.4)) +
  labs(colour="R=3.5",title="Varying duration of immunity") +
  scale_color_manual(values = cols) + geom_vline(xintercept=ymd(last(BC_dat$Reported_Date)+15), linetype="dotdash", 
                                                 color = "gray55", size=1.8) +
  geom_vline(xintercept=ymd("2022-03-26"), linetype="dashed", 
             color = "orange", size=2) 




#combine figures (Fig S2)
gg <- ggarrange(g2_var_reop,g_var_w,  common.legend = FALSE,
                labels = c("A", "B"), nrow=1,font.label = list(size = 15, color="Orange"))
ggsave(path = "results",filename = "simp_model_proj_supl.pdf", width = 13.5, height = 5,gg)

