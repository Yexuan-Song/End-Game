#Codes written by Elisha B. Are for simulations and Figures shown in the article: 
#COVID-19 endgame: from pandemic to endemic? 
#vaccination, reopening and evolution in a well-vaccinated population,
#submitted to The Journal of the royal society interface. 
#Dec 14, 2021


#########supplementary figures (S3) #########################################

theme_pub <- theme(axis.text=element_text(size=15),
                   plot.title = element_text(size=15, face="bold"),
                   legend.position = "bottom", legend.title = element_text(size=15),
                   legend.text = element_text(size=15),
                   axis.title=element_text(size=15,face="bold")) 



#################################### for Fig S3A ###############################


R <- seq(2,7,0.05) #reproduction number 
mu <- 1/(82*365) # life expectancy (82 y in Canada)
sigma <- 1/3
gamma <- 1/6
f <- (150/5.071e6) #f/N per day, approx 3 per day per 100K.   from phac 11.73, 12 per 100K per week
nu=0.005
ve=0.80
endemic <- NULL
endemic_state <- function(w){
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
                (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
               4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+
              4*((1/4*(R*gamma)^2*sigma^2+ ((f-1/2*mu-1/2*sigma)*gamma+
             (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+
                1/4*(mu+sigma)^2*(mu+gamma)^2)*w+ 1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+
                (-w-(nu*ve)-gamma-sigma)*mu^2+((-w-(nu*ve)+(R*gamma)-gamma)*sigma+
                (-w-(nu*ve))*gamma)*mu+((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+
                (w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  return(endemic) 
}

waning_1 <- endemic_state(1/(365)) #values of (w = 1/D)
waning_2 <- endemic_state(1/(2*365))
waning_3 <- endemic_state(1/(4*365))




#results are in I/N
df_waning_trans <- data.frame(waning_1 = waning_1*1e5/6, waning_2 = waning_2*1e5/6, waning_3 = waning_3*1e5/6, transm=R) # *1e5 to get per 100K
# *1e5 to get per 100K


lab1 <- c(expression(D~"="~1),
          expression(D~"="~2), 
          expression(D~"="~4~years))

cols <- c("D = 1 year" = "#D55E00", "D = 2 years" ="#009E73", "D = 4 years" ="#377EB8")

g_waning <- ggplot() + geom_line(aes(x=df_waning_trans$transm, y=df_waning_trans$waning_1,col="D = 1 year"),size=2) + 
  geom_line(aes(x=df_waning_trans$transm, y=df_waning_trans$waning_2, col="D = 2 years"),size=2) +
  geom_line(aes(x=df_waning_trans$transm, y=df_waning_trans$waning_3, col="D = 4 years"),size=2) +
  ylab(" ") + xlab("Reproduction Number") + theme_bw() +
  theme_pub + 
  scale_x_continuous(breaks = seq(2,6,1)) +  scale_y_continuous(limits=c(0, 200)) +
  labs( color = " ", title=expression(Varying~D~and~R["npi"](v[e]~"="~"80%"~","~f~"="~3~per~"100K"))) +
  scale_color_manual(values = cols, labels=lab1)

#,"~f~"="~3~per~"100K"


#f=0.00016,ve=85%,nu=0.7% per day



#################################### for Fig S3B ###############################

f <- seq(0/5.071e6,600/5.071e6,1/5.071e6)#seq(0.05,0.9,0.01) #seq(1,6,0.05)#seq(0.2,0.3,0.01)
nu <- 0.005
sigma <- 1/3
gamma <- 1/6
#f <- 0.271
w <- 1/(2*365) 
ve=0.80
endemic <- NULL
endemic_state <- function(R){
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
                 (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
                4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
                ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+
                1/4*(mu+sigma)^2*(mu+gamma)^2)*w+1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                  1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+
                   ((-w-(nu*ve)+(R*gamma)-gamma)*sigma+(-w-(nu*ve))*gamma)*mu+
                ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  
  return(endemic) 
}

import_1 <- endemic_state(4)
import_2 <- endemic_state(5)
import_3 <- endemic_state(6)

df_import_trans <- data.frame(import_1 = import_1*100000/6, 
                   import_2= import_2*100000/6, import_3= import_3*100000/6, import=f*100000)


lab1 <- c(expression(R["npi"]~"="~4),
          expression(R["npi"]~"="~5), 
          expression(R["npi"]~"="~6))


cols <- c("R_{npi} = 4" = "#377EB8", "R_{npi} = 5" ="#009E73", "R_{npi} = 6" = "#D55E00")

g_import <- ggplot() + geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_1, col= "R_{npi} = 4"),size=2) + 
  geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_2, col = "R_{npi} = 5"),size=2) +
  geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_3, col= "R_{npi} = 6"),size=2) +
  ylab("Endemic incidence per 100K") + xlab("Importations per 100K per day") + theme_bw() +
  theme_pub +  
  scale_x_continuous(breaks = seq(round(0,1),round((650/5.071e6)*100000,1),round((50/5.071e6)*100000,1)),
                     limits = c(0,12)) +
  labs( color = "", title="Varying R_{npi} and f (v_e=80%, D =2 years)") + 
  scale_y_continuous(limits=c(0, 80)) + #max(df_import_trans$import_3) * 1.3)
  scale_color_manual(values = cols, labels=lab1)



R <- 5#seq(2,10,0.05) #seq(1,6,0.05)#seq(0.2,0.3,0.01)
mu <- 1/(82*365)
sigma <- 1/3
gamma <- 1/6
f <- 150/5.071e6#20/5e6#0.0000005
endemic <- NULL
nu=0.005
w <- seq(1/(4*365),1/(1*365),0.00001)
endemic_state <- function(ve){
  
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
              (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
             4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
              ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+1/4*(mu+sigma)^2*(mu+gamma)^2)*w+
              1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
              1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+
               ((-w-(nu*ve)+(R*gamma)-gamma)*sigma+(-w-(nu*ve))*gamma)*mu+
              ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  return(endemic) 
}


efficacy_1 <- endemic_state(0.60)
efficacy_2 <- endemic_state(0.80)
efficacy_3 <- endemic_state(0.90)




df_efficacy <- data.frame(efficacy_1 = efficacy_1*100000/6, 
              efficacy_2 = efficacy_2*100000/6, efficacy_3 = efficacy_3*100000/6, waning=w)

lab1 <- c(expression(v[e]~"="~"60%"),
          expression(v[e]~"="~"80%"), 
          expression(v[e]~"="~"90%"))

cols <- c("v_e = 60%" = "#D55E00", "v_e = 80%" ="#009E73", "v_e = 90%" ="#377EB8")

g_efficacy <- ggplot() + geom_line(aes(x=(1/df_efficacy$waning)/365, y=df_efficacy$efficacy_1,col="v_e = 60%"),size=2) + 
  geom_line(aes(x=(1/df_efficacy$waning)/365, y=df_efficacy$efficacy_2, col="v_e = 80%"),size=2) +
  geom_line(aes(x=(1/df_efficacy$waning)/365, y=df_efficacy$efficacy_3, col="v_e = 90%"),size=2) +
  ylab(" ") + xlab("Duration of immunity (years)") + theme_bw() +
  theme_pub + 
  scale_x_continuous(breaks = seq(0,4,0.5)) +  scale_y_continuous(limits=c(0, 150)) +
  labs( color = "", title=expression(Varying~v[e]~and~D~(R["npi"]~"="~5~","~f~"="~3~per~"100K"))) +
  scale_color_manual(values = cols, labels=lab1)

#################################### for Fig S3C ###############################

R <- seq(2,7,0.05) 
mu <- 1/(82*365)
w <- 1/(2*365)
sigma <- 1/3
gamma <- 1/6
f <- (150/5.071e6)
endemic <- NULL
nu=0.005
#ve=0.75
endemic_state <- function(ve){
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
                (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
                 4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
                  ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+1/4*(mu+sigma)^2*(mu+gamma)^2)*w+
                1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+
                  ((-w-(nu*ve)+(R*gamma)-gamma)*sigma+(-w-(nu*ve))*gamma)*mu+
                ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  
  return(endemic) 
}



efficR_1 <- endemic_state(0.60)
efficR_2 <- endemic_state(0.80)
efficR_3 <- endemic_state(0.90)

df_effic_trans <- data.frame(efficR_1 = efficR_1*100000/6, efficR_2 = efficR_2*100000/6, efficR_3 = efficR_3*100000/6, transm=R)



lab1 <- c(expression(v[e]~"="~"60%"),
          expression(v[e]~"="~"80%"), 
          expression(v[e]~"="~"90%"))

cols <- c("v_e = 60%"  = "#D55E00", "v_e = 80%"  ="#009E73", "v_e = 90%"  ="#377EB8")

g_efficR <- ggplot() + geom_line(aes(x=df_effic_trans$transm, y=df_effic_trans$efficR_1,col="v_e = 60%"),size=2) + 
  geom_line(aes(x=df_effic_trans$transm, y=df_effic_trans$efficR_2, col="v_e = 80%"),size=2) +
  geom_line(aes(x=df_effic_trans$transm, y=df_effic_trans$efficR_3, col="v_e = 90%"),size=2) +
  ylab(" ") + xlab("Reproduction Number") + theme_bw() +
  theme_pub + 
  scale_x_continuous(breaks = seq(2,7,1)) +  scale_y_continuous(limits=c(0,80 )) +
  labs( color = "", title=expression(Varying~v[e]~and~R["npi"](D~"="~2~years~","~f~"="~3~per~"100K"))) +
  scale_color_manual(values = cols, labels=lab1)


#################################### for Fig S3D ###############################







f <- seq(0/5.071e6,600/5.071e6,1/5.071e6)
nu <- 0.005
sigma <- 1/3
gamma <- 1/6
#f <- 0.271
w <- 1/(2*365) 
ve=0.8
endemic <- NULL
endemic_state <- function(R){
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
               (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
                4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
                ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+1/4*(mu+sigma)^2*(mu+gamma)^2)*w+
                1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+
                 ((-w-(nu*ve)+(R*gamma)-gamma)*sigma+
                (-w-(nu*ve))*gamma)*mu+
                ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  
  
  return(endemic) 
}

import_1 <- endemic_state(4)
import_2 <- endemic_state(5)
import_3 <- endemic_state(6)

df_import_trans <- data.frame(import_1 = import_1*100000/6, 
                  import_2= import_2*100000/6, import_3= import_3*100000/6 , import=f*100000)



lab1 <- c(expression(R["npi"]~"="~4),
          expression(R["npi"]~"="~5), 
          expression(R["npi"]~"="~6))

cols <- c("R_{npi} = 4" = "#377EB8", "R_{npi} = 5" ="#009E73", "R_{npi} = 6" = "#D55E00")

g_import <- ggplot() + geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_1, col= "R_{npi} = 4"),size=2) + 
  geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_2, col = "R_{npi} = 5"),size=2) +
  geom_line(aes(x=df_import_trans$import, y=df_import_trans$import_3, col= "R_{npi} = 6"),size=2) +
  ylab("Endemic incidence per 100K") + xlab("Importations per 100K per day") + theme_bw() +
  theme_pub + 
  scale_x_continuous(breaks = seq(round(50/5.071e6,1),round((650/5.071e6)*100000,1),round((50/5.071e6)*100000,1)),
                     limits = c(0,12)) +
  labs( color = "", title=expression(Varying~R["npi"]~and~f~(v[e]~"="~"80%"~","~D~"="~2~years))) +
  scale_y_continuous(limits=c(0, 70)) + #max(df_import_trans$import_3) * 1.3)
  scale_color_manual(values = cols, labels=lab1)



#################################### for Fig S3E ###############################
R <- 5
mu <- 1/(82*365)
sigma <- 1/3
gamma <- 1/6
endemic <- NULL
nu=0.005
ve=0.8
w <- seq(1/(4*365),1/(1*365),0.00001)
endemic_state <- function(f){
  
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
               (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
              4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
                 ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+1/4*(mu+sigma)^2*(mu+gamma)^2)*w+
                   1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                   1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+((-w-(nu*ve)+
                  (R*gamma)-gamma)*sigma+(-w-(nu*ve))*gamma)*mu+
                     ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  return(endemic) 
}


importwan_1 <- endemic_state(50/5.071e6)
importwan_2 <- endemic_state(150/5.071e6)
importwan_3 <- endemic_state(300/5.071e6)

head(df_importwan)


df_importwan <- data.frame(importwan_1 = importwan_1*100000/6, 
                importwan_2 = importwan_2*100000/6, importwan_3 = importwan_3*100000/6, waning=w)


lab1 <- c(expression(f~"="~1),
          expression(f~"="~3), 
          expression(f~"="~6~per~"100K"))

cols <- c("f = 1" = "#377EB8", "f = 3" ="#009E73", "f = 6 per 100K" ="#D55E00")

g_importwan <- ggplot() + geom_line(aes(x=(1/df_importwan$waning)/365, y=df_importwan$importwan_1,col="f = 1"),size=2) + 
  geom_line(aes(x=(1/df_importwan$waning)/365, y=df_importwan$importwan_2, col="f = 3"),size=2) +
  geom_line(aes(x=(1/df_importwan$waning)/365, y=df_importwan$importwan_3, col="f = 6 per 100K"),size=2) +
  ylab("") + xlab("Duration of immunity (years)") + theme_bw() +
  theme_pub  + 
  scale_x_continuous(breaks = seq(1,4,0.5)) +  scale_y_continuous(limits=c(0, 150)) +
  labs( color = "", title=expression(Varying~f~and~D~(R["npi"]~"="~5~","~v[e]~"="~"80%"))) +
  scale_color_manual(values = cols,labels=lab1)




#################################### for Fig S3F ###############################

f <- seq(0/5.071e6,600/5.071e6,1/5.071e6)
nu <- 0.005
sigma <- 1/3
gamma <- 1/6
#f <- 0.271
w <- 1/(2*365) 
#ve=0.75
R=5
endemic <- NULL
endemic_state <- function(ve){
  endemic <-  1/2*(((mu+sigma)^2*(mu+gamma)^2*(nu*ve)^2+((4*((f-1/2*mu-1/2*sigma)*gamma+
               (mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+2*(mu+sigma)^2*(mu+gamma)^2)*w+
                 4*(mu+sigma)*(sigma*(-1/2*mu+f)*(R*gamma)+1/2*mu*(mu+sigma)*(mu+gamma))*(mu+gamma))*(nu*ve)+4*((1/4*(R*gamma)^2*sigma^2+
                ((f-1/2*mu-1/2*sigma)*gamma+(mu+sigma)*(-1/2*mu+f))*sigma*(R*gamma)+1/4*(mu+sigma)^2*(mu+gamma)^2)*w+
                1/4*(R*gamma)^2*mu*sigma^2+(mu+sigma)*(-1/2*mu+f)*(mu+gamma)*sigma*(R*gamma)+
                 1/4*mu*(mu+sigma)^2*(mu+gamma)^2)*(mu+w))^(1/2)-mu^3+(-w-(nu*ve)-gamma-sigma)*mu^2+((-w-(nu*ve)+
                (R*gamma)-gamma)*sigma+(-w-(nu*ve))*gamma)*mu+
                ((-w-(nu*ve))*gamma+w*(R*gamma))*sigma)/(mu^2+(w+gamma+sigma)*mu+(w+gamma)*sigma+gamma*w)/(R*gamma)
  
  
  return(endemic) 
}

importeffic_1 <- endemic_state(0.60)
importeffic_2 <- endemic_state(0.80)
importeffic_3 <- endemic_state(0.90)




df_importeffic_trans <- data.frame(importeffic_1 = importeffic_1*100000/6, importeffic_2= importeffic_2*100000/6,
                        importeffic_3= importeffic_3*100000/6 , import=f*100000)



lab1 <- c(expression(v[e]~"="~"60%"),
          expression(v[e]~"="~"80%"), 
          expression(v[e]~"="~"90%"))


cols <- c("v_e = 60%" = "#D55E00", "v_e = 80%" ="#009E73", "v_e = 90%" ="#377EB8")

g_importeffic <- ggplot() + geom_line(aes(x=df_importeffic_trans$import, y=df_importeffic_trans$importeffic_1
                                          , col= "v_e = 60%"),size=2) + 
  geom_line(aes(x=df_importeffic_trans$import, y=df_importeffic_trans$importeffic_2, col = "v_e = 80%"),size=2) +
  geom_line(aes(x=df_importeffic_trans$import, y=df_importeffic_trans$importeffic_3, col= "v_e = 90%"),size=2) +
  ylab(" ") + xlab("Importations per 100K per day") + theme_bw() +
  theme_pub + 
  scale_x_continuous(breaks = seq(round(0,1),round((650/5.071e6)*100000,1),round((50/5.071e6)*100000,1)),
                     limits = c(0,12)) +
  labs( color = " ", title=expression(Varying~v[e]~and~f~(R["npi"]~"="~5~","~D~"="~2~years))) + 
  scale_y_continuous(limits=c(0, 80)) + 
  scale_color_manual(values = cols,labels=lab1)


#combine figures 


gg <- ggarrange( 
  g_waning,g_efficR, g_import,
  g_importeffic, g_efficacy,g_importwan,
  common.legend = FALSE, nrow=3,ncol=2, hjust=-1.2,legend = "bottom", 
  font.label = list(size = 20, color="Orange")
  , labels = c("A","B","C","D","E","F"))

ggsave("pess_endemic_combo.png", width = 10.5, height = 11,gg)
