compare_simulation = function(df1,df2,name1,name2){

ac <- subset(bcdat, date > "2020-12-31" & date < "2021-10-15")
Es_final_df1    = df1[,grep("E", colnames(df1))]
incid_final_df1 = data.frame(time=df1$time, incid=1/3*rowSums(Es_final_df1))

Es_final_df2   = df2[,grep("E", colnames(df2))]
incid_final_df2 = data.frame(time=df2$time, incid=1/3*rowSums(Es_final_df2))

a = incid_final_df1
b = incid_final_df2
find_cul = function(inc){
  dt = data.frame()
  l = length(inc$time)
  for (i in 1:l){
    time = inc$time[i]
    incid = sum(inc$incid[1:i])
    d = data_frame(time,incid)
    dt = rbind(dt,d)
  }
  return(dt)
}

culmu_df1 = find_cul(a)
culmu_df2 = find_cul(b)


startDate =  lubridate::ymd("2021-01-01")
culmu_df1$date = startDate + culmu_df1$time 
culmu_df2$date = startDate + culmu_df2$time 
endDate = lubridate::ymd("2024-01-01")

p_cul_df1=ggplot(data = filter(culmu_df1,startDate+15< date & date<endDate), 
                     aes(x=date,y=incid)) + theme_light() +
  geom_area(alpha=0.7,fill="lightblue") + theme(axis.title.x = element_blank(), text=element_text(size=8),
                                                axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Incident")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y") +
  scale_fill_viridis(discrete = TRUE, option = "D",begin = 0.5)+ggtitle(name1)+ ylim(0,1250000)+
  theme(legend.position = "bottom")

p_cul_df2=ggplot(data = filter(culmu_df2,startDate+15< date & date<endDate), 
                   aes(x=date,y=incid)) + theme_light() +
  geom_area(alpha=0.7,fill="lightblue") + theme(axis.title.x = element_blank(), text=element_text(size=8),
                                                axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Incident")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y") +
  scale_fill_viridis(discrete = TRUE, option = "D",begin = 0.5)+ggtitle(name2)+ ylim(0,1250000)+
  theme(legend.position = "bottom")




allcases = select(df2,  E1, E2, E3, E4, E5, E6, E7, E8, E9) +
  select(df2,  Ev1, Ev2, Ev3, Ev4, Ev5, Ev6, Ev7, Ev8, Ev9)+
  select(df2,  Ex1, Ex2, Ex3, Ex4, Ex5, Ex6, Ex7, Ex8, Ex9)

mydatC = cbind(select(df2, time), allcases*1/3) %>%
  rename( `0-9`=E1, `10-19`=E2, `20-29`=E3, `30-39`=E4,
          `40-49`=E5, `50-59`=E6, `60-69`=E7, `70-79`=E8, `80+`=E9)   %>%
  pivot_longer(cols = 2:10, names_to = "age_band", values_to = "cases") %>%
  mutate(age_band = as.factor(age_band))

startDate =  lubridate::ymd("2021-01-01")
mydatC$date =  mydatC$time + startDate 

p1=  ggplot(data = filter(mydatC,startDate+15< date & date<endDate), aes(x=date, y=cases*1e5/pop_total, fill=age_band))+theme_light()+
  geom_area(position="stack",alpha=0.7)+ 
  theme(axis.title.x = element_blank(), 
        text=element_text(size=8),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Reported Cases per 100K")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y")+
  ggtitle(name2)+ ylim(0,80)+
  #geom_point(ac,inherit.aes = F,mapping = aes(x=date,y=cases*1e5/pop_total),color="black",alpha=0.3)+
  geom_vline(xintercept=as.numeric(as.Date("2021-12-17")),color = "black",linetype="dotdash")


allcases = select( df1,  E1, E2, E3, E4, E5, E6, E7, E8, E9) +
  select(df1,  Ev1, Ev2, Ev3, Ev4, Ev5, Ev6, Ev7, Ev8, Ev9)+
  select(df1,  Ex1, Ex2, Ex3, Ex4, Ex5, Ex6, Ex7, Ex8, Ex9)

mydatC = cbind(select(df1, time), allcases*1/3) %>%
  rename( `0-9`=E1, `10-19`=E2, `20-29`=E3, `30-39`=E4,
          `40-49`=E5, `50-59`=E6, `60-69`=E7, `70-79`=E8, `80+`=E9)   %>%
  pivot_longer(cols = 2:10, names_to = "age_band", values_to = "cases") %>%
  mutate(age_band = as.factor(age_band))

startDate =  lubridate::ymd("2021-01-01")
mydatC$date =  mydatC$time + startDate 

p2=  ggplot(data = filter(mydatC,startDate+15< date & date<endDate), aes(x=date, y=cases*1e5/pop_total, fill=age_band))+theme_light()+
  geom_area(position="stack",alpha=0.7)+ 
  theme(axis.title.x = element_blank(), 
        text=element_text(size=8),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Reported Cases per 100K")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y")+
  ggtitle(name1)+ ylim(0,80)+
  # geom_point(ac,inherit.aes = F,mapping = aes(x=date,y=cases*1e5/pop_total),color="black",alpha=0.3)+
  geom_vline(xintercept=as.numeric(as.Date("2021-12-17")),color = "black",linetype="dotdash")


######################################################
#get_hospitalization


mydatC = add_hosp_lc(df2, hosp_efficacy=unique(df2$vp)) %>% 
  select(time, H1, H2, H3, H4, H5, H6, H7, H8, H9) %>% 
  rename( `0-9`=H1, `10-19`=H2, `20-29`=H3, `30-39`=H4,
          `40-49`=H5, `50-59`=H6, `60-69`=H7, `70-79`=H8, `80+`=H9)   %>%
  pivot_longer(cols = 2:10, names_to = "age_band", values_to = "cases") %>%
  mutate(age_band = as.factor(age_band))


hlc_df2 = add_hosp_lc(df2, hosp_efficacy=unique(df2$vp))

Es_final_df2    = hlc_df2[,grep("H", colnames(hlc_df2))]
incid_final_df2 = data.frame(time=hlc_df2$time, incid=1/3*rowSums(Es_final_df2))
#write.csv(incid_final_df2,"hos_rapid.csv")
rapid_case <- colSums(incid_final_df2)


hlc_df1 = add_hosp_lc(df1, hosp_efficacy=unique(df1$vp))

Es_final_df1    = hlc_df1[,grep("H", colnames(hlc_df1))]
incid_final_df1 = data.frame(time=hlc_df1$time, incid=1/3*rowSums(Es_final_df1))
#write.csv(incid_final_gradual,"hos_gradual.csv")
gradual_case <- colSums(incid_final_df1)



startDate =  lubridate::ymd("2021-01-01")
mydatC$date =  mydatC$time + startDate 

p3=  ggplot(data = filter(mydatC,startDate+15< date& date<endDate), aes(x=date, y=cases*1e5/pop_total, fill=age_band))+theme_light()+
  geom_area(position="stack",alpha=0.7)+ 
  theme(axis.title.x = element_blank(), 
        text=element_text(size=8),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Hospitalization per 100K")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y")+
  ggtitle(name2)+ ylim(0,25)+
  geom_vline(xintercept=as.numeric(as.Date("2021-12-17")),color = "black",linetype="dotdash")



mydatC = add_hosp_lc(df1, hosp_efficacy=unique(df1$vp)) %>% 
  select(time, H1, H2, H3, H4, H5, H6, H7, H8, H9) %>% 
  rename( `0-9`=H1, `10-19`=H2, `20-29`=H3, `30-39`=H4,
          `40-49`=H5, `50-59`=H6, `60-69`=H7, `70-79`=H8, `80+`=H9)   %>%
  pivot_longer(cols = 2:10, names_to = "age_band", values_to = "cases") %>%
  mutate(age_band = as.factor(age_band))

startDate =  lubridate::ymd("2021-01-01")
mydatC$date =  mydatC$time + startDate 

p4=  ggplot(data = filter(mydatC,startDate+15< date& date<endDate), aes(x=date, y=cases*1e5/pop_total, fill=age_band))+theme_light()+
  geom_area(position="stack",alpha=0.7)+ 
  theme(axis.title.x = element_blank(), 
        text=element_text(size=8),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust=1))+
  ylab("Hospitalization per 100K")+scale_x_date( date_breaks = "3 months",date_labels = "%b-%y")+
  ggtitle(name1)+ylim(0,25)+
  geom_vline(xintercept=as.numeric(as.Date("2021-12-17")),color = "black",linetype="dotdash")

#ggarrange(p1,p2,p3,p4,p_cul_df2,p_cul_df1,align="v", nrow = 3, ncol=2,common.legend = TRUE, legend = "bottom")

return(list(p1,p2,p3,p4,p_cul_df2,p_cul_df1))
}
