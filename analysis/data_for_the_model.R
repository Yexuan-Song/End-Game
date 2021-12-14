library(CanCovidData)
library(dplyr)
library(viridis)

Sys.setlocale("LC_ALL","English")

bci0 <- c(133.0, 241.0, 488.8, 364.0, 315.2, 243.2, 203.2, 111.2, 155.0, 122.2,  91.0,  78.8, 60.8,  50.8,  27.8) # estimated prevalence by age in BC in Jan 2021

agefracs=bci0/sum(bci0) # approx division of init prevalence by age

provincial_data <- get_canada_official_provincial_data() 

bcdat = provincial_data %>% filter(shortProvince == "BC") %>% select(Date, `Official cases`) %>% rename(date = Date, cases=`Official cases`)


total_cases = filter(bcdat, date > ymd("2020-02-27") & date <= ymd("2021-10-04")) %>% select(cases) %>% sum() 

ascFrac = 0.7
initRecovered = (total_cases/ascFrac) * agefracs 


init_prev = filter(bcdat, date > ymd("2020-09-20") & date <= ymd("2021-10-04")) %>% select(cases) %>% sum() 

I0 = c(150,250,260,220,170,150,110,70,90,260,220,170,150,110,70)