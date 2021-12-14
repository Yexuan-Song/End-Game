# COIVD-19 End game
This analysis accompanies the text, **COVID-19 endgame: from pandemic to endemic? vaccination, reopening and evolution in a well-vaccinated population**. Elisha B. Are, Yexuan Song, Jessica E. Stockdale, Paul Tupper, Caroline Colijn. The preprint is here (to be added).

## Abstract 
COVID-19 remains a major public health concern, with large resurgences even where there has been widespread uptake of vaccines. Waning immunity and the emergence of new variants will shape the long-term burden and dynamics of COVID-19. We explore the transition to the endemic state, and the endemic incidence, using a combination of modelling approaches. We compare gradual and rapid reopening and reopening at different vaccination levels. We examine how the eventual endemic state depends on  the duration of immunity, the rate of importations, the efficacy of vaccines and the transmissibility. These depend on the evolution of the virus, which continues to undergo selection. Slower reopening leads to a lower peak level of incidence and fewer overall infections: as much as a 60\% lower peak and a 10\% lower total in some illustrative simulations; under realistic parameters, reopening when 70\% of the population is vaccinated leads to a large resurgence in cases. The long-term endemic behaviour may stabilize as late as January 2023, with further waves of high incidence occurring depending on the transmissibility of the prevalent variant, duration of immunity,  and antigenic drift. We find that long term endemic levels are not necessarily lower than current pandemic levels: in a population of 100,000 with representative parameter settings (Reproduction number 5, 1-year duration of immunity, vaccine efficacy at 80\% and importations at 3 cases per 100K per day) there are over 100 daily incident cases in the model. The consequent burden on health care systems depends on the severity of infection in immunized or previously infected individuals. 

The priprint is here (To be Added).

The core model and simulation functions are adapted from [Bubar et al.](https://github.com/kbubar/vaccine_prioritization) and [Mulberry et al.](https://github.com/nmulberry/essential-workers-vaccine#strategies-for-vaccine-allocation-with-essential-workers). 

## Data collection and code contribution
* Age and Contact Structured Model: Yexuan Song
* SVEIRS Model: Elisha B. Are


## Code Layout
### analysis
* age_and_contact_structured_model.R : core simulation function in the preprint.
* contact-matrix.R : functions for building the contact matrix.
* plot_figures.R : functions for generating figures in the preprint.
* setup.R : setup parameters for the simulation.
* utils.R : functions for plotting and summarizing.
* vaccine-model.R : core model and simulation functions.

### data
* BCCDC_report.csv : data for reopen validation. Data can be downloaded from [BC Centre for Disease Control](http://www.bccdc.ca/health-info/diseases-conditions/covid-19/data). 

### results
* Results that are shown in the priprint.

## Simple Example
```R
# define parameters and load data

source('setup.R')

# define two scenarios, comparing gradual reopening vs rapid reopening over 900 days. Set R=2.4 for reopening.
df1 <- run_over_scen_end_game(ve=0.75, vp=0.9, rapid=FALSE, bc_scen=TRUE, reopenR=2.4, ramp_T=300, days=900)
df2 <- run_over_scen_end_game(ve=0.75, vp=0.9, rapid=TRUE, bc_scen=TRUE, reopenR=2.4, ramp_T=300, days=900)

# compare trajectories

trajectories <- compare_simulation(df1,df2,name1 = "gradual reopening",name2 = "rapid reopening")
ggarrange(plotlist=trajectories,align="v", nrow = 3, ncol=2,common.legend = TRUE, legend = "bottom")
```

## package and software versions
