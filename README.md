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
* vaccine-model.R : core model and simulation functions.
* contact-matrix.R : functions for building the contact matrix with essential workers.

### data
* BCCDC_report.csv : data for reopen validation. Data can be downloaded from [BCCDC](http://www.bccdc.ca/health-info/diseases-conditions/covid-19/data). 

### results
* Results that are shown in the priprint.

## Simple Example

