library(tidyverse)

source("./R/Distributions.R")
source("./R/ClassDefinitions.R")
source("./Data/Diseases.R")
source("./R/loadData.R")
source("./R/estimationFunctions.R")

#Load all the data and assumptions
NNDSSIncidenceAgegroup <- getCasesAgeGroup()
AusPopAgegroup <- getAusPopAgeGroup()
Hospitalisations <- getHospitalisationsAgeGroup()
Costs <- getCosts()
WTPValues <- getWTP()
VSL <- getValueStatisticalLife()
Deaths <- getABSDeaths()
MissedDaysGastro <- getMissedDaysGastro()
Incomes <- getIncomeData() ##What year is this for and does it need to change by year?
FrictionRates <- getFrictionRates()


# Draw from all distributions
ndraws <- 10^3
WorkingDiseases <- c("Campylobacteriosis","Salmonellosis","Shigellosis")


### CURRENTLY THE ESTIMATES COSTS FUNCTION PULLS ON INCIDENCE LIST AS A GLOBAL
### VARIABLE WITH NO REGARD FOR WHICH YEAR INCIDENCE LIST WAS CALCULATED FOR.
### I.E. YOU CAN CALCULATE INCIDENCE FOR YEAR 2019 THEN CALCULATE COSTS FOR YEAR
### 2020 WITHOUT GETTING AN ERROR. FIX BY MAKING INCIDENCE A LIST OF LISTS
### WITH AND ENTRY FOR EACH YEAR?
IncidenceList <- makeIncidenceList(2019,
                                   diseases = DiseaseAssumptions[WorkingDiseases],
                                   ndraws = ndraws)
SequelaeFractions <- calcSequelaeFractions(IncidenceList$Sequel)
DeathsList <- makeDeathsList(2019,
                             diseases = c(DiseaseAssumptions[WorkingDiseases],SequelaeAssumptions),
                             ndraws = ndraws)
CostList <- makeCostList(2019, DiseaseAssumptions[WorkingDiseases], ndraws, discount = 0.07)



#Summarise draws with median, and 90 CIs

CostTable <- CostList %>% as.data.frame(check.names = F) %>%
  mutate(Draw = row.names(.)) %>%
  pivot_longer(-Draw, names_sep = "\\.",
               names_to = c("Pathogen", "AgeGroup", "Disease","CostItem")) %>%
  group_by(Pathogen, AgeGroup, Disease, CostItem) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))

write.csv(CostTable,'./R/CostTable.csv')


