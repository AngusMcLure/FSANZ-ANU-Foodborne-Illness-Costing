library(tidyverse)
source("./R/Distributions.R")
source("./R/ClassDefinitions.R")
source("./Data/Diseases.R")
source("./R/loadData.R")
source("./R/estimationFunctions.R")

#Load all the data and assumptions
NNDSSIncidenceAgegroup <- getCasesNNDSSAgeGroup() %>% subset(Disease != "STEC")
StateIncidenceAgeGroup <- getCasesStateAgeGroup()
NotificationsAgeGroup <- bind_rows(NNDSSIncidenceAgegroup,StateIncidenceAgeGroup)
AusPopAgegroup <- getAusPopAgeGroup()
AusPopSingleYear <- getAusPopSingleYearAge()
Hospitalisations <- getHospitalisationsAgeGroup()
Costs <- getCosts()
WTPValues <- getWTP()
VSL <- getValueStatisticalLife()
Deaths <- getABSDeaths() %>% subset(Method == "Underlying")
MissedDaysGastro <- getMissedDaysGastro()
FrictionRates <- getFrictionRates()
Workforce <- getWorkforceAssumptions() ##What year is this for and does it need to change by year?


# Draw from all distributions
ndraws <- 10^3

### CURRENTLY THE ESTIMATES COSTS FUNCTION PULLS ON INCIDENCE LIST AS A GLOBAL
### VARIABLE WITH NO REGARD FOR WHICH YEAR INCIDENCE LIST WAS CALCULATED FOR.
### I.E. YOU CAN CALCULATE INCIDENCE FOR YEAR 2019 THEN CALCULATE COSTS FOR YEAR
### 2020 WITHOUT GETTING AN ERROR. FIX BY MAKING INCIDENCE A LIST OF LISTS
### WITH AND ENTRY FOR EACH YEAR?
IncidenceList <- makeIncidenceList(2019,
                                   pathogens = PathogenAssumptions,
                                   ndraws = ndraws,
                                   gastroRate = gastroRate)
SequelaeFractions <- calcSequelaeFractions(IncidenceList)
HospList <- makeHospList(2019,
                         IncidenceList,
                         pathogens = PathogenAssumptions,
                         ndraws = ndraws)
DeathList <- makeDeathList(2019,
                           pathogens = PathogenAssumptions,
                           ndraws = ndraws)
CostList <- makeCostList(2019, PathogenAssumptions, ndraws, discount = 0) # no discounting and assuming a 5 year duration of ongoing illness is equivalent to the cross-sectional approach if we assume that case numbers were the same over the past five years.


warning('When summing across agegroups the draws of the multipliers used for each agegroup are considered independent. Making them dependent would require reworking the whole program, and is not necessarily a better assumption, but it is something to be aware of')

#Summarise draws with median, and 90 CIs
# Can probably reuse some code here...

summariseList <- function(list){
  lapply(rapply(list, enquote, how="unlist"), eval) %>%
    as.data.frame(check.names = F) %>%
    mutate(Draw = row.names(.)) %>%
    pivot_longer(-Draw, names_sep = "\\.",
                 names_to = c("Pathogen", "Disease","AgeGroup")) %>%
    bind_rows(.,
              group_by(.,Draw, Pathogen, Disease) %>%
                summarise(value = sum(value)) %>%
                mutate(AgeGroup = 'All Ages')) %>%
    bind_rows(.,
              group_by(.,Draw, Pathogen, AgeGroup) %>%
                summarise(value = sum(value)) %>%
                mutate(Disease = 'All Diseases')) %>%
    # bind_rows(.,
    #           group_by(.,Draw, AgeGroup, Disease) %>%
    #             summarise(value = sum(value)) %>%
    #             mutate(Pathogen = 'All Pathogens')) %>%
    group_by(Pathogen, Disease, AgeGroup) %>%
    summarise(median = median(value),
              `5%` = quantile(value, 0.05),
              `95%` = quantile(value, 0.95))
}

IncidenceTable <- summariseList(IncidenceList)
HospTable <- summariseList(HospList)
DeathTable <- summariseList(DeathList)
EpiTable <- bind_rows(Deaths = DeathTable, Hospitalisations = HospTable, Cases = IncidenceTable,.id = 'Measure') %>%
  subset(!(Disease %in% names(PathogenAssumptions) & Pathogen == 'All Pathogens'))

write.csv(EpiTable,'./R/EpiTable.csv')

summariseCostList <- function(list){
  totals <- lapply(rapply(list, enquote, how="unlist"), eval) %>%
    as.data.frame(check.names = F) %>%
    mutate(Draw = row.names(.)) %>%
    pivot_longer(-Draw, names_sep = "\\.",
                 names_to = c("Pathogen", "Disease","AgeGroup","CostItem")) %>%
    bind_rows(.,
              group_by(.,Draw, Pathogen, Disease,CostItem) %>%
                summarise(value = sum(value)) %>%
                mutate(AgeGroup = 'All Ages')) %>%
    bind_rows(.,
              group_by(.,Draw, Pathogen, AgeGroup,CostItem) %>%
                summarise(value = sum(value)) %>%
                mutate(Disease = 'All Diseases'))  #%>%
  # bind_rows(.,
  #           group_by(.,Draw, Disease, AgeGroup,CostItem) %>%
  #             summarise(value = sum(value)) %>%
  #             mutate(Pathogen = 'All Pathogens'))

  Detailed <-  totals %>%
    group_by(Pathogen, AgeGroup, Disease, CostItem) %>%
    summarise(median = median(value),
              `5%` = quantile(value, 0.05),
              `95%` = quantile(value, 0.95))

  DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
  WTPCat <- c('WTP', 'WTPOngoing')
  LostProdCat <- c("HumanCapital","FrictionHigh", "FrictionLow")

  Categorised <- totals %>%
    subset(CostItem %in% c("Deaths",LostProdCat, paste0("Total.",LostProdCat))) %>%
    bind_rows(group_by(totals,Draw,Pathogen, AgeGroup, Disease) %>%
                summarise(Direct = sum(value[CostItem %in% DirectCat]),
                          WTP = sum(value[CostItem %in% WTPCat])) %>%
                pivot_longer(Direct:WTP,names_to = 'CostItem', values_to = 'value')) %>%
    rename(CostCategory = CostItem) %>%
    group_by(Pathogen, AgeGroup, Disease, CostCategory) %>%
    summarise(median = median(value),
              `5%` = quantile(value, 0.05),
              `95%` = quantile(value, 0.95))

  list(Categorised = Categorised, Detailed = Detailed)
}

CostSummaries <- summariseCostList(CostList)
write.csv(CostSummaries$Detailed,'./R/CostTable.csv')
write.csv(CostSummaries$Categorised,'./R/CostTableCategories.csv')


