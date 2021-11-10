library(tidyverse)
source("./R/Distributions.R")
source("./R/ClassDefinitions.R")
source("./Data/Diseases.R")
source("./R/loadData.R")
source("./R/estimationFunctions.R")

#Load all the data and assumptions
NNDSSIncidenceAgegroup <- getCasesAgeGroup()
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
WorkingDiseases <- c("Campylobacteriosis",
                     "Salmonellosis",
                     "Shigellosis",
                     'Toxoplasma',
                     'Listeriosis',
                     "Typhoid Fever",
                     "Gastroenteritis",
                     "Norovirus",
                     "Escherichia coli (Non-STEC)")


### CURRENTLY THE ESTIMATES COSTS FUNCTION PULLS ON INCIDENCE LIST AS A GLOBAL
### VARIABLE WITH NO REGARD FOR WHICH YEAR INCIDENCE LIST WAS CALCULATED FOR.
### I.E. YOU CAN CALCULATE INCIDENCE FOR YEAR 2019 THEN CALCULATE COSTS FOR YEAR
### 2020 WITHOUT GETTING AN ERROR. FIX BY MAKING INCIDENCE A LIST OF LISTS
### WITH AND ENTRY FOR EACH YEAR?
IncidenceList <- makeIncidenceList(2019,
                                   diseases = DiseaseAssumptions[WorkingDiseases],
                                   ndraws = ndraws,
                                   gastroRate = gastroRate)
SequelaeFractions <- calcSequelaeFractions(IncidenceList$Sequel)
HospList <- makeHospList(2019,
                         IncidenceList,
                         diseases = c(DiseaseAssumptions[WorkingDiseases],SequelaeAssumptions),
                         ndraws = ndraws)
DeathList <- makeDeathList(2019,
                           diseases = c(DiseaseAssumptions[WorkingDiseases],SequelaeAssumptions),
                           ndraws = ndraws)
CostList <- makeCostList(2019, DiseaseAssumptions[WorkingDiseases], ndraws, discount = 0) # no discounting and assuming a 5 year duration of ongoing illness is equivalent to the cross-sectional approach if we assume that case numbers were the same over the past five years.


warning('When summing across agegroups the draws of the multipliers used for each agegroup are considered independent. Making them dependent would require reworking the whole program, and is not necessarily a better assumption, but it is something to be aware of')

#Summarise draws with median, and 90 CIs
# Can probably reuse some code here...
IncidenceTable <- IncidenceList %>% as.data.frame(check.names = F) %>%
  mutate(Draw = row.names(.)) %>%
  pivot_longer(-Draw, names_sep = "\\.",
               names_to = c("Kind","Pathogen", "AgeGroup", "Disease")) %>%
  mutate(Disease = if_else(is.na(Disease), Pathogen, Disease)) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(AgeGroup = 'All Ages')) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, AgeGroup) %>%
              summarise(value = sum(value)) %>%
              mutate(Disease = 'All Diseases')) %>%
  bind_rows(.,
            group_by(.,Draw, AgeGroup, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(Pathogen = 'All Pathogens')) %>%
  group_by(Kind,Pathogen, AgeGroup, Disease) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))

HospTable <- HospList %>%
  data.frame(check.names = F) %>%
  mutate(Draw = row.names(.)) %>%
  pivot_longer(-Draw, names_sep = "\\.",
               names_to = c("Kind","Pathogen", "AgeGroup", "Disease")) %>%
  subset(!is.na(value)) %>%
  mutate(Disease = if_else(is.na(Disease), Pathogen, Disease)) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(AgeGroup = 'All Ages')) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, AgeGroup) %>%
              summarise(value = sum(value)) %>%
              mutate(Disease = 'All Diseases')) %>%
  bind_rows(.,
            group_by(.,Draw, AgeGroup, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(Pathogen = 'All Pathogens')) %>%
  group_by(Pathogen, AgeGroup, Disease) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))

DeathTable <- DeathList %>%
  as.data.frame(check.names = F) %>%
  mutate(Draw = row.names(.)) %>%
  pivot_longer(-Draw, names_sep = "\\.",
               names_to = c("Disease", "AgeGroup", "Pathogen")) %>%
  subset(!is.na(value)) %>%
  mutate(Pathogen = if_else(is.na(Pathogen), Disease, Pathogen)) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(AgeGroup = 'All Ages')) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, AgeGroup) %>%
              summarise(value = sum(value)) %>%
              mutate(Disease = 'All Diseases')) %>%
  bind_rows(.,
            group_by(.,Draw, AgeGroup, Disease) %>%
              summarise(value = sum(value)) %>%
              mutate(Pathogen = 'All Pathogens')) %>%
  group_by(Pathogen, AgeGroup, Disease) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))
EpiTable <- bind_rows(Deaths = DeathTable, Hospitalisations = HospTable, Cases = IncidenceTable,.id = 'Measure') %>%
  subset(!(Disease %in% names(DiseaseAssumptions) & Pathogen == 'All Pathogens'))

write.csv(EpiTable,'./R/EpiTable.csv')

CostListTotals <- CostList %>%
  as.data.frame(check.names = F) %>%
  mutate(Draw = row.names(.)) %>%
  pivot_longer(-Draw, names_sep = "\\.",
               names_to = c("Pathogen", "AgeGroup", "Disease","CostItem")) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, Disease,CostItem) %>%
              summarise(value = sum(value)) %>%
              mutate(AgeGroup = 'All Ages')) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, AgeGroup,CostItem) %>%
              summarise(value = sum(value)) %>%
              mutate(Disease = 'All Diseases')) %>%
  bind_rows(.,
            group_by(.,Draw, Disease, AgeGroup,CostItem) %>%
              summarise(value = sum(value)) %>%
              mutate(Pathogen = 'All Pathogens')) %>%
  bind_rows(.,
            group_by(.,Draw, Pathogen, AgeGroup, Disease) %>%
              summarise(Total.HumanCapital = sum(value[!(CostItem %in% c("FrictionHigh","FrictionLow"))]),
                        Total.FrictionHigh = sum(value[!(CostItem %in% c("HumanCapital","FrictionLow"))]),
                        Total.FrictionLow  = sum(value[!(CostItem %in% c("HumanCapital","FrictionHigh"))])
              ) %>%
              pivot_longer(Total.HumanCapital:Total.FrictionLow, names_to = "CostItem")
  )


CostTable <-  CostListTotals %>%
  group_by(Pathogen, AgeGroup, Disease, CostItem) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))

write.csv(CostTable,'./R/CostTable.csv')

DirectCat <- c('GP','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')
LostProdCat <- c("HumanCapital","FrictionHigh", "FrictionLow")

CostTableCategories <- bind_rows(CostListTotals %>% subset(CostItem %in% c("Deaths",LostProdCat, paste0("Total.",LostProdCat))),
            group_by(CostListTotals,Draw,Pathogen, AgeGroup, Disease) %>%
              summarise(Direct = sum(value[CostItem %in% DirectCat]),
                        WTP = sum(value[CostItem %in% WTPCat])) %>%
              pivot_longer(Direct:WTP,names_to = 'CostItem', values_to = 'value')) %>%
  rename(CostCategory = CostItem) %>%
  group_by(Pathogen, AgeGroup, Disease, CostCategory) %>%
  summarise(median = median(value),
            `5%` = quantile(value, 0.05),
            `95%` = quantile(value, 0.95))

write.csv(CostTableCategories,'./R/CostTableCategories.csv')


