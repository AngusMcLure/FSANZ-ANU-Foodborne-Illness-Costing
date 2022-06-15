##### Calculate attributed cases and costs for food-borne pathogens
library(tidyverse)
source('RFiles/summaryFunctions.R')

## Load cost and epi estimates
CostList <- readRDS('CostList.rds')
DeathList <- readRDS('DeathList.rds')
HospList <- readRDS('HospList.rds')
IncidenceList <- readRDS('IncidenceList.rds')

## This kind of trimming doesn't quite work for Costs since some costs only have a single 0 element so selecting 1:100 produces many NAs
#trim <- 100
#CostList <- CostList %>% map_depth(4,~.x[1:trim])
#DeathList <- DeathList %>% map_depth(3,~.x[1:trim])
#HospList <- HospList %>% map_depth(3,~.x[1:trim])
#IncidenceList <- IncidenceList %>% map_depth(3,~.x[1:trim])

## Load attribution proportions
ndraws <- length(CostList[[1]][[1]][[1]][[1]])
loadAttrProps <- function(ndraws){
  out <- list(`Non-typhoidal Salmonella` = list(Beef = runif(ndraws)/10,
                                                Chicken = runif(ndraws)/10,
                                                Pork = runif(ndraws)/10),
              Norovirus = list(Beef = runif(ndraws)/10,
                               Chicken = runif(ndraws)/10,
                               Pork = runif(ndraws)/10))
  out %>% map(~t(as.matrix(as.data.frame(.x))))
}

AttrProps <- loadAttrProps(ndraws)


## Check for missing-ness of cost and/or attribution data and ensure that pathogens have the same order in both
CommonPathogens <- intersect(names(AttrProps), names(CostList))
MissingPathogens <- setdiff(names(CostList), names(AttrProps))
if(length(MissingPathogens)){
warning('Attribution data is not available for some pathogens for which cost data is available:\n', paste0(MissingPathogens,col = ',\n'))
}
ExtraPathogens <- setdiff(names(AttrProps),names(CostList))
if(length(ExtraPathogens)){
  warning('Cost data is not available for some pathogens for which attribution data is available:\n', paste0(ExtraPathogens,col = ',\n'))
}

CostList <- CostList[CommonPathogens]
AttrProps <- AttrProps[CommonPathogens]
IncidenceList <- IncidenceList[CommonPathogens]
HospList <- HospList[CommonPathogens]
DeathList <- DeathList[CommonPathogens]
EpiList <- list(Cases = IncidenceList, Hospitalisations = HospList, Deaths = DeathList)

##Attributed costs, cases, etc.
AttrCostList <- map2(CostList, AttrProps,
                     function(.c, .a){
                       map_depth(.c, 3, ~{as.list(as.data.frame(t(.x * .a)))})
                       }
                     )
AttrEpiList <- map(EpiList,~{
  map2(.x,AttrProps,
       function(.e,.a){
         map_depth(.e,2,
                   function(.n){as.list(as.data.frame(t(.n * .a)))}
                   )}
       )}
  )


#Tidy up attributed costs and epi figures ready for plotting and turning into figures

#rectangualrise the nested lists
AttrCostList <- AttrCostList %>% rectangle(names_to = c("Pathogen", "Disease",'Agegroup', "CostItem", "Source"), values_to = 'Cost')
AttrEpiList <- AttrEpiList %>% rectangle(names_to = c("Measure","Pathogen", "Disease",'Agegroup', "Source"), values_to = 'Count')

#collapse the detailed cost categories to the categories
DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')
AttrCostList <- AttrCostList %>%
  mutate(CostCategory  = case_when(CostItem %in% DirectCat ~ 'Direct',
                                   CostItem %in% WTPCat ~ 'WTP',
                                   TRUE ~ CostItem)) %>%
  group_by(across(-c(CostItem, Cost))) %>%
  summarise(Cost = list(laddn(Cost, .n = 0)))

#Make totals across agegroups, pathogens and diseases and then calculate quantiles
appendGroupTotals <- function(.d, .x){
  .d %>%
    rename_with(function(.cn){if_else(.cn == .x, '...x', .cn)}) %>%
    group_by(across(-c(...x, Agegroup))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Agegroup = 'All Ages')) %>%
    group_by(across(-c(...x, Disease))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Disease = 'Initial and sequel disease')) %>%
    group_by(across(-c(...x, Pathogen))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Pathogen = 'All pathogens')) %>%
    rename_with(function(.cn){if_else(.cn == '...x', .x, .cn)})
}
quantileListColumn <- function(.d, .x, probs = c(0.5,0.05,0.95)){
  .d %>%
    rename_with(function(.cn){if_else(.cn == .x, '...x', .cn)}) %>%
    mutate(out = map(...x,~quantile(.x, probs = c(0.05, 0.5, 0.95)))) %>%
    unnest_wider(out) %>%
    select(-...x)
}

AttrCostList <- AttrCostList %>% appendGroupTotals('Cost') %>% quantileListColumn('Cost') %>% rename(median = `50%`)
AttrEpiList <- AttrEpiList %>% appendGroupTotals('Count') %>% quantileListColumn('Count') %>% rename(median = `50%`)
