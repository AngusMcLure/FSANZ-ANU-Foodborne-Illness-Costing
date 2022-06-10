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
warning('Attribution data is not available for some pathogens for which cost data is available: ', paste0(MissingPathogens,col = ', '))
}
ExtraPathogens <- setdiff(names(AttrProps),names(CostList))
if(length(ExtraPathogens)){
  warning('Cost data is not available for some pathogens for which attribution data is available: ', paste0(ExtraPathogens,col = ', '))
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

AttrCostList <- AttrCostList %>% rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
  lapply(eval) %>%
  as.data.frame(check.names = F) %>%
  summarise(across(.fns = list)) %>% #make the draws list columns so they take up less space with we lengthen the dataframe
  pivot_longer(everything(), #lengthen dataframe
              names_sep = "\\.",
              names_to = c("Pathogen", "Disease",'Agegroup', "CostItem", "Source"),
              values_to = 'Cost') %>%
  group_by(Pathogen, Disease, CostItem, Source) %>%
  bind_rows(.,summarise(.,Cost = list(laddn(Cost,.n=0))) %>% mutate(Agegroup = 'All Ages')) %>%
  group_by(Pathogen, Agegroup, CostItem, Source) %>%
  bind_rows(.,summarise(.,Cost = list(laddn(Cost,.n=0))) %>% mutate(Disease = 'Initial and sequel disease')) %>%
  group_by(Disease, Agegroup, CostItem, Source) %>%
  bind_rows(.,summarise(.,Cost = list(laddn(Cost,.n=0))) %>% mutate(Pathogen = 'All pathogens'))

temp <- AttrCostList %>%
  map_depth(2,~{.x$`All ages` <- do.call(add2,unname(.x));.x}) %>%
  map(~{.x$`Initial and sequel disease` <- do.call(addn,c(unname(.x),.n = 3));.x})
temp %>% quantilesNestedList(5, c("Pathogen", "Disease","AgeGroup","CostItem","Source"))

