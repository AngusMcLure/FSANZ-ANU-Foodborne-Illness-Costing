##### Calculate attributed cases and costs for food-borne pathogens
library(tidyverse)
library(grid)
library(gridExtra)

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
                                                Chicken = runif(ndraws) * 0.8,
                                                Pork = runif(ndraws)/10,
                                                Eggs = runif(ndraws) * 0.8),
              Norovirus = list(Beef = runif(ndraws)/10,
                               Chicken = runif(ndraws)/10,
                               Pork = runif(ndraws)/10,
                               Eggs = runif(ndraws)/10),
              `Listeria monocytogenes` = list(Beef = runif(ndraws)/10,
                                              Chicken = runif(ndraws)/10,
                                              Pork = runif(ndraws)/10,
                                              Dairy = runif(ndraws)),
              STEC = list(Beef = runif(ndraws)/10,
                          Chicken = runif(ndraws)/10,
                          Pork = runif(ndraws)/10)) %>%
    map(~{.x$AllFood = rep(1,ndraws); .x})
  out %>% map(~t(as.matrix(as.data.frame(.x))))
}

AttrProps <- loadAttrProps(ndraws)

warning('Code currently does not return an error if the food categories are different for each pathogen. This might be a good thing if future surveys use lsightly different categories, however it could lead to fairly confusing tables and inconsistent sums over categories.')

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
rm(IncidenceList,HospList,DeathList)

##Attributed costs, cases, etc.
CostList <- map2(CostList, AttrProps,
                     function(.c, .a){
                       map_depth(.c, 3, ~{as.list(as.data.frame(t(.x * .a)))})
                       }
                     )
EpiList <- map(EpiList,~{
  map2(.x,AttrProps,
       function(.e,.a){
         map_depth(.e,2,
                   function(.n){as.list(as.data.frame(t(.n * .a)))}
                   )}
       )}
  )


#Tidy up attributed costs and epi figures ready for plotting and turning into figures

#rectangualrise the nested lists
CostList <- CostList %>% rectangle(names_to = c("Pathogen", "Disease",'Agegroup', "CostItem", "Source"), values_to = 'Cost')
EpiList <- EpiList %>% rectangle(names_to = c("Measure","Pathogen", "Disease",'Agegroup', "Source"), values_to = 'Count')

#collapse the detailed cost categories to the categories
DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')
CostList <- CostList %>%
  mutate(CostCategory  = case_when(CostItem %in% DirectCat ~ 'Direct',
                                   CostItem %in% WTPCat ~ 'WTP',
                                   TRUE ~ CostItem)) %>%
  group_by(across(-c(CostItem, Cost))) %>%
  summarise(Cost = list(laddn(Cost, .n = 0)))

#Make totals across agegroups, pathogens and diseases and then calculate quantiles
appendGroupTotals <- function(.d, .col){
  .d %>%
    rename_with(function(.cn){if_else(.cn == .col, '...x', .cn)}) %>%
    group_by(across(-c(...x, Agegroup))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Agegroup = 'All Ages')) %>%
    group_by(across(-c(...x, Disease))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Disease = 'Initial and sequel disease')) %>%
    group_by(across(-c(...x, Pathogen))) %>%
    bind_rows(.,summarise(.,...x = list(laddn(...x,.n=0))) %>% mutate(Pathogen = 'All pathogens')) %>%
    rename_with(function(.cn){if_else(.cn == '...x', .col, .cn)})
}
quantileListColumn <- function(.d, .col, probs = c(0.5,0.05,0.95)){
  .d$out <- map(.d[,.col,drop = T],~quantile(.x, probs = probs))
  .d %>%
    unnest_wider(out) %>%
    select(!all_of(.col))
}

CostList <- CostList %>% appendGroupTotals('Cost') %>% quantileListColumn('Cost') %>% rename(median = `50%`)
EpiList <- EpiList %>% appendGroupTotals('Count') %>% quantileListColumn('Count') %>% rename(median = `50%`)



## Make the tables for the report

#Epi tables
EpiList %>%
  subset(Pathogen == 'All pathogens'  & Disease == "Initial and sequel disease") %>%
  ungroup %>%
  group_by(Measure) %>%
  group_walk(~{ .x %>%
      rename(X5. = '5%', X95. = '95%') %>%
      medianCIformat(unit = 1) %>%
      select(Agegroup, Source, Count = Cost) %>%
      pivot_wider(names_from = Agegroup, values_from = Count) %>%
      mutate(Source = factor(Source) %>% fct_relevel('AllFood', after = Inf)) %>%
      arrange(Source) %>%
      write_excel_csv(paste('AttributionReport/EpiTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
  })

#Cost tables
CostList %>%
  subset(CostCategory == "TotalHumanCapital" & Disease == "Initial and sequel disease") %>%
  group_by(Pathogen) %>%
  group_walk(~{.x %>%
      rename(X5. = '5%', X95. = '95%') %>%
      medianCIformat() %>%
      select(Agegroup, Source, Cost) %>%
      pivot_wider(names_from = Agegroup, values_from = Cost) %>%
      mutate(Source = factor(Source) %>% fct_relevel('AllFood', after = Inf)) %>%
      arrange(Source) %>%
      write_excel_csv(paste('AttributionReport/CostTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
    })

P.CostProp <- CostList %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All Ages' &
           CostCategory == "TotalHumanCapital" &
           Pathogen != 'All pathogens') %>%
  mutate(SourceCat = if_else(Source == 'AllFood', 'All Food', "Individual Items"),
         Source = recode(Source, AllFood = 'All Food')) %>%
  ggplot(aes(x = Source,
             y = median/10^6,
             fill = Pathogen,
             label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  xlab('Food product') +
  ylab('Annual cost (millions AUD)') +
  coord_flip() +
  facet_wrap(vars(SourceCat), scales = 'free',nrow = 2)
P.CostProp
P.CostProp <- ggplotGrob(P.CostProp)
P.CostProp$heights[8] <- unit(1, "null")
P.CostProp$heights[13] <- unit(length(unique(CostList$Source))-1, "null")
grid.newpage(); grid.draw(P.CostProp)
P.CostProp <- arrangeGrob(P.CostProp)
ggsave(filename = 'AttributionReport/CostBySourcePathogen.png',P.CostProp)

P.CostPropAlt <- CostList %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All Ages' &
           CostCategory == "TotalHumanCapital" &
           Source != 'AllFood') %>%
  mutate(PathogenCat = if_else(Pathogen == 'All pathogens', 'All Pathogens', "Individual Pathogens")) %>%
  ggplot(aes(x = Pathogen,
             y = median/10^6,
             fill = Source,
             label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  xlab('Pathogen') +
  ylab('Annual cost (millions AUD)') +
  coord_flip() +
  facet_wrap(vars(PathogenCat), scales = 'free',nrow = 2)
P.CostPropAlt
P.CostPropAlt <- ggplotGrob(P.CostPropAlt)
P.CostPropAlt$heights[8] <- unit(1, "null")
P.CostPropAlt$heights[13] <- unit(length(unique(CostList$Source))-1, "null")
grid.newpage(); grid.draw(P.CostPropAlt)
P.CostPropAlt <- arrangeGrob(P.CostPropAlt)
ggsave(filename = 'AttributionReport/CostByPathogenSource.png',P.CostPropAlt)

