##### Calculate attributed cases and costs for food-borne pathogens
library(tidyverse)
library(grid)
library(gridExtra)
library(pluralize)
library(stringr)
library(glue)

source('RFiles/summaryFunctions.R')
source('RFiles/Distributions.R')
source('RFiles/ClassDefinitions.R')
source('RFiles/Diseases.R')

## Load cost and epi estimates
CostList <- readRDS('CostList.rds')
DeathList <- readRDS('DeathList.rds')
HospList <- readRDS('HospList.rds')
IncidenceList <- readRDS('IncidenceList.rds')

## Reduce size of files by dropping most draws from the distributions. Since some costs only have a single 0 element simply selecting 1:trim produces many NAs
trim <- 10000
CostList <- CostList %>% map_depth(4,~.x[1:min(trim,length(.x))])
DeathList <- DeathList %>% map_depth(3,~.x[1:min(trim,length(.x))])
HospList <- HospList %>% map_depth(3,~.x[1:min(trim,length(.x))])
IncidenceList <- IncidenceList %>% map_depth(3,~.x[1:min(trim,length(.x))])

## Load attribution proportions
ndraws <- length(CostList[[1]][[1]][[1]][[1]])
loadAttrProps <- function(ndraws){
  Attr <- readxl::read_excel('Data/AttributionProportions.xlsx',sheet = 'Gamma parameters') %>%
    mutate(across(c(shape, rate), .f  = ~as.numeric(na_if(.x, "NA")))) %>%
    mutate(Commodity = gsub('.', ' ', Commodity, fixed = TRUE)) %>% #tidying up commodity names to remove any periods
    drop_na

  Attr <- Attr %>% group_by(Pathogen, Commodity) %>%
    group_map_named(~{#print(1 - pgamma(1, shape = .x$shape, rate  = .x$rate))
      rgamma(ndraws, shape = .x$shape, rate  = .x$rate)}) %>%
    map(~{.x$AllFood = rep(1,ndraws); .x}) %>%
    map(~as.matrix(as.data.frame(.x,check.names = FALSE)))
  Attr
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
                   map_depth(.c, 3, ~{as.list(as.data.frame(.x * .a))})
                 }
)
EpiList <- map(EpiList,~{
  map2(.x,AttrProps,
       function(.e,.a){
         map_depth(.e,2,
                   function(.n){as.list(as.data.frame(.n * .a))}
         )}
  )}
)


####Tidy up attributed costs and epi figures ready for plotting and turning into figures

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
  summarise(Cost = list(reduce(Cost, `+`)))

#change specific initial illness names all to 'initial'
CostList <- CostList %>% mutate(Disease = ifelse(Disease %in% names(SequelaeAssumptions), Disease, 'Initial'))
EpiList <- EpiList %>% mutate(Disease = ifelse(Disease %in% names(SequelaeAssumptions), Disease, 'Initial'))

#Make totals across agegroups, pathogens and diseases and then calculate quantiles
group_cols <- list(Pathogen = 'All pathogens', Agegroup = 'All ages', `Disease` = 'Initial and sequel disease')
CostList <- CostList %>% appendGroupTotals('Cost', group_cols) %>% quantileListColumn('Cost') %>% rename(median = `50%`)
EpiList <- EpiList %>% appendGroupTotals('Count',group_cols) %>% quantileListColumn('Count') %>% rename(median = `50%`)

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

#Order Food by cost
FoodCatOrdered <- CostList %>% subset(Pathogen == 'All pathogens' &
                                           CostCategory == 'TotalHumanCapital' &
                                           Agegroup == 'All ages' &
                                           Disease == "Initial and sequel disease") %>%
  arrange(median) %>% `[`(,'Source', drop = TRUE)

#Combine the summary data for cost and epi
CombinedSummaries <- EpiList %>% ungroup %>%
  bind_rows(CostList %>% ungroup %>%
              subset(CostCategory == 'TotalHumanCapital') %>%
              select(-CostCategory) %>%
              mutate(Measure = 'Cost')
  ) %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All ages') %>%
  mutate(SourceCat = if_else(Source == 'AllFood', 'All Food', "Individual Items"),
         #Source = factor(Source,FoodCatOrderedCases),
         Source = factor(Source,FoodCatOrdered),
         Source = recode(Source, AllFood = 'All Food'),
         Source = fct_relevel(Source, 'Other', after = 0)
  )

P.CostProp <- CombinedSummaries %>%
  subset(Measure == "Cost" &
           Pathogen != 'All pathogens') %>%
  ggplot(aes(x = Source,
             y = median * 10^-6,
             fill = Pathogen,
             label = round(median * 10^-6))) +
  geom_bar(stat = 'identity') +
  xlab('Food product') +
  ylab('Annual cost (millions AUD)') +
  coord_flip() +
  ggh4x::facet_grid2("SourceCat", scales = 'free',space = 'free_y', independent = 'x',switch = 'both') +
  theme(legend.position = c(0.70, 0.25),
        strip.text.y = element_blank())
P.CostProp
ggsave(filename = 'AttributionReport/CostBySourcePathogen.png',P.CostProp)

### Figure for proportion of cases and deaths by food and pathogen

P.EpiProp <- CombinedSummaries %>%
  subset(Pathogen != 'All pathogens')
  mutate(median = if_else(Measure == 'Cases', median/1000,
                          if_else(Measure == "Costs", median/1000000, median))) %>%
  mutate(Measure = recode(Measure,
                          Cases = 'Cases (thousands)',
                          Cost = 'Cost (millions AUD)')) %>%
  mutate(Measure = factor(Measure, levels = c('Cases (thousands)', 'Hospitalisations', "Deaths",
                                              'Cost (millions AUD)'))) %>%
  ggplot(aes(x = Source,
             y = median,
             fill = Pathogen,
             label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  xlab('Food product') +
  ylab('') +
  coord_flip() +
  ggh4x::facet_grid2(SourceCat~Measure, scales = 'free',space = 'free_y', independent = 'x',switch = 'both') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
    #legend.position = c(0.9,0.125),
        strip.placement = 'outside',
        strip.text.y = element_blank(),
        strip.background.x = element_rect('white'))
P.EpiProp
ggsave(filename = 'AttributionReport/EpiBySourcePathogen.png',P.EpiProp, width = 11)


P.CostPropAlt <- CostList %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All ages' &
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
P.CostPropAlt$heights[13] <- unit(length(unique(CostList$Pathogen))-1, "null")
grid.newpage(); grid.draw(P.CostPropAlt)
P.CostPropAlt <- arrangeGrob(P.CostPropAlt)
ggsave(filename = 'AttributionReport/CostByPathogenSource.png',P.CostPropAlt)


## Calculate cost per case by food product

CombinedSummaries %>% select(-c(`5%`,`95%`)) %>%
  pivot_wider(names_from = Measure, values_from = median) %>%
  mutate(CostPerCase = Cost/Cases) %>%
  subset(Pathogen == "All pathogens")

## Generate summary sentences for each pathogen and for the top sources

textlist <- function(cl, conj = 'and', oxford = TRUE){
  n <- length(cl)
  if(n == 0) return(character(0))
  else if(n == 1) return(as.character(cl))
  else if(n == 2) return(paste(cl[1], conj, cl[2]))
  else return(paste0(paste(cl[1:(n-1)], collapse = ', '), if(oxford){','}else{''},' ', conj, ' ', cl[n]))
}

textCountObj <- function(n, name, plural = NULL){
  plural <- pluralize(name)
  if(n == 1){return(paste(n, name))}
  else{return(paste(n, plural))}
}

matchVerbNoun <- function(verb, noun){
  if(is_plural(noun)){
    return(pluralize(verb))
  }else{
      return(singularize(verb))
    }
}

attribtionSentencesPathogen <- function(pathogen){
  cl <- CostList %>% subset(Pathogen == pathogen &
                              Agegroup == 'All ages' &
                              CostCategory == "TotalHumanCapital")
  el <- EpiList %>% subset(Pathogen == pathogen &
                             Agegroup == 'All ages')

  InitDisease <- PathogenAssumptions[[pathogen]]$name
  SequelDiseases <- names(PathogenAssumptions[[pathogen]]$sequelae)

  TotalCost <- subset(cl, Source == 'AllFood' &
                        Disease == 'Initial and sequel disease')$median #not tidied until later as it is needed for calculation fractions
  CasesInit <- subset(el, Source == 'AllFood' &
                        Measure == 'Cases' &
                        Disease == 'Initial')$median %>% tidyNumber(unit = 1)
  CasesSequel <- subset(el, Source == 'AllFood' &
                          Measure == 'Cases' &
                          Disease %in% SequelDiseases)$median %>% sum %>% tidyNumber(unit = 1)
  Hosp <- subset(el, Source == 'AllFood' &
                   Measure == 'Hospitalisations' &
                   Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1)
  Deaths <- subset(el, Source == 'AllFood' &
                     Measure == 'Deaths' &
                     Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1)
  SourceCosts <-  subset(cl, Disease == 'Initial and sequel disease' & Source != 'AllFood') %>% arrange(desc(median))
  SourcesOrdered <- SourceCosts$Source
  SourceCostsOrdered <- SourceCosts$median
  SourcePropOrdered <- round(SourceCostsOrdered/TotalCost* 100)
  SourceCostsOrdered <- SourceCostsOrdered %>% tidyNumber(unit = 10^6, round = FALSE)
  TotalCost <- TotalCost %>% tidyNumber(unit = 10^6, round = FALSE)


  LeadingSourceCases <- subset(el, Source == SourcesOrdered[1] &
                                 Measure == 'Cases' &
                                 Disease == 'Initial')$median %>% tidyNumber(unit = 1)
  LeadingSourceCasesSequel <- subset(el, Source == SourcesOrdered[1] &
                                       Measure == 'Cases' &
                                       Disease %in% SequelDiseases)$median %>% sum %>% tidyNumber(unit = 1)
  LeadingSourceHosp <- subset(el, Source == SourcesOrdered[1] &
                                Measure == 'Hospitalisations' &
                                Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1)
  LeadingSourceDeaths <- subset(el, Source == SourcesOrdered[1] &
                                  Measure == 'Deaths' &
                                  Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1)
  hasSequel <- length(SequelDiseases) > 0
  str_glue(
    '{pathogen} resulted in an annual cost of approximately {TotalCost} million
    AUD circa 2019 arising from {CasesInit} cases of',
    if(hasSequel){' initial '}else{' '},'illness, ',
    if(hasSequel){'{CasesSequel} cases of sequel illness ({textlist(tolower(SequelDiseases),conj = "or")}), '}else{''},
    '{Hosp} hospitalisations, and {textCountObj(Deaths, "death")}. {SourcesOrdered[1]}
    {matchVerbNoun("was",SourcesOrdered[1])} the leading source ({SourcePropOrdered[1]}%)
    with a total annual cost of {SourceCostsOrdered[1]} million AUD arising from
    {LeadingSourceCases} cases of', if(hasSequel){' initial '}else{' '},'illness, ',
    if(hasSequel){'{LeadingSourceCasesSequel} cases of sequel illness, '}else{''},
    '{LeadingSourceHosp} hospitalisations, and {LeadingSourceDeaths} deaths. The
    next three most frequent sources were {tolower(SourcesOrdered[2])}
    ({SourcePropOrdered[2]}%), {tolower(SourcesOrdered[3])} ({SourcePropOrdered[3]}%),
    and {tolower(SourcesOrdered[4])} ({SourcePropOrdered[4]}%).'
  ) %>%
    gsub('\n',' ',., fixed = TRUE)
}

attributionSentencesSource <- function(){
  cl <- CostList %>% subset(Agegroup == 'All ages' &
                              CostCategory == "TotalHumanCapital")
  el <- EpiList %>% subset(Measure == 'Cases' &
                             Agegroup == 'All ages'&
                             Disease == 'Initial')

  SourceCosts <-  subset(cl, Disease == 'Initial and sequel disease' &
                           Pathogen == 'All pathogens' &
                           Source != 'AllFood') %>% arrange(desc(median))
  SourcesCostOrdered <- SourceCosts$Source
  SourcesCostOrderedCosts <- SourceCosts$median %>% tidyNumber(unit = 10^6, round = FALSE)

  LeadingSourceCosts <- subset(cl, Disease == 'Initial and sequel disease' &
                                 Pathogen != 'All pathogens' &
                                 Source == SourcesCostOrdered[1]) %>% arrange(desc(median))
  PathogensCostOrdered <- LeadingSourceCosts$Pathogen
  PathogensCostOrderedCosts <- LeadingSourceCosts$median %>% tidyNumber(unit = 10^6, round = FALSE)

  ## Sources by number of
  SourceCases <-  subset(el, Pathogen == 'All pathogens' &
                           Source != 'AllFood') %>% arrange(desc(median))
  SourcesCaseOrdered <- SourceCases$Source
  SourcesCaseOrderedCases <- SourceCases$median %>% tidyNumber(unit = 1, round = TRUE)

  LeadingSourceCases <- subset(el, Pathogen != 'All pathogens' &
                                 Source == SourcesCaseOrdered[1]) %>% arrange(desc(median))
  PathogensCaseOrdered <- LeadingSourceCases$Pathogen
  PathogensCaseOrderedCases <- LeadingSourceCases$median %>% tidyNumber(unit = 1, round = TRUE)

  str_glue(
    'Among the pathogens and food categories considered, {tolower(SourcesCostOrdered[1])}
    {matchVerbNoun("was",SourcesCostOrdered[1])} associated with the greatest cost
    of foodborne illness, with a total cost of {SourcesCostOrderedCosts[1]} million AUD.
    Of this total, ',
    textlist(glue('{PathogensCostOrderedCosts} million AUD was due to {PathogensCostOrdered}')),
    '. The food category associated with the most cases of foodborne illness circa 2019
    was', if(SourcesCaseOrdered[1] == SourcesCostOrdered[1]){' also '}else{' '},
    '{tolower(SourcesCaseOrdered[1])}, with a total burden of {SourcesCaseOrderedCases[1]} cases. Of this total, ',
    textlist(glue('{PathogensCaseOrderedCases} cases were due to {PathogensCaseOrdered}')),'.'
  )%>%
    gsub('\n',' ',., fixed = TRUE)
}

attributionSentencesSource() %>% write_lines('AttributionReport/PathogenAttributionSummariesText.txt',
                                             sep = '\n\n')

CommonPathogens %>%
  map(attribtionSentencesPathogen) %>%
  write_lines('AttributionReport/PathogenAttributionSummariesText.txt',
              sep = '\n\n', append = TRUE)



