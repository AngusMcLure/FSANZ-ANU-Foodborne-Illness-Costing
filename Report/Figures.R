# Make figures for the report
source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")
library(tidyverse)
library(plotly)
#library(ggpattern) #for making patterned barcharts. Only available from github via remotes::install_github("coolbutuseless/ggpattern")
SequelaeNames <- names(SequelaeAssumptions)

CostSummaries.Detailed <- read.csv('./RFiles/CostTable.csv') %>%
  #subset(CostItem %in% c('GPSpecialist','ED','Deaths','WTPOngoing','HumanCapital','Medications','Tests'))
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and Specialist Vists',
                           ED = 'ED Visits',
                           Deaths = "Premature Mortality",
                           WTPOngoing = 'WTP-Ongoing',
                           HumanCapital = 'Human Capital'))

PathogenByCost <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease == 'All Diseases' &
           Pathogen != 'All gastro pathogens' &
           CostItem == 'TotalHumanCapital') %>%
  with(Pathogen[order(median)])

CostPerCase <- read.csv('./RFiles/CostPerCase.csv')


CostSummaries.Categorised <- read.csv('./RFiles/CostTableCategories.csv') %>%
  subset(!(CostItem %in% c('FrictionLow', 'FrictionHigh'))) %>%
  subset(!(Pathogen %in% c('All gastro pathogens'))) %>%
  subset(Disease != 'All Diseases') %>%
  mutate(SequelOrInitial = if_else(Disease %in% SequelaeNames, 'Sequel Disease(s)', 'Initial Disease')) %>%
  subset(AgeGroup == 'All Ages') %>%
  mutate(CostItem = recode(CostItem,
                               Deaths = "Premature Mortality",
                               HumanCapital = 'Human Capital'))

CostTotals <- CostSummaries.Categorised %>%
  mutate(Pathogen = Pathogen %>% factor(levels = PathogenByCost),
         SequelOrInitial = forcats::fct_rev(SequelOrInitial)) %>%
  group_by(CostItem, SequelOrInitial, Pathogen) %>%
  summarise(`Annual Cost (millions AUD)` = sum(median)/10^6)
P.TotalCost <-  CostTotals %>%
  ggplot(aes(x = Pathogen, y = `Annual Cost (millions AUD)`,
             fill = CostItem,
             alpha = SequelOrInitial,
             pattern = SequelOrInitial
             )) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  # geom_bar_pattern(stat = 'identity', position = 'stack',
  #                  pattern_fill = 'black',
  #                  pattern_alpha = 0.3,
  #                  pattern_angle = 45,
  #                  pattern_density = 0.1,
  #                  pattern_spacing = 0.025,
  #                  pattern_key_scale_factor = 0.6) +
  # scale_pattern_manual(values = c(Initial = "none", Sequel = "stripe")) +
  scale_alpha_manual(values = c(`Initial Disease` = 1, `Sequel Disease(s)` = 0.5)) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"),
                             title = 'Cost Category'),
         alpha = guide_legend(title = 'Sequel vs Initial')) +
  coord_flip()
P.TotalCost
ggsave(P.TotalCost,filename = 'Report/TotalCostBCatSequel.png')

P.CostPerPase <- CostPerCase %>%
  select(-c(X, X5., X95.)) %>%
  merge(CostTotals %>%
          group_by(Pathogen) %>%
          group_modify(~mutate(.x, CostFraction = `Annual Cost (millions AUD)`/sum(`Annual Cost (millions AUD)`))),
        by ='Pathogen') %>%
  mutate(CostPerCaseByCat = CostFraction * median/1000) %>%
  #subset(Pathogen != 'Listeria monocytogenes') %>%
  ggplot(aes(x = fct_reorder(Pathogen, median), y = CostPerCaseByCat, fill = CostItem)) +
  geom_bar(stat = 'identity', position = 'stack', color = 'black') +
  ylab('Cost per case (thousands AUD)') +
  xlab('Pathogen') +
  guides(fill = guide_legend(title = 'Cost Category')) +
  coord_flip()
P.CostPerPase
ggplotly(P.CostPerPase)
ggsave(P.CostPerPase,filename = 'Report/CostPerCase.png')


P.AllGastroByCat <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease == 'All Diseases' &
           Pathogen == 'All gastro pathogens' &
           !(CostItem %in% c('WTP-Ongoing',
                             'TotalHumanCapital',
                             'TotalFrictionHigh',
                             'TotalFrictionLow',
                             'FrictionHigh',
                             'FrictionLow'))) %>%
  ggplot(aes(x = fct_reorder(CostItem,median), y = median/10^6, label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  #geom_text() +
  xlab('Category') +
  ylab('Annual Cost (millions AUD)') +
  coord_flip()
P.AllGastroByCat
ggsave(P.AllGastroByCat, filename = 'Report/CostAllGastroByCat.png')


