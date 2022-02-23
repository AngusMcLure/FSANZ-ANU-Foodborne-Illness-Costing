# Make figures for the report
source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")
library(tidyverse)
library(plotly)
SequelaeNames <- names(SequelaeAssumptions)

CostSummaries.Detailed <- read.csv('./Outputs/CostTable.csv') %>%
  #subset(CostItem %in% c('GPSpecialist','ED','Deaths','WTPOngoing','HumanCapital','Medications','Tests'))
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and Specialist Vists',
                           ED = 'ED Visits',
                           Deaths = "Premature Mortality",
                           WTPOngoing = 'WTP-Ongoing',
                           HumanCapital = 'Human Capital'))

ItemByCost <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease == 'Initial and Sequel Disease' &
           Pathogen == 'All gastro pathogens' &
           CostItem == 'TotalHumanCapital') %>%
  with(Pathogen[order(median)])

CostPerCase <- read.csv('./Outputs/CostPerCase.csv')


CostSummaries.Categorised <- read.csv('./Outputs/CostTableCategories.csv') %>%
  subset(!(CostItem %in% c('FrictionLow', 'FrictionHigh','TotalFrictionHigh','TotalFrictionLow','TotalHumanCapital'))) %>%
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




ItemByCost.AllGastro <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease == 'Initial and Sequel Disease' &
           Pathogen == 'All gastro pathogens' &
           !(CostItem %in% c('WTP-Ongoing',
                             'TotalHumanCapital',
                             'TotalFrictionHigh',
                             'TotalFrictionLow',
                             'FrictionHigh',
                             'FrictionLow'))) %>%
  with(CostItem[order(median)])

P.AllGastroByCat <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease != 'Initial and Sequel Disease' &
           Pathogen == 'All gastro pathogens' &
           !(CostItem %in% c('WTP-Ongoing',
                             'TotalHumanCapital',
                             'TotalFrictionHigh',
                             'TotalFrictionLow',
                             'FrictionHigh',
                             'FrictionLow'))) %>%
 mutate(CostItem = factor(CostItem,levels =ItemByCost.AllGastro),
        SequelOrInitial = if_else(Disease %in% SequelaeNames,
                                  'Sequel Diseases',
                                  'Initial Disease')) %>%
  ggplot(aes(x = CostItem,
             y = median/10^6,
             label = round(median/10^6),
             alpha = fct_rev(SequelOrInitial))) +
  geom_bar(stat = 'identity') +
  #geom_text() +
  xlab('Category') +
  ylab('Annual Cost (millions AUD)') +
  coord_flip() +
  scale_alpha_manual(values = c(`Initial Diseases` = 1, `Sequel Diseases` = 0.5)) +
  guides(alpha = guide_legend(title = 'Sequel vs Initial'))
P.AllGastroByCat
ggsave(P.AllGastroByCat, filename = 'Report/CostAllGastroByCat.png')


