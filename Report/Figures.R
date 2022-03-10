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
                           GPSpecialist = 'GP and specialist vists',
                           ED = 'Emergency department visits',
                           Deaths = "Premature mortality",
                           WTP = "Pain and suffering",
                           HumanCapital = 'Non-fatal productivity losses'))

CostPerCase <- read.csv('./Outputs/CostPerCase.csv')


CostSummaries.Categorised <- read.csv('./Outputs/CostTableCategories.csv') %>%
  subset(!(CostItem %in% c('FrictionLow', 'FrictionHigh','TotalFrictionHigh','TotalFrictionLow','TotalHumanCapital'))) %>%
  subset(!(Pathogen %in% c('All gastro pathogens', 'All pathogens'))) %>%
  subset(Disease != 'All Diseases') %>%
  mutate(SequelOrInitial = if_else(Disease %in% SequelaeNames, 'Sequel disease(s)', 'Initial disease')) %>%
  subset(AgeGroup == 'All ages') %>%
  mutate(CostItem = recode(CostItem,
                               Deaths = "Premature Mortality",
                               HumanCapital = 'Non-fatal productivity losses'))

PathogenByCost <- CostSummaries.Detailed %>%
  subset(!(Pathogen %in% c("All pathogens", "All gastro pathogens")) &
           Disease == 'Initial and sequel disease' &
           AgeGroup == 'All ages' &
           CostItem == "TotalHumanCapital") %>%
  with(Pathogen[order(median)])


CostTotals <- CostSummaries.Categorised %>%
  mutate(Pathogen = Pathogen %>% factor(levels = PathogenByCost),
         SequelOrInitial = forcats::fct_rev(SequelOrInitial)) %>%
  group_by(CostItem, SequelOrInitial, Pathogen) %>%
  summarise(`Annual cost (millions AUD)` = sum(median)/10^6)
P.TotalCost <-  CostTotals %>%
  ggplot(aes(x = Pathogen, y = `Annual cost (millions AUD)`,
             fill = CostItem,
             alpha = SequelOrInitial,
             pattern = SequelOrInitial
             )) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"),
                             title = 'Cost category'),
         alpha = guide_legend(title = 'Sequel vs initial')) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.25))
P.TotalCost
ggsave(P.TotalCost,filename = 'Report/TotalCostBCatSequel.png')

P.CostPerPase <- CostPerCase %>%
  select(-c(X, X5., X95.)) %>%
  merge(CostTotals %>%
          group_by(Pathogen) %>%
          group_modify(~mutate(.x, CostFraction = `Annual cost (millions AUD)`/sum(`Annual cost (millions AUD)`))),
        by ='Pathogen') %>%
  mutate(CostPerCaseByCat = CostFraction * median/1000) %>%
  ggplot(aes(x = fct_reorder(Pathogen, median), y = CostPerCaseByCat, fill = CostItem)) +
  geom_bar(stat = 'identity', position = 'stack', color = 'black') +
  ylab('Cost per case (thousands AUD)') +
  xlab('Pathogen') +
  guides(fill = guide_legend(title = 'Cost category')) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15))
P.CostPerPase
ggplotly(P.CostPerPase)
ggsave(P.CostPerPase,filename = 'Report/CostPerCase.png')



CostAllGastro <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All ages' &
           Pathogen == 'All gastro pathogens' &
           !(CostItem %in% c('TotalHumanCapital',
                             'TotalFrictionHigh',
                             'TotalFrictionLow',
                             'FrictionHigh',
                             'FrictionLow',
                             'WTPOngoing')))

ItemByCost.AllGastro <- CostAllGastro %>%
  subset(Disease == 'Initial and sequel disease') %>%
  with(CostItem[order(median)])

P.AllGastroByCat <- CostAllGastro %>%
 mutate(CostItem = factor(CostItem,levels =ItemByCost.AllGastro),
        SequelOrInitial = if_else(Disease %in% SequelaeNames,
                                  'Sequel diseases',
                                  'Initial disease')) %>%
  ggplot(aes(x = CostItem,
             y = median/10^6,
             label = round(median/10^6),
             alpha = fct_rev(SequelOrInitial))) +
  geom_bar(stat = 'identity') +
  #geom_text() +
  xlab('Category') +
  ylab('Annual cost (millions AUD)') +
  coord_flip() +
  scale_alpha_manual(values = c(`Initial diseases` = 1, `Sequel diseases` = 0.5)) +
  guides(alpha = guide_legend(title = 'Sequel vs Initial')) +
  theme(legend.position = c(0.75, 0.15))

P.AllGastroByCat
ggsave(P.AllGastroByCat, filename = 'Report/CostAllGastroByCat.png')


