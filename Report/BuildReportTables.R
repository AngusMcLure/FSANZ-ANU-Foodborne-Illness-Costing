load('AusFBDiseaseImage.RData')
library(tidyverse)
CostItems.Ordered <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications',
                       'HumanCapital','WTP','Deaths','WTPOngoing','TotalHumanCapital')

Diseases.Ordered <- c("Initial Disease", "IBS", "ReactiveArthritis", "GBS", 'HUS')


medianCIformat <- function(df){
  df %>%
    mutate(across(c(X5.,X95.,median),~format(signif(.x/10^3,3), #format to three significant figures
                                             big.mark = ",",
                                             scientific = FALSE,
                                             nsmall = 0,
                                             trim = T,
                                             drop0trailing = TRUE))) %>%
    mutate(across(c(X5.,X95.),           #remove intervals when they are the same as the point estimate
                  ~if_else(.x == median,
                           '',
                           .x))) %>%
    mutate(Cost = if_else(X5. == '',  #merge cost and and interval into a single line
                          median,
                          paste0(median, '\n', '(',X5.,' - ',X95.,')')))
}



read.csv("Outputs/CostTable.csv") %>%
  select(-X) %>%
  mutate(Disease = ifelse(Disease == unlist(map(PathogenAssumptions, ~.x$name))[Pathogen],
                          'Initial Disease',
                          Disease)) %>%
  #subset(Disease %in% Diseases.Ordered) %>%
  #mutate(Disease = factor(Disease, levels = Diseases.Ordered)) %>%
  group_by(Pathogen, Disease) %>%
  group_walk(~{
    .x %>%
      subset(CostItem %in% CostItems.Ordered) %>%
      medianCIformat %>%
      mutate(CostItem = factor(CostItem, levels = CostItems.Ordered)) %>%
      select(AgeGroup, CostItem, Cost) %>%
      pivot_wider(names_from = AgeGroup, values_from = Cost) %>%
      arrange(CostItem) %>%
      subset(`All Ages` != '0') %>%
      write_excel_csv(paste0('Report/',paste(paste(.y,collapse = '.'),'Table.csv',sep = '.')))
  })

PathogenSummaryTable <- read.csv("Outputs/CostTableCategories.csv") %>%
  subset(Disease == "Initial and Sequel Disease" &
           AgeGroup == 'All Ages' &
           CostItem %in% c("Direct", "HumanCapital","WTP","Deaths","TotalHumanCapital")) %>%
  medianCIformat %>%
  select(Pathogen, CostItem, Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  select(Pathogen,Direct, HumanCapital,WTP,Deaths,TotalHumanCapital) %>%
  as.data.frame
rownames(PathogenSummaryTable) <- PathogenSummaryTable$Pathogen
PathogenSummaryTable[c("All gastro pathogens","Campylobacter","Listeria monocytogenes",
                       "Non-typhoidal salmonella","Norovirus","Shigella","STEC",
                       "Escherichia coli (Non-STEC)","Salmonella Typhi",
                       "Toxoplasma gondii","Yersinia Enterocolitica"),] %>%
  write_excel_csv('Report/TotalCostByPathogenTable.csv')


DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')

CategorisedCosts <- CostList %>%
  map_depth(3,~{
    .x$Direct <- reduce(.x[DirectCat],`+`) #sum over direct costs
    .x$WTP <- reduce(.x[WTPCat],`+`) #sum of WTP and WTP-onging
    .x[c("Deaths","HumanCapital", "TotalHumanCapital","Direct","WTP")] #drop sub-categories
  })

# Cost of sequelae by category

CostSequelae <- imap(SequelaeAssumptions,
     function(.s,.sn){
       map(CategorisedCosts,~.x[[.sn]])
     }) %>% #keep only sequel diseases
  map(~{.x[!unlist(map(.x,is.null))]}) %>% #drop pathogens with no sequelae
  map_depth(2,~do.call(add,.x)) %>% #Sum over age groups
  map_depth(1,~do.call(add,.x)) %>% #Sum over diseases (initial and sequelae)
  quantilesNestedList(2,names_to = c("Disease", "CostItem")) %>%
  rename(X5. = `5%`, X95. = `95%`) %>%
  medianCIformat %>%
  select(Disease, CostItem, Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  as.data.frame %>%
  `rownames<-`(.$Disease) %>%
  `[`(c("GBS","HUS","IBS","ReactiveArthritis"),) %>%
  select(Disease,Direct, HumanCapital,WTP,Deaths,TotalHumanCapital)

write_excel_csv(CostSequelae, './Report/TotalCostSequelae.csv')
# Cost of gastro + sequelae by category

CostAllGastroIncludingSequelae <- CategorisedCosts %>%
  map(~.x[names(.x) %in% c("Gastroenteritis",names(SequelaeAssumptions))]) %>% #keep only 'All Gastroenteritis' and sequelae
  map_depth(2,~do.call(add,.x)) %>% #Sum over age groups
  map_depth(1,~do.call(add,unname(.x))) %>% #Sum over diseases (initial and sequelae)
  `[`(!unlist(map(.,~is.null(.x)))) %>% #drop pathogens without sequelae
  do.call(add,.) %>% #sum across pathogens
  quantilesNestedList(1,names_to = "CostItem") %>%
  rename(X5. = `5%`, X95. = `95%`) %>%
  medianCIformat %>%
  select(CostItem, Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  select(Direct, HumanCapital,WTP,Deaths,TotalHumanCapital)

write_excel_csv(CostAllGastroIncludingSequelae, './Report/TotalCostAllGastroIncludingSequelae.csv')


### Lost productivity sensitivity analysis
LostProductivityCosts <- imap(SequelaeAssumptions,
       function(.s,.sn){
         map(CostList,~.x[[.sn]])
       }) %>%
  map(~{.x[!unlist(map(.x,is.null))]}) %>% #drop pathogens with no sequelae
  map_depth(1,~do.call(add2,.x)) %>% #Sum over pathogens for each sequelae


map_depth(3,~{.x[c("HumanCapital","FrictionHigh","FrictionLow")]}) %>%

  map_depth(2,~do.call(add,.x)) %>% #Sum over age groups %>%

  map_depth(1,~do.call(add,unname(.x))) #Sum over diseases (initial and sequelae)
str(LostProductivityCosts)

#
# mutate(CostItem = recode(CostItem,
#                          GPSpecialist = 'GP and Specialist Vists',
#                          ED = 'ED Visits',
#                          Deaths = "Premature Mortality",
#                          WTPOngoing = 'WTP-Ongoing',
#                          FrictionLow = 'Friction-Low',
#                          FrictionHigh = 'Friction-High',
#                          HumanCapital = 'Human Capital',
#                          TotalFrictionHigh = 'Total.Friction-High',
#                          TotalFrictionLow = 'Total.Friction-Low',
#                          TotalHumanCapital = 'Total.Human Capital')) %>%
#
#
#
