library(tidyverse)

source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")
source("./RFiles/loadData.R")
source('./RFiles/summaryFunctions.R')
source("./RFiles/estimationFunctions.R")


#Load all the data and assumptions
NNDSSIncidenceAgegroup <- getCasesNNDSSAgeGroup() %>% subset(Disease != "STEC")
StateIncidenceAgeGroup <- getCasesStateAgeGroup()
NotificationsAgeGroup <- bind_rows(NNDSSIncidenceAgegroup,StateIncidenceAgeGroup)
AusPopAgegroup <- getAusPopAgeGroup()
AusPopSingleYear <- getAusPopSingleYearAge()
Hospitalisations <- getHospitalisationsAgeGroup()
Costs <- getCosts()
VSL <- getValueStatisticalLife()
Deaths <- getABSDeaths() %>% subset(Method == "Underlying")
MissedDaysGastro <- getMissedDaysGastro()
FrictionRates <- getFrictionRates()
Workforce <- getWorkforceAssumptions() ##What year is this for and does it need to change by year?


# Draw from all distributions
ndraws <- 10^5
set.seed(20220222) #Date at time of final run
Year <- 2019

WTPList <- getWTP(ndraws)
FBMult <- list() #setup container to hold FoodBorne multiplier draws. Actual drawing happens during the next line (make Incidence List). Though not ideal, I've done it this way to not change the order in which random variables are drawn so the outputs are identical.

IncidenceList <- makeIncidenceList(Year,
                                   pathogens = PathogenAssumptions,
                                   ndraws = ndraws,
                                   gastroRate = gastroRate)

SequelaeFractions <- calcSequelaeFractions(IncidenceList)
HospList <- makeHospList(Year,
                         IncidenceList,
                         pathogens = PathogenAssumptions,
                         ndraws = ndraws)
DeathList <- makeDeathList(Year,
                           pathogens = PathogenAssumptions,
                           ndraws = ndraws)
CostList <- makeCostList(Year, PathogenAssumptions, ndraws, discount = 0) # no discounting and assuming a 5 year duration of ongoing illness is equivalent to the cross-sectional approach if we assume that case numbers were the same over the past five years.

### Inluce sequelae as part of 'All gastro'

appendSequelaeToAllGastro <- function(List, .f){
  List$`All gastro pathogens` <- c(List$`All gastro pathogens`,
                                   imap(SequelaeAssumptions, #re-nest list with sequelae above pathogens and dropping initial diseases
                                        function(.s,.sn){
                                          map(List,~.x[[.sn]])
                                        }) %>%
                                     map(~{.x[!unlist(map(.x,is.null))]}) %>% #drop pathogens with no sequelae
                                     map(~do.call(.f,unname(.x)))) #sum over pathogens by sequelae
  List
}
CostList <- appendSequelaeToAllGastro(CostList,add2)
HospList <- appendSequelaeToAllGastro(HospList,add)
DeathList <- appendSequelaeToAllGastro(DeathList,add)
IncidenceList <- appendSequelaeToAllGastro(IncidenceList,add)

### Create a new category called all pathogens (which includes all pathogens, not just those that cause gastro)
appendAllPathogens <- function(List, .f){
  List$`All pathogens`<- c(list(Initial = .f(List$`All gastro pathogens`$Gastroenteritis,
                                             List$`Salmonella Typhi`$`Typhoid Fever`,
                                             List$`Toxoplasma gondii`$Toxoplasmosis,
                                             List$`Listeria monocytogenes`$Listeriosis)),
                           List$`All gastro pathogens`[names(List$`All gastro pathogens`) != 'Gastroenteritis']
  )
  List
}

CostList <- appendAllPathogens(CostList,add2)
HospList <- appendAllPathogens(HospList,add)
DeathList <- appendAllPathogens(DeathList,add)
IncidenceList <- appendAllPathogens(IncidenceList,add)



warning('When summing across agegroups the draws of the multipliers used for each agegroup are considered independent. Making them dependent would require reworking the whole program, and is not necessarily a better assumption, but it is something to be aware of')

#Include `All ages` and `All diseases` sums, calculate median, and 90 CIs, then reformat as data.frame
summariseEpiList <- function(list){
  list %>%
    map_depth(2,~{.x$`All ages` <- reduce(.x,`+`);.x}) %>%
    map(~{.x$`Initial and sequel disease` <- do.call(add,unname(.x));.x}) %>%
    quantilesNestedList(3, c("Pathogen", "Disease","AgeGroup"))
}

# Include `All ages` and `All diseases` sums, calculate median, and 90 CIs, then
# reformat as data.frame. Outputs are returned in categorised format (Deaths,
# Human Captial, Direct, WTP) or detailed (Tests, Medications, Hospitalisation...)
summariseCostList <- function(list){
  #Add totals for `All ages` and `All diseases` (aka initial and sequel diseases)
  totals <- list %>%
    map_depth(2,~{.x$`All ages` <- do.call(add,unname(.x));.x}) %>%
    map(~{.x$`Initial and sequel disease` <- do.call(add2,unname(.x));.x})
  Detailed <- totals %>% quantilesNestedList(4, c("Pathogen", "Disease","AgeGroup","CostItem"))

  DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
  WTPCat <- c('WTP', 'WTPOngoing')
  LostProdCat <- c("HumanCapital","FrictionHigh", "FrictionLow")

  Categorised <- totals %>%
    map_depth(3,~{
      .x$Direct <- reduce(.x[DirectCat],`+`) #sum over direct costs
      .x$WTP <- reduce(.x[WTPCat],`+`) #sum of WTP and WTP-onging
      .x <- .x[c("Deaths",LostProdCat, paste0("Total",LostProdCat),"Direct","WTP")] #drop sub-categories
      names(.x) <- c("Premature mortality", LostProdCat, paste0("Total",LostProdCat), 'Direct','Pain and suffering')
      .x
    }) %>%
    quantilesNestedList(4, c("Pathogen", "Disease","AgeGroup","CostItem"))

  list(Categorised = Categorised, Detailed = Detailed)
}

##### Calculate and store selected summaries ##################################
TotalCostByPathogen <- CostList %>%
  map_depth(3,~.x$TotalHumanCapital) %>% #Extract out total cost (human capital method)
  map_depth(2,~reduce(.x,`+`)) %>% #Sum over age groups
  map_depth(1,~reduce(.x,`+`)) #Sum over diseases (initial and sequelae)

TotalIncidence <- IncidenceList %>%
  map(~.x[[1]]) %>% #only consider initial infections (drop sequelae counts)
  map_depth(1,~reduce(.x,`+`)) #sum over age groups

#Note this includes all sequelae for the purpose of counting costs, but only the initial cases for the purpose of counting cases.
CostPerCase <- map2(TotalCostByPathogen,TotalIncidence,~.x/.y) %>%
  quantilesNestedList(1,"Pathogen")
write.csv(CostPerCase, './Outputs/CostPerCase.csv')


IncidenceTable <- summariseEpiList(IncidenceList)
HospTable <- summariseEpiList(HospList)
DeathTable <- summariseEpiList(DeathList)
EpiTable <- bind_rows(Deaths = DeathTable,
                      Hospitalisations = HospTable,
                      Cases = IncidenceTable,
                      .id = 'Measure')
write.csv(EpiTable,'./Outputs/EpiTable.csv')

CostSummaries <- summariseCostList(CostList)
write.csv(CostSummaries$Detailed,'./Outputs/CostTable.csv')
write.csv(CostSummaries$Categorised,'./Outputs/CostTableCategories.csv')

# save workspace in two versions; one light version to be used by the shiny app
# and another larger version with everything
save.image('AusFBDiseaseImage.RData')
UnusedLargeObjects <- c('CostList','SequelaeFractions','TotalCostByPathogen',
                        'TotalIncidence')
#trim DeathList, HospList, and IncidenceList down to 1000 draws
trim <- 1000
DeathList <- DeathList %>% map_depth(3,~.x[1:trim])
HospList <- HospList %>% map_depth(3,~.x[1:trim])
IncidenceList <- IncidenceList %>% map_depth(3,~.x[1:trim])
WTPList <- WTPList %>% map_depth(2,~.x[1:trim])
Hospitalisations <- subset(Hospitalisations,
                           DC4D %in% (map(c(PathogenAssumptions,
                                            SequelaeAssumptions),
                                          ~.x$hospCodes) %>%
                                        unlist(use.names = F) %>% unique))

save(list = setdiff(ls(all.names = T), UnusedLargeObjects),
     file = 'Outputs/AusFBDiseaseImage-Light.RData')
