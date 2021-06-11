library(mc2d)
library(tidyverse)

source("./R/AlternateParameterisations.R")
source("./Data/Diseases.R")
source("./R/loadData.R")

NNDSSIncidence <- read.csv("./Data/NationallyNotifiedDiseases.csv")
row.names(NNDSSIncidence) <- NNDSSIncidence$Year

NNDSSIncidenceAgegroup <- getCasesAgegroup()
NNDSSIncidenceState <- read.csv("./Data/NationallyNotifiedDiseasesState.csv")

AustralianPopulation <- read.csv("./Data/AustralianPopulation.csv")
row.names(AustralianPopulation) <- AustralianPopulation$Year

AusPopAgegroup <- getAusPopAgeGroup()



gastroRate <- rdist(type = "rpert_alt",
                    lowq = 0.64,
                    mode = 0.74,
                    highq = 0.84,
                    lowp = 0.025,
                    highp = 0.975)

#The same multiplier is used for all diseases for under-diagnosis for cause of deaths and hospitalisations
#They used a pert 1 2 3, but not sure what the lower (0, 2.5%, or 5%?) or upper percentiles were
multiplierHospDeath <- rdist(type = "rpert_alt",
                             lowq = 1,
                             mode = 2,
                             highq = 3,
                             lowp = 0,
                             highp = 1)


estimateIncidence <- function(disease, population, ndraws = 10^6, gastroRate = NULL, cases = NULL){
  #abbreviate
  d <- disease
  n <- ndraws

  if(d$notifiable){
    if(is.null(cases)){
      cases = 1
      warning("Case numbers not provided, returning multipliers per case")
    }
    total <- cases * d$correction * draw(d$underreporting, n)
    domestic <- total * draw(d$domestic, n)
    foodborne <- domestic * draw(d$foodborne, n)
  }else{
    if(is.null(gastroRate)){
      error("gastroRate must be supplied if disease is not notifiable")
    }
    domestic <- population * draw(gastroRate, n) * draw(d$fractionOfGastro, n)
    foodborne <- domestic * draw(d$foodborne, n)
  }
  list(Domestic = domestic, Foodborne  = foodborne,
       DomesticPerMillion = domestic/population * 10^6,
       FoodbornePerMillion = foodborne/population * 10^6)
}

estimateHospDeaths <- function(disease, ndraws = 10^6, cases = NULL){
  #This estimates hospitalisations or deaths for a specific disease in a specific year
  #abbreviate
  d <- disease
  n <- ndraws
  if(is.null(cases)){
    cases = 1
    warning("Case numbers not provided, returning multipliers per case")
  }
  total <- cases * draw(multiplierHospDeath, n)
  domestic <- total * draw(d$domestic, n)
  foodborne <- domestic * draw(d$foodborne, n)

  list(Domestic = domestic, Foodborne  = foodborne)
}




estimateIncidence(DiseaseAssumptions$Salmonellosis,
                  population = subset(AusPopAgegroup,Year == 2019 & AgeGroup == "<5")$Persons,
                  cases = subset(NNDSSIncidenceAgegroup,Year == 2020 & AgeGroup == "<5")$Cases)







estimateIncidence(DiseaseAssumptions$Campylobacteriosis,
                  gastroRate = gastroRate,
                  population = AustralianPopulation["2010",2],
                  cases = NNDSSIncidence["2010", "Campylobacteriosis"],
                  ndraws = 10^6)[["Domestic"]] %>%
  quantile(p = c(0.05,0.5,0.95)) %>%
  t() %>%
  mutate(Meas = paste0(c(0.05,0.5,0.95) * 100,"%"))



# Recalculate all the multipliers that can apply to all years and populations?
# Currently this has issues with incidence per 1000,000 population, but this
# doesn't have to be pre-calculated
temp <-  DiseaseAssumptions %>%
  map(.f = function(x,...){
    estimateIncidence(x, ...) %>%
      map(.f = quantile,p = c(0.05,0.5,0.95))
  },
  gastroRate = gastroRate,
  population = 1,
  cases = 1,
  ndraws = 10^6) %>%
  as.data.frame() %>%
  mutate(Percentile = row.names(.)) %>%
  pivot_longer(-Percentile,
               names_to = c("Disease","Multiplier"),
               names_sep = "\\.")




