library(mc2d) #for the PERT distribution
library(tidyverse)

source("./R/AlternateParameterisations.R")
source("./Data/Diseases.R")
source("./R/loadData.R")

#NNDSSIncidence <- read.csv("./Data/NationallyNotifiedDiseases.csv")
#row.names(NNDSSIncidence) <- NNDSSIncidence$Year

NNDSSIncidenceAgegroup <- getCasesAgeGroup()
#NNDSSIncidenceState <- read.csv("./Data/NationallyNotifiedDiseasesState.csv")

#AustralianPopulation <- read.csv("./Data/AustralianPopulation.csv")
#row.names(AustralianPopulation) <- AustralianPopulation$Year

AusPopAgegroup <- getAusPopAgeGroup()
Hospitalisations <- getHospitalisationsAgeGroup()
Costs <- getCosts()



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


estimateIncidence <- function(disease, population = NULL, ndraws = 10^6, gastroRate = NULL, notifications = NULL){
  #abbreviate
  d <- disease
  n <- ndraws

  if(d$notifiable){
    if(is.null(notifications)){
      cases = 1
      warning("Notification numbers not provided, returning multipliers per notification")
    }
    total <- notifications * d$correction * draw(d$underreporting, n)
    domestic <- total * draw(d$domestic, n)
  }else{
    if(is.null(gastroRate)){
      error("gastroRate must be supplied if disease is not notifiable")
    }
    domestic <- population * draw(gastroRate, n) * draw(d$fractionOfGastro, n)
  }
  foodborne <- domestic * draw(d$foodborne, n)

  out <- list(Domestic = domestic, Foodborne  = foodborne)
  if(!is.null(population)){
    out <- c(out, list(DomesticPerMillion = domestic/population * 10^6,
                       FoodbornePerMillion = foodborne/population * 10^6))
  }
  out
}

estimateHosp <- function(disease, population = NULL, ndraws = 10^6, separations = NULL){
  #This estimates hospitalisations for a specific disease in a specific year
  #abbreviate
  d <- disease
  n <- ndraws
  if(is.null(separations)){
    seperations = 1
    warning("Seperations with primary diagnoses not provided, returning multipliers per separation")
  }
  total <- separations / draw(d$hospPrincipalDiagnosis, n) * draw(d$underdiagnosis, n)
  domestic <- total * draw(d$domestic, n)
  foodborne <- domestic * draw(d$foodborne, n)

  out <- list(Domestic = domestic, Foodborne  = foodborne)
  if(!is.null(population)){
    out <- c(out, list(DomesticPerMillion = domestic/population * 10^6,
                       FoodbornePerMillion = foodborne/population * 10^6))
  }
  out
}

# estimateGP, estimateED, estimateSequelae, esimateMeds, estimateTests can all be generalised by a single function

estimateGeneric <- function(items, ndraws = 10^6, incidence = NULL){
  n <- ndraws
  if(is.null(incidence)){
    incidence = 1
    warning("Case numbers of underlying cause are not provided, returning multipliers per case")
  }
  if(length(incidence) != 1 & length(incidence) != n){
    stop('The length of incidence should either be 1 or equal to ndraws')
  }
  map(items, ~{draw(.x, n) * incidence})
}
estimateGP <- function(disease, ndraws = 10^6, incidence = NULL){
  estimateGeneric(disease[c("gpShort","gpLong")],
                  ndraws = ndraws, incidence = incidence)
}
estimateED <- function(disease, ndraws = 10^6, incidence = NULL){
  ed = estimateGeneric(disease["ed"],
                       ndraws = ndraws, incidence = incidence)
}
estimateSequelae <- function(disease, ndraws = 10^6, incidence = NULL){
  estimateGeneric(disease[["sequelae"]],
                  ndraws = ndraws, incidence = incidence)
}
estimateMeds <- function(disease, ageGroup, ndraws = 10^6, incidence = NULL){
  estimateGeneric(c(disease[["medications"]][["AllAges"]], disease[["medications"]][[ageGroup]]),
                  ndraws = ndraws, incidence = incidence)
}
estimateTests <- function(disease, ageGroup, ndraws = 10^6, incidence = NULL){
  estimateGeneric(c(disease[["tests"]][["AllAges"]], disease[["tests"]][[ageGroup]]),
                  ndraws = ndraws, incidence = incidence)
}

costList <- function(l){
  MissingCosts <- setdiff(names(l),row.names(Costs))
  if(length(MissingCosts)){
    stop("No cost item(s) called ", cat(MissingCosts,sep = ", "))
  }

  if(!is.numeric(Costs[names(l),"Cost"]) | any(is.na(Costs[names(l),"Cost"]))){
    stop('invalid costs:', paste(names(l)[is.na(Costs[names(l),"Cost"])]))
  }

  l %>%
    imap(~{Costs[.y,"Cost"] * .x}) %>%
    data.frame %>%
    rowSums
}

mquant <- function(x){
  map(x,~quantile(.x, probs = c(0.05,0.5,0.95)))
}



estimateCosts <- function(disease, year = NULL, ageGroup, ndraws = 10^6, notifications = NULL, separations = NULL){
  d <- disease
  a <- ageGroup
  #p <- subset(AusPopAgegroup,Year == year & AgeGroup == a)$Persons #This is unnecessary since we are not calculating rates

  if(is.null(notifications)){
    notifications <- subset(NNDSSIncidenceAgegroup,Disease == d$name & Year == year & AgeGroup == a)$Cases
  }

  IncidencePrimary <- estimateIncidence(d, notifications = notifications, ndraws = ndraws)$Foodborne
  IncidenceSequelae <- estimateSequelae(d,incidence = IncidencePrimary,ndraws = ndraws)
  Incidence <- c(list(IncidencePrimary),IncidenceSequelae)
  names(Incidence)[1] <- d$name

  dList <- c(list(d),
             SequelaeAssumptions[names(d$sequelae)])
  names(dList)[1] <- d$name

  # Number of reported hospitalisations for the pathogen and its sequelae
  # NEEDS ADJUSTMENT FOR THE SEQUELAE SINCE ONLY A FRACTION WILL BE DUE TO SPECIFIC PATHOGENS

  if(is.null(separations)){
    separations <- map(dList,~sum(subset(Hospitalisations,DC4D %in% .x$hospCodes & AgeGroup == a & FYNumeric == year)$Separations))
  }

  warning("HOSPITALISATION COSTS NEED ADJUSTMENT FOR THE SEQUELAE SINCE FOR REA AND IBS ONLY A FRACTION WILL BE DUE TO A GIVEN PATHOGEN")

  out <- map2(dList,Incidence,~{
    GP <- estimateGP(.x, .y, ndraws = ndraws)
    ED <- estimateED(.x, .y, ndraws = ndraws)
    Meds <- estimateMeds(.x, ageGroup = a,
                         incidence = switch(.x$medicationsToWhom,
                                            GP = GP$gpShort + GP$gpLong,
                                            Cases = .y,
                                            Notifications = notifications),
                         ndraws = ndraws)
    Tests <- estimateTests(.x, ageGroup = a,
                           incidence = switch(.x$testsToWhom,
                                              GP = GP$gpShort + GP$gpLong,
                                              Cases = .y,
                                              Notifications = notifications),
                           ndraws=ndraws)
    Hosp <- estimateHosp(.x,separations = separations[[.x$name]], ndraws = ndraws)
    list(GP = costList(GP),
         ED = costList(ED),
         Medications = costList(Meds),
         Tests = costList(Tests),
         Hospitalisation = Hosp$Foodborne * Costs[.x$DRGCodes[[a]],"Cost"])}
  )
  out
  #Hosp = HospPrimary$Foodborne * Costs[d$DRGCodes[[a]],"Cost"])
}

#### Calculate the number of each type of sequel illness that a foodborne


SalmOutbreakAssumptions <- DiseaseAssumptions$Salmonellosis

SalmOutbreakAssumptions$foodborne <- rdist("rdiscrete", x = 1)
SalmOutbreakAssumptions$domestic <- rdist("rdiscrete", x = 1)
SalmOutbreakAssumptions$underreporting <- rdist("rdiscrete", x = 203/91)
SalmOutbreakAssumptions$ed <- rdist("rdiscrete", x = 58/203)
SalmOutbreakAssumptions$hospPrincipalDiagnosis <- rdist("rdiscrete", x = 1)
SalmOutbreakAssumptions$underdiagnosis <- rdist("rdiscrete", x = 1)

SalmOutbreakCost <- estimateCosts(SalmOutbreakAssumptions, ageGroup = "5-64",notifications = 91,
                                  separations = list(Salmonellosis = 32, IBS = 0, ReactiveArthiritis = 0))

mquant(SalmOutbreakCost$Salmonellosis)
######

ndraws <- 10^6
AgeGroups <- c("<5","5-64","65+")
names(AgeGroups) <- c("LessThan5","Ages5To64","Ages65AndOver")
temp <- map(AgeGroups, ~estimateCosts(DiseaseAssumptions$Salmonellosis,2019,.x,ndraws = ndraws))


tempLong <- data.frame(temp) %>%
  mutate(Draw = 1:ndraws) %>%
  pivot_longer(-Draw,names_sep = "\\.",
               names_to = c("AgeGroup","Disease","CostItem"),
               values_to = "Cost") %>%
  mutate(AgeGroup = recode_factor(AgeGroup, !!!as.list(AgeGroups)))

tempSummary <- tempLong %>%
  group_by(Disease, CostItem) %>%
  group_modify(~{bind_rows(.x, .x %>% group_by(Draw) %>% summarise(Cost = sum(Cost), AgeGroup = "AllAges"))}) %>%
  group_by(AgeGroup,Disease, CostItem) %>%
  summarise(Median = round(median(Cost)/1e3),
            `5%` = round(quantile(Cost, prob = 0.05)/1e3),
            `95%` = round(quantile(Cost, prob = 0.95)/1e3))

tempSummary

View(tempSummary)

### Outbreak costing

estimateCosts(DiseaseAssumptions$Salmonellosis,2019,.x,ndraws = ndraws, notifications = )



# Recalculate all the multipliers that can apply to all years and populations?
# Currently this has issues with incidence per 1,000,000 population, but this
# doesn't have to be pre-calculated
temp <-  DiseaseAssumptions %>%
  map(.f = function(x,...){
    estimateIncidence(x, ...) %>%
      map(.f = quantile,p = c(0.05,0.5,0.95))
  },
  gastroRate = gastroRate,
  population = 1,
  notifications = 1,
  ndraws = 10^6) %>%
  as.data.frame() %>%
  mutate(Percentile = row.names(.)) %>%
  pivot_longer(-Percentile,
               names_to = c("Disease","Multiplier"),
               names_sep = "\\.")




