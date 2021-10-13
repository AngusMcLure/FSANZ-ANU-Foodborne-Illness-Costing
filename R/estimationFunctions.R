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
      stop("gastroRate must be supplied if disease is not notifiable")
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

estimateDeaths <- function(disease, ageGroup, ndraws = 10^6, reports = NULL){
  if(is.null(reports)){ #if deaths aren't provided use the population adjusted

  }
}

# estimateGP, estimateED, estimateSequelae, esimateMeds, estimateTests can all be generalised by a single function

estimateGeneric <- function(items, ndraws = 10^6, incidence = NULL){
  if(class(items) != 'list') stop('items must be a list')
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


costWTP <- function(disease, ageGroup, cases, separations = NULL, discount = NULL, ndraws = 10^6){

  if(length(cases) != 1 & length(cases) != ndraws){
    stop('The length of cases should either be 1 or equal to ndraws')
  }

  if(!is.null(separations) && length(separations) != 1 && length(cases) != ndraws){
    stop('The length of separations should either be 1 or equal to ndraws')
  }

  if(disease$kind == 'initial'){
    costNonHosp <- (cases - separations)  * disease$duration["NonHosp"] *
      subset(WTPValues, severity == disease$severity["NonHosp"] & symptom == disease$symptoms)$WTPvalue
    costHosp <- separations  * disease$duration["Hosp"] *
      subset(WTPValues, severity == disease$severity["Hosp"] & symptom == disease$symptoms)$WTPvalue
    list(FirstYear = costNonHosp + costHosp,
         Ongoing = 0)
  }else{
    WTPMild <- subset(WTPValues, severity=='mild' & symptom == disease$symptoms)$WTPvalue
    WTPSevere <- subset(WTPValues, severity=='severe' & symptom == disease$symptoms)$WTPvalue
    FirstYear <- WTPMild * (1- disease$propSevere) + WTPSevere * disease$propSevere;
    Ongoing <- WTPMild * draw(disease$propOngoing[[ageGroup]],ndraws) *
      (1 - (1-discount)^disease$durationOngoing)/discount * (1 - discount)
    list(FirstYear = FirstYear * cases,
         Ongoing = Ongoing * cases)
  }
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

costHumanCapital <- function(disease,ageGroup,cases,ndraws){
  if(length(cases) != 1 & length(cases) != ndraws){
    stop('The length of cases should either be 1 or equal to ndraws')
  }


  if(disease$symptoms == "GI"){
    parsC <- subset(MissedDaysGastro, AgeGroup == ageGroup & Type == "Carer")
    CarerDays <- cases * rlnorm(ndraws,
                                meanlog = log(parsC$estimate.mu),
                                sdlog = parsC$sd.mu/parsC$estimate.mu)
    if(ageGroup !="<5"){
      parsS <- subset(MissedDaysGastro, AgeGroup == ageGroup & Type == "Self")
      SelfDays <- cases * rlnorm(ndraws,
                                 meanlog = log(parsS$estimate.mu),
                                 sdlog = parsS$sd.mu/parsS$estimate.mu)
    }
  }else{
    CarerDays <- cases * disease$missedWorkCarer[[ageGroup]]; #warning('Carer days off work for sequelae do not have uncertainty estimates around them')
    if(ageGroup !="<5"){
      SelfDays <- cases * disease$missedWorkSelf[[ageGroup]]; #warning('Self days off work for sequalae do not have uncertainty estimates around them')
    }
  }

  Cost <- CarerDays * subset(Incomes, AgeGroup == "5-64")$Income
  if(ageGroup !="<5"){
    Cost <-  Cost  + SelfDays * subset(Incomes, AgeGroup == ageGroup)$Income
  }
  Cost
}

# costFriction <- function(disease,ageGroup,cases,ndraws,rate){
#   if(length(cases) != 1 & length(cases) != ndraws){
#     stop('The length of cases should either be 1 or equal to ndraws')
#   }
#   if(disease$symptoms == "GI"){
#     return(costHumanCapital(disease,ageGroup,cases,ndraws) * rate)
#   }else{
#     warning("I haven't implemented friction costs for non-gastro illness")
#     return(rep(0, ndraws))}
# }

mquant <- function(x){
  map(x,~quantile(.x, probs = c(0.05,0.5,0.95)))
}

makeIncidenceList <- function(year, diseases, ndraws = 10^6){

  ageGroups <- c("<5","5-64","65+")
  names(ageGroups) <- ageGroups

  Initial <- map(diseases,
                 ~map(ageGroups,
                      function(.a){
                        if(.x$notifiable){
                          notifications <- subset(NNDSSIncidenceAgegroup,Disease == .x$name & Year == year & AgeGroup == .a)$Cases
                          if(length(notifications) == 0){
                            stop('No notifications available for this time period')
                          } else if(length(notifications) > 1){
                            stop('More than one number found for notifications...')
                          }
                        }else{
                          stop('I have not implemented the logic for non-notifiable diseases yet')
                        }

                        estimateIncidence(.x,
                                          notifications = notifications,
                                          ndraws = ndraws)$Foodborne
                      }
                 )
  )
  Sequel <- imap(diseases,
                 ~imap(ageGroups,
                       function(.a,.n){
                         estimateSequelae(.x,
                                          incidence = Initial[[.y]][[.n]],
                                          ndraws = ndraws)
                       }
                 )
  ) #%>% as.data.frame() %>% pivot_longer(everything(),names_sep = "\\.", names_to = c("InitialDisease", "AgeGroup", "Disease"))
  # bind_rows(Initial = Initial %>% as.data.frame() %>%
  #             mutate(Draw = 1:ndraws) %>%
  #             pivot_longer(-Draw,names_sep = "\\.", names_to = c("Disease", "AgeGroup")),
  #           Sequel = Sequel %>% as.data.frame() %>%
  #             mutate(Draw = 1:ndraws) %>%
  #             pivot_longer(-Draw,names_sep = "\\.", names_to = c("InitialDisease", "AgeGroup","Disease")),
  #           .id = "Type")
  list(Initial = Initial, Sequel = Sequel)
}

calcSequelaeFractions <- function(sequelIncidence){
  sequelIncidence %>% as.data.frame(check.names = FALSE) %>%
    mutate(Draw = 1:ndraws) %>%
    pivot_longer(-Draw,names_sep = "\\.", names_to = c("InitialDisease", "AgeGroup","Disease")) %>%
    pivot_wider(values_from = value, names_from = InitialDisease) %>%
    mutate(Total = rowSums(across(-c(Draw,Disease,AgeGroup)), na.rm = TRUE)) %>%
    mutate(across(-c(Draw,Disease,AgeGroup),~.x/Total)) %>%
    select(-Total)
}

makeDeathsList <- function(year, diseases, ndraws = 10^6){

  ageGroups <- c("<5","5-64","65+")
  names(ageGroups) <- ageGroups

  AusPop <- getAusPopAgeSex()

  # Adjust to target year
  PopInTargetYear <- AusPop %>% subset(Year == year) %>%
    mutate(AgeGroup = ifelse(Age<5, "<5",ifelse(Age<65,"5-64", "65+"))) %>%
    group_by(AgeGroup) %>%
    summarise(Population = sum(Count))

  Deaths <- map(diseases,function(.d){
    map(ageGroups,function(.a){
      dths <- subset(Deaths,Cause %in% .d$mortCodes & AgeGroup == .a)
      #rgamma(ndraws, sum(dths$Count) + 0.5, dths$PersonYears[1]) * subset(PopInTargetYear, AgeGroup == .a)$Population *
      rbeta(ndraws, sum(dths$Count) + 0.5, dths$PersonYears[1] - sum(dths$Count) + 0.5) * subset(PopInTargetYear, AgeGroup == .a)$Population *
        draw(.d$underdiagnosis,ndraws) *
        draw(.d$domestic,ndraws) *
        draw(.d$foodborne,ndraws)
    })
  })
  return(Deaths)
}



estimateCosts <- function(disease, year, ageGroup, ndraws = 10^6,
                          notifications = NULL, separations = NULL, deaths = NULL,
                          discount){
  d <- disease
  a <- ageGroup
  #p <- subset(AusPopAgegroup,Year == year & AgeGroup == a)$Persons #This is unnecessary since we are not calculating rates

  if(is.null(notifications)){
    notifications <- subset(NNDSSIncidenceAgegroup,Year == year & AgeGroup == ageGroup & Disease == d$name)$Cases
    incidencePrimary <- IncidenceList$Initial[[d$name]][[a]]
    incidenceSequelae <- IncidenceList$Sequel[[d$name]][[a]]
  }else{
    incidencePrimary <- estimateIncidence(d, notifications = notifications, ndraws = ndraws)$Foodborne
    incidenceSequelae <- estimateSequelae(d,incidence = incidencePrimary, ndraws = ndraws)
  }
  incidence <- c(list(incidencePrimary),incidenceSequelae)
  names(incidence)[1] <- d$name
  dList <- c(list(d),
             SequelaeAssumptions[names(d$sequelae)])
  names(dList)[1] <- d$name

  if(is.null(separations)){
    separations <- map(dList,~{
      out <- sum(subset(Hospitalisations,DC4D %in% .x$hospCodes & AgeGroup == a & FYNumeric == year)$Separations)
      if(.x$kind == "sequel"){
        return(out * subset(SequelaeFractions, AgeGroup == a & Disease == .x$name)[,d$name, drop = TRUE])
      }else{return(out)}
    }
    )
  }

  if(is.null(deaths)){
    deaths <- map(dList,~{
      out <- DeathsList[[.x$name]][[a]]
      if(.x$kind == "sequel"){
        return(out * subset(SequelaeFractions, AgeGroup == a & Disease == .x$name)[,d$name, drop = TRUE])
      }else{return(out)}
    }
    )
  }

  out <- pmap(list(dList,incidence,separations,deaths),~{
    GP <- estimateGP(..1, ..2, ndraws = ndraws)
    ED <- estimateED(..1, ..2, ndraws = ndraws)
    Meds <- estimateMeds(..1, ageGroup = a,
                         incidence = switch(..1$medicationsToWhom,
                                            GP = GP$gpShort + GP$gpLong,
                                            Cases = ..2,
                                            Notifications = notifications,
                                            None = rep(0,ndraws)),
                         ndraws = ndraws)
    Tests <- estimateTests(..1, ageGroup = a,
                           incidence = switch(..1$testsToWhom,
                                              GP = GP$gpShort + GP$gpLong,
                                              Cases = ..2,
                                              Notifications = notifications,
                                              None = rep(0,ndraws)),
                           ndraws=ndraws)
    Hosp <- switch(..1$hospMethod,
                   AIHW = estimateHosp(..1,separations = ..3, ndraws = ndraws),
                   AllCases = list(Foodborne = ..2))

    HumanCapital <- costHumanCapital(..1, a, ..2, ndraws = ndraws)
    # FrictionHigh <- costFriction(..1, a, ..2, ndraws = ndraws, FrictionRates$High)
    # FrictionLow <- costFriction(..1, a, ..2, ndraws = ndraws, FrictionRates$Low)
    FrictionHigh <- HumanCapital * FrictionRates$High
    FrictionLow <- HumanCapital * FrictionRates$Low
    warning('Friction costs assume time off is below the 3 month cap!')
    Deaths <- VSL * ..4
    WTP = costWTP(..1,a,cases = ..2, separations = ..3,discount = discount, ndraws = ndraws)

    out <- list(GP = costList(GP),
                ED = costList(ED),
                Medications = costList(Meds),
                Tests = costList(Tests),
                Hospitalisation = Hosp$Foodborne * Costs[..1$DRGCodes[[a]],"Cost"],
                Deaths = Deaths,
                WTP = WTP$FirstYear,
                WTPOngoing = WTP$Ongoing,
                HumanCapital = HumanCapital,
                FrictionHigh = FrictionHigh,
                FrictionLow = FrictionLow)
    map(out, ~{if(length(.x) == 0){rep(0, ndraws)}else{.x}})
  }
  )
  out
  #Hosp = HospPrimary$Foodborne * Costs[d$DRGCodes[[a]],"Cost"])
}


makeCostList <- function(year,
                         diseases,
                         ndraws = 10^6, discount){
  AgeGroups <- c("<5","5-64","65+")
  names(AgeGroups) <- c("<5","5-64", "65+")

  map(diseases, function(.d){
    map(AgeGroups, function(.a){
      estimateCosts(.d,year,.a,ndraws = ndraws, discount = discount)
    })
  })
}
