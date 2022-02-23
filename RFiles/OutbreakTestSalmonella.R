load('AusFBDiseaseImage.RData')
source('./Outbreak.R')

ndraws <- 10^5
set.seed(20220222)
ageGroups <- c("<5","5-64","65+")
names(ageGroups) <- ageGroups

##################################################
#######Salmonella outbreak in bakery (2016)#######
##################################################

SalmOutbreakTyphimurium <- PathogenAssumptions$`Non-typhoidal salmonella`
SalmOutbreakTyphimurium$ed <- rdist('discrete', value = 91/203, continuous = FALSE)
warning('In the document we specifically say that LOS was 5 days, but this is not reflected in our costing in any way')
SalmOutbreakCost <- estimateCosts(SalmOutbreakTyphimurium,
                                  notifications = list(`<5` = 0, `5-64` = 91, `65+` = 0),
                                  cases = list(Salmonellosis = list(`<5` = 0, `5-64` = 203, `65+` = 0),
                                               IBS = list(`<5` = 0, `5-64` = 0, `65+` = 0),
                                               ReactiveArthiritis = list(`<5` = 0, `5-64` = 0, `65+` = 0)),
                                  separations = list(Salmonellosis = list(`<5` = 0, `5-64` = 32, `65+` = 0),
                                                     IBS = list(`<5` = 0, `5-64` = 0, `65+` = 0),
                                                     ReactiveArthiritis = list(`<5` = 0, `5-64` = 0, `65+` = 0)),
                                  deaths = list(Salmonellosis = list(`<5` = 0, `5-64` = 0, `65+` = 0),
                                                IBS = list(`<5` = 0, `5-64` = 0, `65+` = 0),
                                                ReactiveArthiritis = list(`<5` = 0, `5-64` = 0, `65+` = 0)),
                                  year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                  discount = 0,
                                  ndraws = ndraws) #We are not even using discounting any more so I might drop this argument all together?
SalmOutbreakCost.Summaries <- summariseCostList(list(Salmonella = SalmOutbreakCost))
View(SalmOutbreakCost.Summaries$Categorised)
View(SalmOutbreakCost.Summaries$Detailed)

SalmOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "GPSpecialist") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "gpShort")$Cost)


##################################################
######Listeria outbreak in rockmelons (2018)######
##################################################

ListeriaOutbreak <- PathogenAssumptions$`Listeria monocytogenes`
ListeriaOutbreak$domestic <- rdist("discrete", value = 1)
ListeriaOutbreak$foodborne <- rdist("discrete", value = 1)
ListeriaOutbreak.notifications <- list(`<5` = 0, `5-64` = 22-10, `65+` = 10) #We are assuming 10 cases were 65+ and the remainder 5-64
ListeriaOutbreak.deaths <- list(`<5` = 1, `5-64` = 7, `65+` = 0) #7 deaths (assumed to be 5-64) and one miscarriage (costed as an additional death)
ListeriaOutbreakCost <- costOutbreak(ListeriaOutbreak, ndraws = ndraws,
                                     notifications = ListeriaOutbreak.notifications,
                                     deaths = list(Listeriosis = ListeriaOutbreak.deaths))
ListeriaOutbreakCost.Summaries <- summariseCostList(list(`Listeria monocytogenes` = ListeriaOutbreakCost))
ListeriaOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == 'All Ages' &
           Disease == 'Listeriosis') %>%
  as.data.frame %>%
  select(CostItem, median, X5. = `5%`, X95. = `95%`) %>%
  medianCIformat(unit = 1,newline = FALSE) %>%
  select(CostItem, Cost) %>%
  `rownames<-`(.$CostItem) %>%
  `[`(c('GPSpecialist','ED','Hospitalisation','Tests','Medications','HumanCapital',
        'WTP','Deaths','TotalHumanCapital'),) %>%
  write_excel_csv('./Report/ListeriaOutbreak.csv')

ListeriaOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Listeriosis" & CostItem == "ED") %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "ed")$Cost)

ListeriaOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Listeriosis" & CostItem == "Hospitalisation") %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "T01A/T01B")$Cost)

ListeriaOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Listeriosis" & CostItem == "GPSpecialist") %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "gpShort")$Cost * 0.75 + subset(Costs, Name == "gpLong")$Cost *0.25 +
        subset(Costs, Name == "specialistInitial")$Cost * 0.5 + subset(Costs, Name == "specialistRepeat")$Cost *0.5)


##################################################
#######Salmonella enteritidis in eggs (2019)######
##################################################

EnteritidisOutbreak <- PathogenAssumptions$`Non-typhoidal salmonella`
EnteritidisOutbreak$domestic <- rdist("discrete", value = 1)

EnteritidisOutbreak.notifications <- list(`<5` = 0, `5-64` = 235, `65+` = 0)
EnteritidisOutbreak.cases <- estimateIncidence(pathogen = EnteritidisOutbreak,
                                               ndraws = ndraws,
                                               notifications = EnteritidisOutbreak.notifications)
EnteritidisOutbreak.deaths <- list(`<5` = 0, `5-64` = 1, `65+` = 0)
EnteritidisOutbreak.sequelae <- estimateSequelae(EnteritidisOutbreak, EnteritidisOutbreak.cases, ndraws = ndraws)
EnteritidisOutbreak.cases <- c(list(Salmonellosis = EnteritidisOutbreak.cases),EnteritidisOutbreak.sequelae)
EnteritidisOutbreak.separations <- imap(EnteritidisOutbreak.cases,
                                        function(.c,.dn){map(ageGroups,
                                                             function(.a){HospList$`Non-typhoidal salmonella`[[.dn]][[.a]]/
                                                                 IncidenceList$`Non-typhoidal salmonella`[[.dn]][[.a]] *
                                                                 .c[[.a]]})})

EnteritidisOutbreak.deaths <- c(list(Salmonellosis = EnteritidisOutbreak.deaths),
                                imap(EnteritidisOutbreak.cases[c("ReactiveArthritis", "IBS")],
                                     function(.c,.dn){map(ageGroups,
                                                          function(.a){DeathList$`Non-typhoidal salmonella`[[.dn]][[.a]]/
                                                              IncidenceList$`Non-typhoidal salmonella`[[.dn]][[.a]] *
                                                              .c[[.a]]})}))

EnteritidisOutbreakCost <- estimateCosts(EnteritidisOutbreak,
                                         ndraws = ndraws,
                                         cases = EnteritidisOutbreak.cases,
                                         notifications = EnteritidisOutbreak.notifications,
                                         separations = EnteritidisOutbreak.separations,
                                         deaths = EnteritidisOutbreak.deaths,
                                         year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                         discount = 0) #We are not even using discounting any more so I might drop this argument all together?

EnteritidisOutbreakCost.Summaries <- summariseCostList(list(`Salmonella Enteritidis` = EnteritidisOutbreakCost))
View(EnteritidisOutbreakCost.Summaries$Detailed)
View(EnteritidisOutbreakCost.Summaries$Categorised)

EnteritidisOutbreakCost <- costOutbreak(EnteritidisOutbreak, ndraws = ndraws,
                                        notifications = EnteritidisOutbreak.notifications,
                                        deaths = list(Salmonellosis = list(`<5` = 0, `5-64` = 1, `65+` = 0),
                                                      IBS = NULL, ReactiveArthritis = NULL))

EnteritidisOutbreakCost

# reverse engineer number of ED visits, separations etc!!!
# (If I knew I wanted this information I would have written the program differently
# --- might still do this if wanted for the outbreak tab)

EnteritidisOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "ED") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "ed")$Cost)

EnteritidisOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "Hospitalisation") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "G67B")$Cost)

EnteritidisOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "GPSpecialist") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "gpShort")$Cost)


##################################################
#######Salmonella Weltevreden in eggs (2019)######
##################################################

WeltevredenOutbreak <- PathogenAssumptions$`Non-typhoidal salmonella`
WeltevredenOutbreak$domestic <- rdist("discrete", value = 1)

WeltevredenOutbreak.notifications <- list(`<5` = 0, `5-64` = 83, `65+` = 0)
WeltevredenOutbreak.cases <- estimateIncidence(disease = WeltevredenOutbreak,
                                               ndraws = ndraws,
                                               notifications = WeltevredenOutbreak.notifications)
WeltevredenOutbreak.deaths <- list(`<5` = 0, `5-64` = 0, `65+` = 0)
WeltevredenOutbreak.sequelae <- estimateSequelae(WeltevredenOutbreak, WeltevredenOutbreak.cases, ndraws = ndraws)
WeltevredenOutbreak.cases <- c(list(Salmonellosis = WeltevredenOutbreak.cases),WeltevredenOutbreak.sequelae)
WeltevredenOutbreak.separations <- imap(WeltevredenOutbreak.cases,
                                        function(.c,.dn){map(ageGroups,
                                                             function(.a){HospList$`Non-typhoidal salmonella`[[.dn]][[.a]]/
                                                                 IncidenceList$`Non-typhoidal salmonella`[[.dn]][[.a]] *
                                                                 .c[[.a]]})})

WeltevredenOutbreak.deaths <- c(list(Salmonellosis = WeltevredenOutbreak.deaths),
                                imap(WeltevredenOutbreak.cases[c("ReactiveArthritis", "IBS")],
                                     function(.c,.dn){map(ageGroups,
                                                          function(.a){DeathList$`Non-typhoidal salmonella`[[.dn]][[.a]]/
                                                              IncidenceList$`Non-typhoidal salmonella`[[.dn]][[.a]] *
                                                              .c[[.a]]})}))

WeltevredenOutbreakCost <- estimateCosts(WeltevredenOutbreak,
                                         ndraws = ndraws,
                                         cases = WeltevredenOutbreak.cases,
                                         notifications = WeltevredenOutbreak.notifications,
                                         separations = WeltevredenOutbreak.separations,
                                         deaths = WeltevredenOutbreak.deaths,
                                         year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                         discount = 0) #We are not even using discounting any more so I might drop this argument all together?

WeltevredenOutbreakCost.Summaries <- summariseCostList(list(`Salmonella Enteritidis` = WeltevredenOutbreakCost$costs))
View(WeltevredenOutbreakCost.Summaries$Detailed)
View(WeltevredenOutbreakCost.Summaries$Categorised)


WeltevredenOutbreakCost <- costOutbreak(WeltevredenOutbreak, ndraws = ndraws,
                                        notifications = WeltevredenOutbreak.notifications,
                                        deaths = list(Salmonellosis = list(`<5` = 0, `5-64` = 0, `65+` = 0),
                                                      IBS = NULL, ReactiveArthritis = NULL))


# reverse engineer number of ED visits, separations etc!!!
# (If I knew I wanted this information I would have written the program differently
# --- might still do this if wanted for the outbreak tab)

WeltevredenOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "ED") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "ed")$Cost)

EnteritidisOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "Hospitalisation") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "G67B")$Cost)

EnteritidisOutbreakCost.Summaries$Detailed %>%
  subset(AgeGroup == "All Ages" & Disease == "Salmonellosis" & CostItem == "GPSpecialist") %>%
  ungroup %>%
  select(median:`95%`) %>%
  `/`(subset(Costs, Name == "gpShort")$Cost)


