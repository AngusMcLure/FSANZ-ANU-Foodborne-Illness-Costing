source("./R/BuildCostTable.R")

ndraws <- 10^3
ageGroups <- c("<5","5-64","65+")
names(ageGroups) <- ageGroups

##################################################
#######Salmonella outbreak in bakery (2016)#######
##################################################

SalmOutbreakTyphimurium <- PathogenAssumptions$`Non-typhoidal salmonella`

warning('In the document we specifically say that LOS was 5 days, but this is not reflected in our costing in any way')

SalmOutbreakCost <- estimateCosts(SalmOutbreakTyphimurium,
                                  notifications = list(`<5` = 0, `5-64` = 91, `65+` = 0),
                                  cases = list(Salmonellosis = list(`<5` = 0, `5-64` = 32, `65+` = 0),
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
## Salmonella Enteridis Outbreak 2019

##################################################
######Listeria outbreak in rockmelons (2018)######
##################################################

ListeriaOutbreak <- PathogenAssumptions$`Listeria monocytogenes`

ListeriaOutbreak$domestic <- rdist("discrete", value = 1)

warning('We are assuming 10 cases were 65+ and the remainder 5-64')
warning('How are we costing the miscarriage? One additional death? (for a total of 8?)')
warning('We dont have a good way to estimate hospitalisations out of the box')

#Perhaps we could use the empirical distribution of Listeria hospitalisations / Listeria cases to get a multiplier to estimate the number of hospitalisations?

ListeriaOutbreak.notifications <- list(`<5` = 0, `5-64` = 22-10, `65+` = 10)
ListeriaOutbreak.cases <- estimateIncidence(disease = ListeriaOutbreak, ndraws = ndraws, notifications = ListeriaOutbreak.notifications)
ListeriaOutbreak.deaths <- list(`<5` = 1, `5-64` = 7, `65+` = 10)
ListeriaOutbreak.separations <- map(ageGroups,
                                    ~{HospList$`Listeria monocytogenes`$Listeriosis[[.x]]/
                                        IncidenceList$`Listeria monocytogenes`$Listeriosis[[.x]] *
                                        ListeriaOutbreak.cases[[.x]]})

ListeriaOutbreakCost <- estimateCosts(ListeriaOutbreak,
                                      ndraws = ndraws,
                                      cases = list(Listeriosis = ListeriaOutbreak.cases),
                                      notifications = list(Listeriosis = ListeriaOutbreak.notifications),
                                      separations = list(Listeriosis = ListeriaOutbreak.separations),
                                      deaths = list(Listeriosis = ListeriaOutbreak.deaths),
                                      year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                      discount = 0) #We are not even using discounting any more so I might drop this argument all together?

ListeriaOutbreakCost.Summaries <- summariseCostList(list(`Listeria monocytogenes` = ListeriaOutbreakCost))
View(ListeriaOutbreakCost.Summaries$Detailed)

##################################################
#######Salmonella enteritidis in eggs (2019)######
##################################################

EnteritidisOutbreak <- PathogenAssumptions$`Non-typhoidal salmonella`
EnteritidisOutbreak$domestic <- rdist("discrete", value = 1)

EnteritidisOutbreak.notifications <- list(`<5` = 0, `5-64` = 235, `65+` = 0)
EnteritidisOutbreak.cases <- estimateIncidence(disease = EnteritidisOutbreak,
                                               ndraws = ndraws,
                                               notifications = EnteritidisOutbreak.notifications)
EnteritidisOutbreak.deaths <- list(`<5` = 0, `5-64` = 1, `65+` = 0)
EnteritidisOutbreak.sequelae <- estimateSequelae(EnteritidisOutbreak, EnteritidisOutbreak.cases)
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
WeltevredenOutbreak.sequelae <- estimateSequelae(WeltevredenOutbreak, WeltevredenOutbreak.cases)
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

WeltevredenOutbreakCost.Summaries <- summariseCostList(list(`Salmonella Enteritidis` = WeltevredenOutbreakCost))
View(WeltevredenOutbreakCost.Summaries$Detailed)
View(WeltevredenOutbreakCost.Summaries$Categorised)




