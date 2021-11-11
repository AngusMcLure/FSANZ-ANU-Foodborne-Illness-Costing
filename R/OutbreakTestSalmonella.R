source("./R/BuildCostTable.R")


## Salmonella Typhimurium Outbreak 2016
SalmOutbreakTyphimurium <- DiseaseAssumptions$Salmonellosis

SalmOutbreakTyphimurium$foodborne <- rdist("discrete", value = 1)
SalmOutbreakTyphimurium$domestic <- rdist("discrete", value = 1)
SalmOutbreakTyphimurium$underreporting <- rdist("discrete", value = 203/91)
SalmOutbreakTyphimurium$ed <- rdist("discrete", value = 58/203)
SalmOutbreakTyphimurium$hospPrincipalDiagnosis <- rdist("discrete", value = 1)
SalmOutbreakTyphimurium$underdiagnosis <- rdist("discrete", value = 1)
warning('In the document we specifically say that LOS was 5 days, but this is not reflected in our costing in any way')

SalmOutbreakCost <- estimateCosts(SalmOutbreakTyphimurium,
                                  ageGroup = "5-64",
                                  notifications = 91,
                                  separations = list(Salmonellosis = 32, IBS = 0, ReactiveArthiritis = 0),
                                  deaths = 0,
                                  year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                  discount = 0) #We are not even using discounting any more so I might drop this argument all together?

t(data.frame(mquant(SalmOutbreakCost$Salmonellosis)))

## Salmonella Enteridis Outbreak 2019

##Listeria outbreak in rockmelons (2018)

ListeriaOutbreak <- DiseaseAssumptions$Listeriosis

ListeriaOutbreak$domestic <- rdist("discrete", value = 1)
warning('We are assuming 10 cases were 65+ and the remainder 5-64')
warning('How are we costing the miscarriage? One additional death? (for a total of 8?)')
warning('We dont have a good way to estimate hospitalisations out of the box')

#Perhaps we could use the empirical distribution of Listeria hospitalisations / Listeria cases to get a multiplier to estimate the number of hospitalisations?

ListeriaOutbreakCost <- estimateCosts(ListeriaOutbreak,
                                  ageGroup = "5-64",
                                  notifications = 22,
                                  separations = NULL,
                                  deaths = 7,
                                  year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                                  discount = 0) #We are not even using discounting any more so I might drop this argument all together?



