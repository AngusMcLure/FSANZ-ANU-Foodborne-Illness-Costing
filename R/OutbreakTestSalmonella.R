source("./R/BuildCostTable.R")

SalmOutbreakAssumptions <- DiseaseAssumptions$Salmonellosis

SalmOutbreakAssumptions$foodborne <- rdist("discrete", value = 1)
SalmOutbreakAssumptions$domestic <- rdist("discrete", value = 1)
SalmOutbreakAssumptions$underreporting <- rdist("discrete", value = 203/91)
SalmOutbreakAssumptions$ed <- rdist("discrete", value = 58/203)
SalmOutbreakAssumptions$hospPrincipalDiagnosis <- rdist("discrete", value = 1)
SalmOutbreakAssumptions$underdiagnosis <- rdist("discrete", value = 1)

SalmOutbreakCost <- estimateCosts(SalmOutbreakAssumptions, ageGroup = "5-64",notifications = 91,
                                  separations = list(Salmonellosis = 32, IBS = 0, ReactiveArthiritis = 0))

mquant(SalmOutbreakCost$Salmonellosis)
