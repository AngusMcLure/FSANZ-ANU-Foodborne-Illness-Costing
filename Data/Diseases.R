#Assumptions specific to each disease
IBSMultiplier <- rdist("rpert_alt", lowp = 0.025, lowq = 0.072, median = 0.088, highp = 0.975, highq = 0.104)

MedicationsBacterial <- list(
  Antidiarrhoeal = list(`0-4` = rdist("rpert_alt", lowp = 0.025, lowq = 0.0025,median=0.003, highp = 0.975, highq=0.3),
                        `5-64`= rdist("rpert_alt", lowp = 0.025, lowq = 0.166, median=0.286, highp = 0.975, highq=0.439),
                        `65+` = rdist("rpert_alt", lowp = 0.025, lowq = 0.094, median=0.653, highp = 0.975, highq=0.906)),
  Painkillers =    list(`0-4` = rdist("rpert_alt", lowp = 0.025, lowq = 0.12,  median=0.38,  highp = 0.975, highq=0.65) ,
                        `5-64`= rdist("rpert_alt", lowp = 0.025, lowq = 0.127, median=0.242, highp = 0.975, highq=0.385),
                        `65+` = rdist("rpert_alt", lowp = 0.025, lowq = 0.008, median=0.307, highp = 0.975, highq=0.708)),
  AntiNausea =     list(`0-4` = rdist("rpert_alt", lowp = 0.025, lowq = 0.0025,median=0.003, highp = 0.975, highq=0.305),
                        `5-64`= rdist("rpert_alt", lowp = 0.025, lowq = 0.042, median=0.12,  highp = 0.975, highq=0.237),
                        `65+` = rdist("rpert_alt", lowp = 0.025, lowq = 0.008, median=0.307, highp = 0.975, highq=0.708)),
  AntiCramps =     list(`0-4` = rdist("rdiscrete", x = 0), #this is just a point mass at 0, but framing it as a distribution for consistency
                        `5-64`= rdist("rpert_alt", lowp = 0.025, lowq = 0.028, median=0.077, highp = 0.975, highq=0.204),
                        `65+` = rdist("rpert_alt", lowp = 0.025, lowq = 0.008, median=0.277, highp = 0.975, highq=0.708)),
  Antibiotics =    list(`0-4` = rdist("rpert_alt", lowp = 0.025, lowq = 0.003, median=0.082, highp = 0.975, highq=0.305),
                        `5-64`= rdist("rpert_alt", lowp = 0.025, lowq = 0.016, median=0.067, highp = 0.975, highq=0.169),
                        `65+` = rdist("rert_alt",  lowp = 0,     lowq = 0,     median=0.051, highp = 0.975, highq=0.104)) #this has min 0 -- need to check if this is correct
)

# MedAntibioticsShigella <-list(AllAges = rdist("rpert_alt", lowp = 0.025, lowq = 0.25,  median=0.37,  highp = 0.975, highq=0.50))


DiseaseAssumptions <- list(
  Campylobacteriosis = list(name = "Campylobacteriosis",
                            notifiable = TRUE,
                            correction = 1.5,
                            domestic = rdist("rpert", min = 0.91, mode = 0.97, max = 0.99),
                            underreporting = rdist("rlnorm_alt", mean = 10.45, sd = 2.98),
                            foodborne = rdist("rpert_alt", lowq = 0.62, mode = 0.77, highq = 0.89, lowp = 0.05, highp = 0.95)
  ),

  Salmonellosis = list(name = "Salmonellosis (Non-Typhoidal)",
                       notifiable = TRUE,
                       correction = 1,
                       domestic = rdist("rpert", min = 0.7, mode = 0.85, max = 0.95),
                       underreporting = rdist("rlnorm_alt", mean = 7.44, sd = 2.38),
                       foodborne = rdist("rpert_alt", lowq = 0.53, highq = 0.86, median = 0.72, lowp = 0.05, highp = 0.95),
                       gp = rdist('rpert_alt', lowp = 0.025, lowq = 0.241, median = 0.367, highp = 0.975, highq = 0.501),
                       ed = rdist('rpert_alt', lowp = 0.025, lowq = 0.06, median = 0.124, highp = 0.975, highq = 0.228),
                       sequelae = list(ReactiveArthritis = rdist("rpert_alt", lowq = 0, lowp = 0, median = 0.085, highq = 0.26, highp = 1),
                                       IBS = IBSMultiplier),
                       hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.77),
                       hospCodes = paste0("A02.",1:9),
                       mortCodes = "A02",
                       underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
                       medications = MedicationsBacterial,
                       test = rdist("rdiscrete", x = 1) # i.e. all have tests
  ),


  Listeriosis = list(name = "Listeriosis",
                     notifiable = TRUE,
                     correction = 1,
                     domestic = rdist("rpert", min = 1, mode = 1, max = 1),
                     underreporting = rdist("rpert_alt", lowq = 1, mode = 2, highq = 3, lowp = 0.025, highp = 0.975),
                     foodborne = rdist("rpert", min = 0.9, mode = 0.98, max = 1)
  ),


  Typhoid = list(name = "Typhoid Fever",
                 notifiable = TRUE,
                 correction = 1,
                 domestic = rdist("rpert", min = 0.02, mode = 0.11, max = 0.25),
                 underreporting = rdist("rpert_alt", lowq = 1, mode = 2, highq = 3, lowp = 0.025, highp = 0.975),
                 foodborne = rdist("rpert", min = 0.02, mode = 0.75, max = 0.97)
  ),

  STEC = list(name = "STEC",
              notifiable = TRUE,
              correction = 13.4,
              domestic = rdist("rpert", min = 0.93, mode = 0.99, max = 1),
              underreporting = rdist("rlnorm_alt", mean = 8.83, sd = 3.7),
              foodborne = rdist("rpert_alt", lowq = 0.32, mode = 0.56, highq = 0.83, lowp = 0.05, highp = 0.95)
  ),

  Shigellosis = list(name = "Shigellosis",
                     notifiable = TRUE,
                     correction = 1,
                     domestic = rdist("rpert", min = 0.45, mode = 0.7, max = 0.84),
                     underreporting = rdist("rlnorm_alt", mean = 7.44, sd = 2.38),
                     foodborne = rdist("rpert_alt", lowq = 0.05, mode = 0.12, highq = 0.23, lowp = 0.05, highp = 0.95)
  ),

  Yersinia = list(name = "Yersinia enterocolitica",
                  notifiable = TRUE,
                  correction = 9.61,
                  domestic = rdist("rpert", min = 0.8, mode = 0.9, max = 1),
                  underreporting = rdist("rlnorm_alt", mean = 7.44, sd = 2.38),
                  foodborne = rdist("rpert", min = 0.28, mode = 0.84, max = 0.94)
  ),

  Ecoli = list(name = "Escherichia coli (Non-STEC)",
               notifiable = FALSE,
               fractionOfGastro = rdist("rpert_alt", lowq = 0.0525, mode = 0.074, highq = 0.0914, lowp = 0.025, highp = 0.975),
               foodborne = rdist("rpert_alt", lowq = 0.08, mode = 0.23, highq = 0.55, lowp = 0.05, highp = 0.95)
  ),

  Norovirus = list(name = "Norovirus",
                   notifiable = FALSE,
                   fractionOfGastro = rdist("rpert_alt", lowq = 0.0772, mode = 0.0982, highq = 0.1226, lowp = 0.025, highp = 0.975),
                   foodborne = rdist("rpert_alt", lowq = 0.05, mode = 0.18, highq = 0.35, lowp = 0.05, highp = 0.95)
  )
)
#Toxoplasma <- list(name = "Toxoplasma gondii")

SequelaeAssumptions <- list(
  ReactiveArthritis = list(
    domestic =   rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.86,  highq = 0.95,  median = 0.91),  #not sure what these multipliers are for exactly...
    bacterial =  rdist("rpert_alt", lowp = 0,     highp = 1,     lowq = 0.5,   highq = 0.947, median = 0.66), #ditto
    foodborne =  rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.36,  highq = 0.61,  median = 0.48),#ditto
    gp =         rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.66,  highq = 0.89,  median = 0.8),
    ed =         rdist("rdiscrete", x = 0), #i.e. none
    specialist = rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.223, highq = 0.258, median = 0.24),
    ongoing =    rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.23,  highq = 0.77,  median = 0.5),
    hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.5),
    hospCodes = c("M02.1", "M02.3","M02.8","M03.2"),
    mortCodes = c("M02.1","M02.8"),
    underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
    medications = list(Antibiotics = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16 , highq = 0.244, median = 0.2)),
                       NSAID = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.528, highq = 0.762, median = 0.762)),
                       EyeDrops = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16, highq = 0.244, median = 0.2)),
                       Prednisone = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.001, highq = 0.099, median = 0.019)),
                       InterarticularGlucocorticoid = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16, highq = 0.244, median = 0.2)),
                       DMARD = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012, highq = 0.304, median = 0.095)),
                       JointAspiration = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16, highq = 0.244, median = 0.2))
                       )
  ),
  IBS = list(
    domestic =   rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.88,  highq = 0.94,  median = 0.91),  #not sure what these multipliers are for exactly...
    bacterial =  rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.06,  highq = 0.33,  median = 0.17),  #ditto
    foodborne =  rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.08,  highq = 0.33,  median = 0.13),  #ditto
    gp =         rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 4.27,  highq = 4.73,  median = 4.5),
    ed =         rdist("rdiscrete", x = 0), #i.e. none
    specialist = rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.286, highq = 0.315, median = 0.3),
    ongoing =    rdist("rpert_alt", lowp = 0.025, highp = 0.975, lowq = 0.218, highq = 0.66,  median = 0.429),
    hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.5),
    hospCodes = c("K58.0", "K58.9"),
    mortCodes = c("K58"),
    underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
    medications = list(AnyMedication = list(AllAges = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.385, highq = 0.416, median = 0.4)))
  )
)

