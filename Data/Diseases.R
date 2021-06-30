

#Generic assumptions

IBSMultiplier <- rdist("rpert_alt",  lowq = 0.072, median = 0.088,  highq = 0.104) # only for Campy, Salmonella and Shigella

MedicationsBacterial <- list(
  `<5`   = list(Antidiarrhoeal = rdist("rpert",      min  = 0.0025,mode   = 0.003,  max   = 0.3), #Changed to match Laura's numbers -- different from the numbers Katie initially had in the table
                Painkillers    = rdist("rpert_alt",  lowq = 0.12,  median = 0.38,   highq = 0.65) ,
                AntiNausea     = rdist("rpert",      min  = 0.0025,mode   = 0.003,  max   = 0.305), #Changed to match Laura's numbers -- different from the numbers Katie initially had in the table
                AntiCramp      = rdist("rdiscrete",  x = 0), #this is just a point mass at 0, but framing it as a distribution for consistency
                Antibiotics    = rdist("rpert_alt",  lowq = 0.003, median = 0.082,  highq = 0.305)
  ),
  `5-64` = list(Antidiarrhoeal = rdist("rpert_alt",  lowq = 0.166, median = 0.286,  highq = 0.439),
                Painkillers    = rdist("rpert_alt",  lowq = 0.127, median = 0.242,  highq = 0.385),
                AntiNausea     = rdist("rpert_alt",  lowq = 0.042, median = 0.12,   highq = 0.237),
                AntiCramp      = rdist("rpert_alt",  lowq = 0.028, median = 0.077,  highq = 0.204),
                Antibiotics    = rdist("rpert_alt",  lowq = 0.016, median = 0.067,  highq = 0.169)
  ),
  `65+`  = list(Antidiarrhoeal = rdist("rpert_alt",  lowq = 0.094, median = 0.653,  highq = 0.906),
                Painkillers    = rdist("rpert_alt",  lowq = 0.008, median = 0.307,  highq = 0.708),
                AntiNausea     = rdist("rpert_alt",  lowq = 0.008, median = 0.307,  highq = 0.708),
                AntiCramp      = rdist("rpert_alt",  lowq = 0.008, median = 0.277,  highq = 0.708),
                Antibiotics    = rdist("rpert",     min  = 0,                   mode   = 0.051,                max   = 0.104) #Laura had a point mass at zero for this in share folder but an unnamed distribution with parameters (0, 0.051, 0.104) in the supp materials of her paper. Have gone with this.
  )
)



# MedAntibioticsShigella <-list(AllAges = rdist("rpert_alt",  lowq = 0.25,  median=0.37,   highq=0.50))

#Assumptions specific to each disease

DiseaseAssumptions <- list(
  Campylobacteriosis = list(name = "Campylobacteriosis",
                            notifiable = TRUE,
                            correction = 1.5,
                            domestic = rdist("rpert", min = 0.91, mode = 0.97, max = 0.99),
                            underreporting = rdist("rlnorm_alt", mean = 10.45, sd = 2.98),
                            foodborne = rdist("rpert_alt", lowq = 0.62, mode = 0.77, highq = 0.89, lowp = 0.05, highp = 0.95),
                            sequelae = list(ReactiveArthritis = rdist("rpert_alt", min = 0.028, mode = 0.07, max = 0.16),
                                            IBS = IBSMultiplier)
  ),

  Salmonellosis = list(name = "Salmonellosis",
                       notifiable = TRUE,
                       correction = 1,
                       domestic = rdist("rpert", min = 0.7, mode = 0.85, max = 0.95),
                       underreporting = rdist("rlnorm_alt", mean = 7.44, sd = 2.38),
                       foodborne = rdist("rpert_alt", lowq = 0.53, highq = 0.86, median = 0.72, lowp = 0.05, highp = 0.95),
                       gpShort = rdist('rpert_alt',  lowq = 0.241, median = 0.367,  highq = 0.501),
                       gpLong = rdist('rdiscrete', x = 0), # No long gp consults
                       ed = rdist('rpert_alt',  lowq = 0.06, median = 0.124,  highq = 0.228),
                       sequelae = list(ReactiveArthritis = rdist("rpert", min = 0, mode = 0.085, max = 0.26),
                                       IBS = IBSMultiplier),
                       hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.77),
                       hospCodes = paste0("A02.",0:9),
                       mortCodes = "A02",
                       DRGCodes = list(`<5` = "G67B",
                                       `5-64` = "G67B",
                                       `65+` = "G67A"),
                       underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
                       medications = MedicationsBacterial,
                       medicationsToWhom = "Cases", ##CHECK THIS
                       tests = list(AllAges = list(Stool_culture = rdist("rdiscrete", x = 1))), # i.e. all notifications have tests
                       testsToWhom = "Notifications" ##CHECK THIS
  ),


  Listeriosis = list(name = "Listeriosis",
                     notifiable = TRUE,
                     correction = 1,
                     domestic = rdist("rpert", min = 1, mode = 1, max = 1),
                     underreporting = rdist("rpert_alt", lowq = 1, mode = 2, highq = 3, lowp = 0.05, highp = 0.95),
                     foodborne = rdist("rpert", min = 0.9, mode = 0.98, max = 1)
  ),


  Typhoid = list(name = "Typhoid",
                 notifiable = TRUE,
                 correction = 1,
                 domestic = rdist("rpert", min = 0.02, mode = 0.11, max = 0.25),
                 underreporting = rdist("rpert_alt", lowq = 1, mode = 2, highq = 3),
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
                     foodborne = rdist("rpert_alt", lowq = 0.05, mode = 0.12, highq = 0.23, lowp = 0.05, highp = 0.95),
                     sequelae = list(ReactiveArthritis = rdist("rpert", min = 0.012, mode = 0.097, max = 0.098),
                                     IBS = IBSMultiplier)
  ),

  `Yersinia enterocolitica` = list(name = "Yersinia enterocolitica",
                                   notifiable = TRUE,
                                   correction = 9.61,
                                   domestic = rdist("rpert", min = 0.8, mode = 0.9, max = 1),
                                   underreporting = rdist("rlnorm_alt", mean = 7.44, sd = 2.38),
                                   foodborne = rdist("rpert", min = 0.28, mode = 0.84, max = 0.94),
                                   sequelae = list(ReactiveArthritis = rdist("rpert", min = 0, mode = 0.12, max = 0.231))
  ),

  `Escherichia coli (Non-STEC)` = list(name = "Escherichia coli (Non-STEC)",
                                       notifiable = FALSE,
                                       fractionOfGastro = rdist("rpert_alt", lowq = 0.0525, mode = 0.074, highq = 0.0914),
                                       foodborne = rdist("rpert_alt", lowq = 0.08, mode = 0.23, highq = 0.55, lowp = 0.05, highp = 0.95)
  ),

  Norovirus = list(name = "Norovirus",
                   notifiable = FALSE,
                   fractionOfGastro = rdist("rpert_alt", lowq = 0.0772, mode = 0.0982, highq = 0.1226),
                   foodborne = rdist("rpert_alt", lowq = 0.05, mode = 0.18, highq = 0.35, lowp = 0.05, highp = 0.95)
  )
)
#Toxoplasma <- list(name = "Toxoplasma gondii")

SequelaeAssumptions <- list(
  ReactiveArthritis = list(
    name = "ReactiveArthritis",
    domestic =   rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.86,       highq = 0.95,       median = 0.91),  #These are just for hospitalisations and deaths
    bacterial =  rdist("rpert_alt", lowp = 0,     highp = 1,     lowq = 0.5,        highq = 0.947,      median = 0.66),  #ditto
    #foodborne =  rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.36,       highq = 0.61,       median = 0.48),  #ditto
    gpShort =    rdist("rpert_alt",   lowq = 0.75*0.66,  highq = 0.75*0.89,  median = 0.75*0.8),
    gpLong  =    rdist("rpert_alt",   lowq = 0.25*0.66,  highq = 0.25*0.89,  median = 0.25*0.8),
    ed =         rdist("rdiscrete", x = 0), #i.e. none
    specialist = rdist("rpert_alt",   lowq = 0.223, highq = 0.258, median = 0.24),
    ongoing =    rdist("rpert_alt",   lowq = 0.23,  highq = 0.77,  median = 0.5),
    hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.5),
    hospCodes = c("M02.1", "M02.3","M02.8","M03.2"),
    mortCodes = c("M02.1","M02.8"),
    DRGCodes = list(`<5` = "I66B",
                    `5-64` = "I66B",
                    `65+` = "I66B"),
    underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
    medicationsToWhom = "GP",
    medications = list(
      AllAges = list(
        Antibiotics                   = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16 ,       highq = 0.244,       median = 0.2),
        NSAID                         = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.528,       highq = 0.918,       median = 0.762),
        Eye_drops                     = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2),
        Prednisone                    = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.01,        highq = 0.099,       median = 0.039), # The multiplier in the first draft of the report was dodgy (implied min<0 and mode<5% quantile)
        Interarticular_Glucocorticoid = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2),
        DMARD_Methotrexate            = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012 * 0.8, highq = 0.304 * 0.8, median = 0.095 * 0.8),
        DMARD_Infliximab              = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012 * 0.2, highq = 0.304 * 0.2, median = 0.095 * 0.2),
        Joint_Aspiration              = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2)
      )
    ),
    testsToWhom = "GP",
    tests = list(
      AllAges = list(
        Stool_culture               = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        Serology	                  = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        Urine_test	                = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        CRP_Urate                   = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        FBC                         = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        ESR                         = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        #EUC	                        = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244), #included in two Renal function and two bloods. Check that this is correct
        #ANA	                        = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244), #included in two Renal function and two bloods. Check that this is correct
        Rheumatoid_factor	          = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        Renal_function_two_bloods	  = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        HLA_B27	                    = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        Lumbosacral_X_ray           = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012, median=0.095, highq = 0.304),
        Lower_limb_ultrasound	      = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.017, median=0.034, highq = 0.062),
        MRI	                        = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.002, median=0.01,  highq = 0.03)
      )
    )
  ),
  IBS = list(
    name = "IBS",
    domestic =   rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.88,         highq = 0.94,         median = 0.91),  #not sure what these multipliers are for exactly...
    bacterial =  rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.06,         highq = 0.33,         median = 0.17),  #ditto
    #foodborne =  rdist("rpert_alt",   lowq = 0.08,         highq = 0.33,         median = 0.13),  #ditto
    foodborne =  rdist("rpert_alt", lowp = 0.05,  highp = 0.95,  lowq = 0.06,         highq = 0.33,         median = 0.17),
    gpShort =    rdist("rpert_alt",   lowq = 4.27 * 0.75,  highq = 4.73 * 0.75,  median = 4.5 * 0.75),
    gpLong =     rdist("rpert_alt",   lowq = 4.27 * 0.25,  highq = 4.73 * 0.25,  median = 4.5 * 0.25),
    ed =         rdist("rdiscrete", x = 0), #i.e. none
    specialist = rdist("rpert_alt",   lowq = 0.286, highq = 0.315, median = 0.3),
    ongoing =    rdist("rpert_alt",   lowq = 0.218, highq = 0.66,  median = 0.429),
    hospPrincipalDiagnosis = rdist("rdiscrete", x = 0.5),
    hospCodes = c("K58.0", "K58.9"),
    mortCodes = c("K58"),
    DRGCodes = list(`<5` = "G67B",
                    `5-64` = "G67B",
                    `65+` = "G67B"),
    underdiagnosis = rdist("rpert", min = 1, mode = 2, max = 3),
    medicationsToWhom = "Cases",
    medications = list(AllAges = list(IBSAnyMedication = rdist("rpert_alt", lowp = 0.05, highp = 0.95, lowq = 0.385, highq = 0.416, median = 0.4))),
    testsToWhom = "Cases",
    tests = list(
      AllAges = list(Stool_culture        = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     FBC                  = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     ESR                  = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     Liver_function_test  = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     CRP                  = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     Coeliac_screening    = rdist('rpert', min = 0.667, mode = 1, max = 1),
                     Abdominal_X_ray      = rdist('rpert_alt', lowp = 0.05, highp = 0.95, lowq = 0.652, median = 0.667, highq = 0.681),
                     Abdominal_Ultrasound	= rdist('rpert_alt', lowp = 0.05, highp = 0.95, lowq = 0.484, median = 0.5,   highq = 0.516)),
      `<5` =    list(Endoscopy_and_biopsy = rdist('rdiscrete', x = 0)),
      `5-64` =  list(Endoscopy_and_biopsy = rdist('rpert_alt', lowp = 0.05, highp = 0.95, lowq = 0.05, median = 0.1,    highq = 0.15)),
      `65+` =   list(Endoscopy_and_biopsy = rdist('rpert_alt', lowp = 0.05, highp = 0.95, lowq = 0.15, median = 0.2,    highq = 0.25))
    )
  )
)






