#Functions for extracting data
getAusPopAgeGroup <- function(){
  AusPopAgeGroup <- getAusPopSingleYearAge() %>%
    mutate(AgeGroup = ifelse(Age<5, "<5",ifelse(Age<65,"5-64", "65+"))) %>%
    group_by(Year, AgeGroup) %>%
    summarise(Persons = sum(Persons))
  AusPopAgeGroup
}

getAusPopSingleYearAge <- function(){
  AusPop <- readxl::read_xls("./Data/AustralianPopulationByAge.xls",
                                     sheet = 'Data1',
                                     range = "A1:GU59")
  AusPop <- AusPop[10:nrow(AusPop),] %>%
    rename(Year = `...1`) %>%
    pivot_longer(-Year,names_sep = ";",names_to = c(NA,'Sex','Age',NA),values_to = "Count") %>%
    mutate(Count = as.integer(Count),
           Age = trimws(Age) %>%
             recode(`100 and over` = "100") %>%
             as.integer(),
           Sex = trimws(Sex)) %>%
    mutate(Year = Year %>%
             as.integer %>%
             as.Date(origin = "1899-12-30") %>%
             lubridate::year()) %>%
    group_by(Year,Age) %>%
    summarise(Persons = sum(Count))
  AusPop
}

getAusPopAgeSex <- function(){
  AusPopAgeSex <- readxl::read_xls("./Data/AustralianPopulationByAge.xls",
                                     sheet = 'Data1',
                                     range = "A1:GU59")
  AusPopAgeSex <- AusPopAgeSex[10:nrow(AusPopAgeSex),] %>%
    rename(Year = `...1`) %>%
    pivot_longer(-Year,names_sep = ";",names_to = c(NA,'Sex','Age',NA),values_to = "Count") %>%
    mutate(Count = as.integer(Count),
           Age = trimws(Age) %>%
             recode(`100 and over` = "100") %>%
             as.integer(),
           Sex = trimws(Sex)) %>%
    mutate(Year = Year %>%
             as.integer %>%
             as.Date(origin = "1899-12-30") %>%
             lubridate::year())
    AusPopAgeSex
}



# getCasesAgeGroupOld <- function(){
#   read.csv("./Data/NationallyNotifiedDiseasesAgeGroup.csv") %>%
#     mutate(AgeGroup = recode(AgeGroup,
#                              `00-04` = '<5',
#                              `05-09` = '5-64',
#                              `10-14` = '5-64',
#                              `15-19` = '5-64',
#                              `20-24` = '5-64',
#                              `25-29` = '5-64',
#                              `30-34` = '5-64',
#                              `35-39` = '5-64',
#                              `40-44` = '5-64',
#                              `45-49` = '5-64',
#                              `50-54` = '5-64',
#                              `55-59` = '5-64',
#                              `60-64` = '5-64',
#                              `65-69` = '65+',
#                              `70-74` = '65+',
#                              `75-79` = '65+',
#                              `80-84` = '65+',
#                              `85+`   = '65+')) %>%
#     subset(AgeGroup != "Unknown") %>% ## Perhaps those of unknown age should go with '5-64' rather than being excluded?
#     group_by(Year, Disease, AgeGroup) %>%
#     summarise(Cases = sum(Cases))
# }

getCasesAgeGroup <- function(){
  readxl::read_xlsx("./Data/NNDSS Disease by AgeGroup and Year.xlsx") %>%
    pivot_longer(`0 - 4`:`85+`, names_to = 'AgeGroup', values_to = "Cases") %>%
    mutate(AgeGroup = recode(AgeGroup,
                             `0 - 4` = '<5',
                             `5 - 9` = '5-64',
                             `10 - 14` = '5-64',
                             `15 - 19` = '5-64',
                             `20 - 24` = '5-64',
                             `25 - 29` = '5-64',
                             `30 - 34` = '5-64',
                             `35 - 39` = '5-64',
                             `40 - 44` = '5-64',
                             `45 - 49` = '5-64',
                             `50 - 54` = '5-64',
                             `55 - 59` = '5-64',
                             `60 - 64` = '5-64',
                             `65 - 69` = '65+',
                             `70 - 74` = '65+',
                             `75 - 79` = '65+',
                             `80 - 84` = '65+',
                             `85+`   = '65+')) %>%
    rename(Disease = `Disease Name`) %>%
    group_by(Year, Disease, AgeGroup) %>%
    summarise(Cases = sum(Cases))
}

getHospitalisationsAgeGroup <- function(){
  HospFiles <- list.files("./Data", "Principal_diagnosis_data_cube_")
  Years <- sub("\\.xlsx.*", "", sub(".*Principal_diagnosis_data_cube_", "", HospFiles))
  out <- map(HospFiles,function(x){
    readxl::read_xlsx(paste0("./Data/",x),
                      sheet = '5-character PDx Counts Data',
                      range = readxl::cell_limits(c(5, 1), c(NA, NA))) %>%
      mutate(DC3D = substr(`3 digit diagnosis`, start = 1, stop = 3),
             DC4D = substr(`4 digit diagnosis`, start = 1, stop = 5)) %>%
      mutate(DC3D = ifelse(grepl('[A-Z][0-9]{2}',DC4D),DC3D,NA),
             DC4D = ifelse(grepl('[A-Z][0-9]{2}\\.[0-9]',DC4D),DC4D,NA),
             AgeGroup = as.integer(substr(`Age Group`,start = 1, stop = 2)),
             AgeGroup = ifelse(AgeGroup<=2,'<5',
                               ifelse(AgeGroup <= 14, '5-64',
                                      ifelse(AgeGroup <= 19, '65+',NA)))) %>%
      group_by(DC3D, DC4D, AgeGroup) %>%
      summarise(Separations  = sum(Separations),
                PatientDays = sum(`Patient Days`))})
  names(out) <- Years
  bind_rows(out, .id = "Year") %>%
    mutate(FYNumeric = 2000 + as.integer(substr(Year, 6, 7))) %>%
    mutate(meanLOS = PatientDays/Separations)
}

getCosts <- function(){
  out <- readxl::read_xlsx("./Data/Costs.xlsx") %>%
    as.data.frame()
  rownames(out) <- out$Name
  out
}

getWTP <- function(){
  out <- readxl::read_xlsx("./Data/WTPvalues.xlsx") %>%
    as.data.frame()
  rownames(out) <- out$Name
  out
}

getValueStatisticalLife <- function(){
  read.csv('./Data/ValueStatisticalLife.csv')$Value
}

getABSDeaths <- function(year){
  out <- readxl::read_xlsx("./Data/ABSDeathTable2001-2010.xlsx",
                           sheet = "Table 2",
                           range = "A9:K69",
                           col_types = c("text",
                                         "skip",
                                         "text",
                                         "text",
                                         "text",
                                         "text",
                                         "skip",
                                         "text",
                                         "text",
                                         "text",
                                         "text"),
                           col_names = c("Cause",
                                         "Male.0-14",
                                         "Male.15-64",
                                         "Male.65+",
                                         "Male.All",
                                         "Female.0-14",
                                         "Female.15-64",
                                         "Female.65+",
                                         "Female.All"
                           )) %>%
    as.data.frame() %>%
    pivot_longer(-Cause, names_to = c("Sex", "AgeGroup"), names_sep = "\\.") %>%
    mutate(value = value %>%
             na_if('np') %>%
             recode(`–` = "0") %>%
             as.numeric,
           Cause = Cause %>% str_extract('^([^\\s]+)'),
           Cause = if_else(nchar(Cause)>3,
                           paste(substr(Cause, 1,3), substr(Cause, 4,nchar(Cause)), sep ='.'),
                           Cause)
    )

  #Fix the missing deaths for some of the females. All Causes have a total
  #given, but there are no estimates from some age groups I am using the
  #difference between the total and the age groups with deaths given to estimate
  #the totals across the age groups with missing values and the apportioning the
  #deaths across the age groups according to size of the population in those age
  #groups

  AusPop <- getAusPopAgeSex()

  AgeGroupWeights <- AusPop %>%
    subset(Year %in% 2001:2010) %>%
    mutate(AgeGroup = ifelse(Age<15, "0-14",ifelse(Age<65,"15-64", "65+"))) %>%
    group_by(Sex, AgeGroup) %>%
    summarise(Count = sum(Count))

  for(n in 1:nrow(out)){
    r <- out[n,]
    if(is.na(out$value[n])){
      deficit <- subset(out, Cause == r$Cause & Sex == r$Sex & AgeGroup == 'All')$value -
        sum(subset(out, Cause == r$Cause & Sex == r$Sex & AgeGroup != 'All')$value, na.rm = T)
      out[n,'value'] <- deficit * subset(AgeGroupWeights, Sex == r$Sex & AgeGroup == r$AgeGroup)$Count/
        sum(subset(AgeGroupWeights, Sex == r$Sex & AgeGroup %in% subset(out, Cause == r$Cause & Sex == r$Sex & is.na(value))$AgeGroup)$Count)
    }
  }

  #Collapse by sex and remove totals

  out <- out %>% subset(AgeGroup != "All") %>%
    group_by(Cause, AgeGroup) %>%
    summarise(Count = sum(value))

  #Readjust to agegroups <5, 5-64, and 65+ -- assume rate in <5 is the same as rate in
  Frac0to14Under5 <- sum(subset(AusPop, Year %in% 2001:2010 & Age<5)$Count)/
    sum(subset(AusPop, Year %in% 2001:2010 & Age<15)$Count)

  out <- bind_rows(out %>%
                     mutate(AgeGroup = recode(AgeGroup,
                                              `0-14` = "<5",
                                              `15-64` = "5-64"),
                            Count = ifelse(AgeGroup == "<5",
                                           Count * Frac0to14Under5,
                                           Count)),
                   out %>%
                     subset(AgeGroup == "0-14") %>%
                     mutate(Count = Count * (1-Frac0to14Under5),
                            AgeGroup = "5-64")) %>%
    group_by(Cause,AgeGroup) %>%
    summarise(Count = sum(Count))

  PopFinalAgeGroups <- AusPop %>% subset(Year %in% 2001:2010) %>%
    mutate(AgeGroup = ifelse(Age<5, "<5",ifelse(Age<65,"5-64", "65+"))) %>%
    group_by(AgeGroup) %>%
    summarise(PersonYears = sum(Count))

  # Calculate as rate per person per year
  out <- merge(out, PopFinalAgeGroups, by = "AgeGroup") %>%
    mutate(Rate = Count/PersonYears)

  return(out)
}


getMissedDaysGastro <- function(){
  days <- readxl::read_xlsx("./Data/Missed work.xlsx",
                           sheet = "Missed days - machine readible") %>%
    as.data.frame()
  days[days$Type == 'Self',"<5"] <- 0
  days <-  days %>%
    pivot_longer(-c(Type, Days), names_to = 'AgeGroup', values_to = 'Count') %>%
    group_by(Days, Type, AgeGroup) %>%
    group_modify(~{data.frame(dummy= rep(1,.x$Count))}) %>%
    select(-dummy)

  fits <- days %>%
    subset(!(AgeGroup == "<5" & Type == "Self")) %>%
    group_by(AgeGroup, Type) %>%
    group_modify(~{
      invisible(capture.output(a <- fitdistrplus::fitdist(.x$Days,'nbinom'))) #invisible(capture.output(...)) is used to suppress the unnecessary printing to screen
      #plot(a)
      as.data.frame(as.list(c(estimate = a$estimate["mu"], sd = a$sd["mu"])))})
  rbind(fits, data.frame(AgeGroup = '<5', Type = "Self", estimate.mu = 0, sd.mu = 0))
}

# getMissedDaysHospitalisation <- function(){
#   HospFiles <- list.files("./Data", "Principal_diagnosis_data_cube_")
#   Years <- sub("\\.xlsx.*", "", sub(".*Principal_diagnosis_data_cube_", "", HospFiles))
#   out <- map(HospFiles,function(x){
#     readxl::read_xlsx(paste0("./Data/",x),
#                       sheet = '5-character PDx Counts Data',
#                       range = readxl::cell_limits(c(5, 1), c(NA, NA))) %>%
#       mutate(DC3D = substr(`3 digit diagnosis`, start = 1, stop = 3),
#              DC4D = substr(`4 digit diagnosis`, start = 1, stop = 5)) %>%
#       mutate(DC3D = ifelse(grepl('[A-Z][0-9]{2}',DC4D),DC3D,NA),
#              DC4D = ifelse(grepl('[A-Z][0-9]{2}\\.[0-9]',DC4D),DC4D,NA),
#              AgeGroup = as.integer(substr(`Age Group`,start = 1, stop = 2)),
#              AgeGroup = ifelse(AgeGroup<=2,'<5',
#                                ifelse(AgeGroup <= 14, '5-64',
#                                       ifelse(AgeGroup <= 19, '65+',NA)))) %>%
#       group_by(DC3D, DC4D, AgeGroup)})
#   names(out) <- Years
#   out <- bind_rows(out, .id = "Year") %>%
#     mutate(FYNumeric = 2000 + as.integer(substr(Year, 6, 7)))
#
#   fits <- days %>%
#     subset(!(AgeGroup == "<5" & Type == "Self")) %>%
#     group_by(AgeGroup, Type) %>%
#     group_modify(~{
#       invisible(capture.output(a <- fitdistrplus::fitdist(.x$Days,'nbinom'))) #invisible(capture.output(...)) is used to suppress the unnecessary printing to screen
#       #plot(a)
#       as.data.frame(as.list(c(estimate = a$estimate["mu"], sd = a$sd["mu"])))})
#   rbind(fits, data.frame(AgeGroup = '<5', Type = "Self", estimate.mu = 0, sd.mu = 0))
# }


getWorkforceAssumptions <- function(){
  out <- read.csv("./Data/WorkforceAssumptions.csv")
  rownames(out) <- out$AgeGroup
  out
}

getFrictionRates <- function(){
  read.csv("./Data/Friction.csv")
}

