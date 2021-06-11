#Functions for extracting data
getAusPopAgeGroup <- function(){
  AusPopAgegroup <- readxl::read_xls("./Data/AustralianPopulationByAge.xls",
                                     sheet = 'Data1',
                                     range = "A1:GU59")
  AusPopAgegroup <- AusPopAgegroup[10:nrow(AusPopAgegroup),] %>%
    rename(Year = `...1`) %>%
    pivot_longer(-Year,names_sep = ";",names_to = c(NA,'Sex','Age',NA),values_to = "Count") %>%
    mutate(Count = as.integer(Count),
           Age = trimws(Age) %>%
             recode(`100 and over` = "100") %>%
             as.integer(),
           Sex = trimws(Sex)) %>%
    mutate(AgeGroup = ifelse(Age<5, "<5",ifelse(Age<65,"5-64", "65+")),
           Year = Year %>%
             as.integer %>%
             as.Date(origin = "1899-12-30") %>%
             lubridate::year()) %>%
    group_by(Year, AgeGroup) %>%
    summarise(Persons = sum(Count))
  AusPopAgegroup
}

getCasesAgegroup <- function(){
  read.csv("./Data/NationallyNotifiedDiseasesAgegroup.csv") %>%
    mutate(AgeGroup = recode(AgeGroup,
                             `00-04` = '<5',
                             `05-09` = '5-64',
                             `10-14` = '5-64',
                             `15-19` = '5-64',
                             `20-24` = '5-64',
                             `25-29` = '5-64',
                             `30-34` = '5-64',
                             `35-39` = '5-64',
                             `40-44` = '5-64',
                             `45-49` = '5-64',
                             `50-54` = '5-64',
                             `55-59` = '5-64',
                             `60-64` = '5-64',
                             `65-69` = '65+',
                             `70-74` = '65+',
                             `75-79` = '65+',
                             `80-84` = '65+',
                             `85+`   = '65+')) %>%
    subset(AgeGroup != "Unknown") %>% ## Perhaps those of unknown age should go with '5-64' rather than being excluded?
    group_by(Year, Disease, AgeGroup) %>%
    summarise(Cases = sum(Cases))
}

getHospitalisationsAgegroup <- function(Codes){
  readxl::read_xls("./Data/aihw-web-216-Principal-Diagnosis-Cube-2018-19.xlsx")

  %>%
    mutate(AgeGroup = recode(AgeGroup,
                             `00-04` = '<5',
                             `05-09` = '5-64',
                             `10-14` = '5-64',
                             `15-19` = '5-64',
                             `20-24` = '5-64',
                             `25-29` = '5-64',
                             `30-34` = '5-64',
                             `35-39` = '5-64',
                             `40-44` = '5-64',
                             `45-49` = '5-64',
                             `50-54` = '5-64',
                             `55-59` = '5-64',
                             `60-64` = '5-64',
                             `65-69` = '65+',
                             `70-74` = '65+',
                             `75-79` = '65+',
                             `80-84` = '65+',
                             `85+` = '65+')) %>%
    subset(AgeGroup != "Unknown") %>%
    group_by(Year, Disease,AgeGroup) %>%
    summarise(Cases = sum(Cases))
}




