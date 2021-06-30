#Functions for extracting data
getAusPopAgeGroup <- function(){
  AusPopAgeGroup <- readxl::read_xls("./Data/AustralianPopulationByAge.xls",
                                     sheet = 'Data1',
                                     range = "A1:GU59")
  AusPopAgeGroup <- AusPopAgeGroup[10:nrow(AusPopAgeGroup),] %>%
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
  AusPopAgeGroup
}

getCasesAgeGroup <- function(){
  read.csv("./Data/NationallyNotifiedDiseasesAgeGroup.csv") %>%
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
    mutate(FYNumeric = 2000 + as.integer(substr(Year, 6, 7)))
}

getCosts <- function(){
  out <- readxl::read_xlsx("./Data/Costs.xlsx") %>%
    as.data.frame()
  rownames(out) <- out$Name
  out
}



