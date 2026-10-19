
getCPI <- function(ReferencePoint){
  read.csv('Data/CPI-ABS.csv',skip = 1,
           col.names = c('Time.Point', 'Change.Previous.Point')) %>%
    drop_na() %>%
    mutate(Date = as_date(paste0('01-',Time.Point),format = '%d-%m-%y')) %>%
    subset(Date > as_date(paste0('01-',ReferencePoint),format = '%d-%m-%y')) %>%
    arrange(Date) %>%
    mutate(Change.Previous.Point = as.numeric(Change.Previous.Point),
           Cumm.Inflation.Multiplier = cumprod(1+Change.Previous.Point/100),
           CummCPI = 100*(Cumm.Inflation.Multiplier-1)) %>%
    `row.names<-`(.$Time.Point)
}
