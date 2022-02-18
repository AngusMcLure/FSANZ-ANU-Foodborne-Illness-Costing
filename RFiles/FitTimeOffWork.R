library(fitdistrplus)


days <- readxl::read_xlsx("./Data/Missed work.xlsx",
                          sheet = "Missed days - machine readible") %>%
  as.data.frame()
days[days$Type == 'Self',"<5"] <- 0
days <-  days %>%
  pivot_longer(-c(Type, Days), names_to = 'AgeGroup', values_to = 'Count') %>%
  group_by(Days, Type, AgeGroup) %>%
  group_modify(~{data.frame(dummy= rep(1,.x$Count))}) %>%
  dplyr::select(-dummy)


a<- fitdist(subset(days, AgeGroup =="65+" & Type == "Self")$Days,'nbinom')
class(a)
fitdistrplus:::confint.fitdist(a)

AF:::CI.AF(a$estimate["mu"], a$sd["mu"], 0.95, 'log')
AF:::CI.AF

log(AF) + Std.Error/AF

adist <- rdist('lnorm',
               meanlog = log(a$estimate["mu"]),
               sdlog = a$sd["mu"]/a$estimate["mu"])
plot(adist)
mean(draw(adist,10000))
a$estimate["mu"]
