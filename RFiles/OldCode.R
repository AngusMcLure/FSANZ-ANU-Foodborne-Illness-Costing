#
# estimateGP <- function(disease, ndraws = 10^6, incidence = NULL){
#   #This estimates hospitalisations for a specific disease in a specific year
#   #abbreviate
#   v <- disease[c("gpShort","gpLong")]
#   n <- ndraws
#   if(is.null(incidence)){
#     incidence = 1
#     warning("Case numbers of underlying cause are not provided, returning multipliers per case")
#   }
#   if(length(incidence) != 1 & length(incidence) != n){
#     stop('The length of incidence should either be 1 or equal to ndraws')
#   }
#
#   map(v, function(x){draw(x, n) * incidence})
# }
#
# estimateED <- function(disease, ndraws = 10^6, incidence = NULL){
#   #This estimates hospitalisations for a specific disease in a specific year
#   #abbreviate
#   ed <- disease$ed
#   n <- ndraws
#   if(is.null(incidence)){
#     incidence = 1
#     warning("Case numbers of underlying cause are not provided, returning multipliers per case")
#   }
#   if(length(incidence) != 1 & length(incidence) != n){
#     stop('The length of incidence should either be 1 or equal to ndraws')
#   }
#
#   list(ed = draw(ed, n) * incidence)
# }
#
# estimateSequelae <- function(disease, ndraws = 10^6, incidence = NULL){
#   #This estimates hospitalisations for a specific disease in a specific year
#   #abbreviate
#   n <- ndraws
#   s <- disease$sequelae
#   if(is.null(incidence)){
#     incidence = 1
#     warning("Case numbers of underlying cause are not provided, returning multipliers per case")
#   }
#   if(length(incidence) != 1 & length(incidence) != n){
#     stop('The length of incidence should either be 1 or equal to ndraws')
#   }
#
#   map(s, function(x){draw(x, n) * incidence})
# }
#
# estimateMeds <- function(disease, ageGroup, ndraws = 10^6, incidence = NULL){
#   #This estimates hospitalisations for a specific disease in a specific year
#   #abbreviate
#   n <- ndraws
#   d <- disease
#   m <- c(d$medications[[ageGroup]],d$medications[["AllAges"]])
#
#   if(is.null(incidence)){
#     incidence = 1
#     warning("Case numbers of underlying cause are not provided, returning multipliers per case")
#   }
#   if(length(incidence) != 1 & length(incidence) != n){
#     stop('The length of incidence should either be 1 or equal to ndraws')
#   }
#
#   map(m, function(x){draw(x, n) * incidence})
# }
#
# estimateTests <- function(disease, ageGroup, ndraws = 10^6, incidence = NULL){
#   #This estimates hospitalisations for a specific disease in a specific year
#   #abbreviate
#   n <- ndraws
#   d <- disease
#   t <- c(d$tests[[ageGroup]],d$tests[["AllAges"]])
#
#   if(is.null(incidence)){
#     incidence = 1
#     warning("Case numbers of underlying cause are not provided, returning multipliers per case")
#   }
#   if(length(incidence) != 1 & length(incidence) != n){
#     stop('The length of incidence should either be 1 or equal to ndraws')
#   }
#
#   map(t, function(x){draw(x, n) * incidence})
# }
