warning('assumes that ndraws is the same for this and for HospList, DeathsList, and IncidenceList')
warning('Year 2019 is used to estimate LOS -- should there be an option for year or an options to use all data available?')
costOutbreak <- function(pathogen,
                         notifications,
                         cases = NULL,
                         separations = NULL,
                         deaths = NULL,
                         ndraws = 10^3){
  print(ndraws)
  ageGroups = c("<5","5-64","65+")
  names(ageGroups) <- ageGroups
  if(is.null(cases)){
    cases <- rep(list(NULL), 1 + length(pathogen$sequelae))
    names(cases) <- c(pathogen$name, names(pathogen$sequelae))
  }
  if(is.null(separations)){
    separations <- rep(list(NULL), 1 + length(pathogen$sequelae))
    names(separations) <- c(pathogen$name, names(pathogen$sequelae))
  }
  if(is.null(deaths)){
    deaths <- rep(list(NULL), 1 + length(pathogen$sequelae))
    names(deaths) <- c(pathogen$name, names(pathogen$sequelae))
  }

  if(is.null(cases[[pathogen$name]])){
    if(is.null(notifications)) stop('need notification data if number of cases not provided')
    cases[[pathogen$name]] <- estimateIncidence(pathogen = pathogen,
                                                ndraws = ndraws,
                                                notifications = notifications)
  }
  print("C")
  needsSequelaeCalc <- unlist(imap(pathogen$sequelae, ~is.null(cases[[.y]])))
  sequelae <- estimateSequelae(pathogen, cases[[pathogen$name]], ndraws = ndraws)
  if(!is.null(needsSequelaeCalc)){
    cases <- c(cases[c(TRUE,!needsSequelaeCalc)],sequelae[needsSequelaeCalc])
  }
  print("D")

  separations <- imap(cases,
                      function(.c,.dn){
                        if(is.null(separations[[.dn]])){
                          map(ageGroups,
                              function(.a){
                                HospList[[pathogen$pathogen]][[.dn]][[.a]]/
                                  IncidenceList[[pathogen$pathogen]][[.dn]][[.a]] *
                                  .c[[.a]]})
                        }else{separations[[.dn]]}
                      })
  print("E")
  deaths <- imap(cases,
                 function(.c,.dn){
                   if(is.null(deaths[[.dn]])){
                     map(ageGroups,
                         function(.a){DeathList[[pathogen$pathogen]][[.dn]][[.a]]/
                             IncidenceList[[pathogen$pathogen]][[.dn]][[.a]] *
                             .c[[.a]]})
                   }else{deaths[[.dn]]}
                 })
  print(deaths)

  print("F")
  costs <- estimateCosts(pathogen,
                         ndraws = ndraws,
                         cases = cases,
                         notifications = notifications,
                         separations = separations,
                         deaths = deaths,
                         year = 2019, #Year is used to estimate LOS -- should there be an option for NA or ALL which uses all data available?
                         discount = 0) #We are not even using discounting any more so I might drop this argument all together?
  #out <- list(cases = cases, deaths = deaths, separations = separations, costs = costs)
  print("G")
  costs
}

