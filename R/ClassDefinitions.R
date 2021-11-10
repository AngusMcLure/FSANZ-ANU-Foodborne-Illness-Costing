#class definitions


# functions for building distribution object
rdist <- function(type,..., continuous = TRUE){
  # continuous attribute indicates whether the associated functions d<type>
  # function should be interpreted as a probability probability density function
  # (default) or a probability mass function
  params <- list(...)
  nms <- names(params)
  if(!length(params)){
    warning('Distribution has been provided no parameters. This will use the default parameters for ',type,' which is rarely what is wanted' )
  } else if(is.null(nms) || any(nms == "")){
    stop("All distribution parameters for 'rdist' must be named")
  }
  if(length(unique(nms)) < length(nms))stop("Distrtribution parameter names for 'rdist' must be unique")
  if("n" %in% nms)stop("'n' is not an allowable parameter name for an 'rdist' object")
  if(class(type) != "character")stop("'type' must be the name of a distribution, e.g. norm (normal distribution) or pert (PERT distribution) ")
  structure(list(type = type, params = list(...)), class = "rdist", continuous = continuous)
}

# functions for sampling from a distribution object
draw <- function(distribution, n){
  UseMethod("draw")
}

draw.numeric <- function(x,n){
  rdiscrete(n,x)
}

draw.rdist <- function(distribution, n){
  do.call(paste0('r',distribution$type), c(n = n, distribution$params))
}

plot.rdist <- function(x,from = 0, to = 1, kind = "density", ...){
  f <- switch(kind,
    cummulative = get(paste0("p",x$type)),
    density = get(paste0("d",x$type))
  )
  if(kind == 'density' & !attr(x,"continuous")){
    warning('Density plots for discrete distributions do not work very well')
  }

  plot(function(.x){do.call(f, c(list(x = .x), x$params))}, from = from, to = to,...)
}

#tests for rdist objects
is.rdist <- function(x){class(x) == "rdist"}

`<=.rdist` <- function(distribution, x){
  do.call(paste0('p',distribution$type), c(x,distribution$params)) == 1
}

`>.rdist` <- function(distribution, x){
  (1 - do.call(paste0('p',distribution$type), c(x,distribution$params))) == 1
}

`>=.rdist` <- function(distribution, x){
  if(attr(distribution, "continuous", exact = TRUE)){
    mass <- 0
  } else{
    mass <- do.call(paste0('d',distribution$type), c(x,distribution$params))
  }
  (1 - do.call(paste0('p',distribution$type), c(x,distribution$params)) + mass) == 1
}

`<.rdist` <- function(distribution, x){
  if(attr(distribution, "continuous", exact = TRUE)){
    mass <- 0
  } else{
    mass <- do.call(paste0('d',distribution$type), c(x,distribution$params))
  }
  (do.call(paste0('p',distribution$type), c(x,distribution$params)) - mass) == 1
}

alert_bounds <- function(x, lower = 0, upper = NULL, name = NULL){
  if(is.null(name)){
    name <- substitute(x)
  }
  if(!is.null(lower) && !is.null(upper)){
    if(!(x>=lower) || !(x<=upper)){
    stop(name, ' must be between ', lower, ' and ', upper)
    }
  } else if(!is.null(lower)){
    if(!(x>=lower)){
      stop(name, ' must be greater than or equal to ', lower)
    }
  }else if(!is.null(upper)){
    if(!(x<=upper)){
      stop(name, ' must be less than or equal to ', upper)
    }
  }
}

# functions for building disease objects
disease <- function(
  name, kind, caseMethod, correction = NULL, domestic = NULL, underreporting = NULL,
  symptomatic = NULL, FOI = NULL, gastroFraction = NULL,
  foodborne, gp, gpFracLong, ed, sequelae = NULL,
  hospPrincipalDiagnosis = NULL, hospMethod, hospCodes = NULL, mortCodes, DRGCodes,
  underdiagnosis, medications, medicationsToWhom, tests,
  testsToWhom, bacterial = NULL,
  specialist, specialistToWhom, physio = NULL,
  propOngoing = NULL, durationOngoing = NULL, propSevere = NULL,
  duration = NULL, severity = NULL, symptoms,
  missedWorkCarer = NULL, missedWorkSelf = NULL
 ){

  out <- list(
    name = name, kind = kind, caseMethod = caseMethod, correction = correction,
    domestic = domestic, underreporting = underreporting, symptomatic = symptomatic,
    FOI = FOI, gastroFraction = gastroFraction, foodborne = foodborne,
    gp = gp, gpFracLong = gpFracLong, ed = ed, sequelae = sequelae,
    hospPrincipalDiagnosis = hospPrincipalDiagnosis, hospMethod = hospMethod,
    hospCodes = hospCodes, mortCodes = mortCodes, DRGCodes = DRGCodes,
    underdiagnosis = underdiagnosis, medications = medications,
    medicationsToWhom = medicationsToWhom, tests = tests,
    testsToWhom = testsToWhom, bacterial = bacterial,
    specialist = specialist, specialistToWhom = specialistToWhom,
    physio = physio,
    propOngoing = propOngoing, durationOngoing = durationOngoing,
    propSevere = propSevere,
    duration = duration, severity = severity, symptoms = symptoms,
    missedWorkCarer = missedWorkCarer, missedWorkSelf = missedWorkSelf
  )
  #out <- as.list(environment())
  out <- out[!unlist(lapply(out,is.null))] # drop all NULL arguments


  AllowedKinds <- c("sequel", "initial")
  if(!(kind %in% AllowedKinds)){
    stop("kind is listed as ", kind, ". Allowable kinds are: ", paste0(AllowedKinds, collapse = ", "))
  }

  ArgumentsKind <- list(initial = c('correction', 'sequelae',
                                          'duration', 'severity'),
                              sequel = c('propOngoing',
                                         'durationOngoing','propSevere',
                                         'missedWorkCarer','missedWorkSelf'))

  if(!all(ArgumentsKind[[kind]] %in% names(out))){
    stop(paste(setdiff(ArgumentsKind[[kind]],names(out)), collapse = ", "), ' are required arguments for ', kind, ' diseases')
  }

  # valid case estimation methods
  AllowedCaseMethods <- c('NNDSS','sequel','GastroFraction', 'Seroprevalence')
  if(!(caseMethod %in% AllowedCaseMethods)){stop('caseMethod must be one of: ', paste(AllowedCaseMethods, collapse = ", "))}

  ArgumentsCaseMethod <- list(GastroFraction = c('gastroFraction'),
                              NNDSS = c('domestic', 'underreporting'),
                              Seroprevalence = c('domestic','symptomatic'),
                              sequel = c('domestic'))
  if(!all(ArgumentsCaseMethod[[caseMethod]] %in% names(out))){
    stop(paste(setdiff(ArgumentsCaseMethod[[caseMethod]],names(out)), collapse = ", "), ' are required arguments for diseases where cases are estimated using the ', caseMethod, 'method.')
  }


  #Check hospMethod
  AllowedHospMethods <- c("AIHW", "AllCases")
  if(!(hospMethod %in% AllowedHospMethods)){
    stop(hospMethod, " is not a valid value for argument hospMethod.",
         "There are two options for estimating hospitalisations: ",
         paste0(AllowedHospMethods, collapse = ", "), ".")
  }

  ArgumentsAIHW <- c("hospPrincipalDiagnosis", "hospCodes")
  if(hospMethod == "AIHW" && !all(ArgumentsAIHW %in% names(out))){
    stop(paste(ArgumentsAIHW, collapse = ", "), ' are required arguments for diseases where hospitalisations estimates with from AIHW data')
  }

  # check that input classes are correct
  ArgsCorrectClasses <- list(name = "character",
                             kind = "character",
                             caseMethod = 'character',
                             correction = 'numeric',
                             domestic = 'rdist',
                             underreporting = 'rdist',
                             symptomatic = 'rdist',
                             FOI = 'rdist',
                             gastroFraction = 'rdist',
                             foodborne = 'rdist',
                             bacterial = 'rdist',
                             gp = 'rdist',
                             gpFracLong = 'numeric',
                             ed = 'rdist',
                             specialist = 'rdist',
                             specialistToWhom = 'character',
                             physio = 'rdist',
                             ongoing = 'list',
                             sequelae = 'list',
                             hospPrincipalDiagnosis = 'rdist',
                             hospMethod = "character",
                             hospCodes = 'character',
                             mortCodes = 'character',
                             DRGCodes = 'list',
                             underdiagnosis = 'rdist',
                             medications = 'list',
                             medicationsToWhom = 'character',
                             tests = 'list',
                             testsToWhom = 'character',
                             symptoms = 'character',
                             severity = 'character',
                             duration = 'numeric',
                             propOngoing = 'list',
                             durationOngoing = 'numeric',
                             propSevere = 'numeric',
                             missedWorkCarer = 'list',
                             missedWorkSelf = 'list')

  ArgsCorrectClasses <- ArgsCorrectClasses[names(out)]

  purrr::pmap(list(out, ArgsCorrectClasses, names(out)),
              ~alert_type_error(..1, class_name = ..2, x_name = ..3 ))


  AllowedMedsTestsMethods <- c('GP', 'Cases','Notifications','None')
  if(!(testsToWhom %in% AllowedMedsTestsMethods)){stop('testsToWhom  must be one of: ', paste(AllowedMedsTestsMethods, collapse = ", "))}
  if(!(medicationsToWhom %in% AllowedMedsTestsMethods)){stop('medicationsToWhom  must be one of: ', paste(AllowedMedsTestsMethods, collapse = ", "))}

  AllowedSpecialistMethods <- c('Cases','Hospitalisations','None')
  if(!(specialistToWhom %in% AllowedSpecialistMethods)){stop('testsToWhom  must be one of: ', paste(AllowedMedsTestsMethods, collapse = ", "))}


  # check that input values are valid
  # domestic, foodborne, gastroFraction, symptomatic must be a distribution with support on a subset of [0,1]

  chkargs <- setdiff(c(ArgumentsCaseMethod[[caseMethod]], 'foodborne'), 'underreporting')
  imap(out[chkargs], ~alert_bounds(.x, lower = 0, upper = 1,name = .y))

  # under-reporting must be positive distribution


  # gp, ed, specialist, physio must be distributions of non-negative random variables
  # gpFracLong needs to be 0 and 1
  alert_bounds(gp)
  alert_bounds(gpFracLong, upper = 1)
  alert_bounds(ed)
  alert_bounds(specialist)
  if(!is.null(physio)){
    alert_bounds(physio)
  }


  # underdiagnosis and underreporting must be a distribution for variables >= 1
  alert_bounds(underdiagnosis, lower = 1, name = paste("underdiagnosis for disease ", name))
  if(!is.null(underreporting)) alert_bounds(underreporting)


  # sequelae must be a named list of rdist objects, each with support on [0,1]
  if(kind == 'initial'){
    alert_wellnamed(sequelae)
    purrr::imap(sequelae, ~alert_type_error(.x, class_name = "rdist", x_name = paste('sequel illness, ', .y , ' for disease ', name)))
    purrr::imap(sequelae, ~alert_bounds(.x, lower = 0, upper = 1, name = paste(.y, "multiplier for disease ", name)))
  }

  # medications, tests must be a named list of named list of rdist objects.
  # Each of the top level dist must have names of age groups. Each of the rdist
  # objects must be non-negative random variables

  checkMedTests <- function(l, upper = NULL){
    nm <- substitute(l)
    alert_wellnamed(l)
    if(!all(names(l) %in% c("<5", "5-64", "65+", "AllAges"))){
      stop(nm, ' must be defined for age groups, <5, 5-64, 65+, or AllAges')
    }
    purrr::imap(l, ~alert_wellnamed(.x,name = paste(nm, " for age group ",.y, " and disease ", name)))
    purrr::imap(l, function(x,y){
      purrr::imap(x, ~{
        alert_type_error(.x, class_name = "rdist", x_name = paste0("the mulitplier for ", .y, ' for ', name ,' in age group ', y))
        alert_bounds(.x, upper = upper, name = paste0("the mulitplier for ", .y, ' for ', name ,' in age group ', y))
      })
    })
  }
  checkMedTests(tests)
  checkMedTests(medications)

  checkAgeNames<- function(l){
    nm <- substitute(l)
    if(!setequal(names(l),c("<5", "5-64", "65+")) && !setequal(names(l),c("AllAges"))){
      print(names(l))
      stop(nm, ' must be a list defined for age groups, <5, 5-64, and 65+, or just for "AllAges"')
    }
  }

  checkAgeNames(DRGCodes)
  purrr::imap(DRGCodes,~alert_type_error(.x, class_name = "character",
                                         x_name = paste0("the DRG code in age group ", .y)))
  if(kind == "sequel"){
    checkAgeNames(missedWorkCarer)
    purrr::imap(missedWorkCarer, ~alert_type_error(.x, class_name = "numeric",
                                           x_name = paste0("the number of carer days in age group ", .y)))
    purrr::imap(missedWorkCarer, ~alert_bounds(.x, lower = 0, name = paste(.y, "number of carer days", name)))

    checkAgeNames(missedWorkSelf)
    purrr::imap(missedWorkCarer, ~alert_type_error(.x, class_name = "numeric",
                                           x_name = paste0("the number of days off work in age group ", .y)))
    purrr::imap(missedWorkCarer, ~alert_bounds(.x, lower = 0, name = paste(.y, "number of days off work", name)))


    if(propSevere < 0 || propSevere > 1) stop('propSevere needs to be between 0 and 1')
    if(durationOngoing < 0 ) stop('durationOngoing needs to be greater than or equal to 0 (in years)')
    if(durationOngoing > 100 ) warning('durationOngoing is measured in years so a value of ', durationOngoing, ' is not reasonable')
    checkAgeNames(propOngoing)
    purrr::imap(propOngoing,~alert_bounds(.x, upper = 1, name = paste0("the proportion ongoing illness in age group ", .y)))
  }else{
    if(!setequal(names(duration), c("Hosp", "NonHosp"))){
      stop('duration must be a named vector of severities for the disease, with named entries for Hosp (hospitalised cases) and NonHosp (non-hospitalised cases)')
    }
    if(any(duration < 0)){
      stop('disease durations must be positive')
    }

    if(!setequal(names(severity), c("Hosp", "NonHosp"))){
      stop('severity must be a named vector of severities for the disease, with named entries for Hosp (hospitalised cases) and NonHosp (non-hospitalised cases)')
    }
    if(any(!(severity %in% c("severe", "mild")))){
      stop('severity must be "severe" or "mild"')
    }
  }
  structure(out, class = c(kind, 'disease'))
}

alert_wellnamed <- function(x,...){
  UseMethod("alert_wellnamed")
}

alert_wellnamed.list <- function(x, name = NULL){
  if(is.null(name)){
    name <- substitute(x)
  }
  nms <- names(x)
  if("" %in% nms){
    stop(name, ' has un-named elements')
  }
  if(length(unique(nms)) < length(nms)){
    stop(name, ' has non-unique names')
  }
}

alert_type_error <- function(x, test = NULL, class_name =  NULL, x_name = NULL){
  if(is.null(x_name)){
    x_name <- substitute(x)
  }
  if(is.null(class_name) && substring(substitute(test), first = 1, last = 3) == "is."){
    class_name <- substring(substitute(test), first = 4)
    if(!test(x)) stop(x_name,' must be a ', class_name, ', not an object of class ', class(x))
  } else if(is.null(test)){
    if(!is(x,class_name)){
      stop(x_name,' must be a ', class_name, ', not an object of class ', class(x))
    }
  }
}
