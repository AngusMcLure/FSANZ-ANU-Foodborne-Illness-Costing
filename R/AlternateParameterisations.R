# function for building distribution object
rdist <- function(type,...){
  list(type = type, params = list(...))
}

# functions for sampling from a distribution object
draw <- function(distribution, n){
  do.call(distribution$type, c(n = n, distribution$params))
}

# Alternate parameterisations of various distributions

## PERT
ppert_alt <- function (q, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.05, highp = 0.95, median = NULL,
                       shape = 4, lower.tail = TRUE,
                       log.p = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  ppert(q, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, lower.tail = lower.tail,
        log.p = log.p)
}

qpert_alt <- function (p, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.05, highp = 0.95, median = NULL,
                       shape = 4, lower.tail = TRUE,
                       log.p = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  qpert(p, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, lower.tail = lower.tail,
        log.p = log.p)
}

dpert_alt <- function (x, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.05, highp = 0.95, median = NULL,
                       shape = 4, log = FALSE) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  dpert(x, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape, log = log)
}

rpert_alt <- function (n, mode = 0, lowq = -1, highq = 1,
                       lowp = 0.05, highp = 0.95, median = NULL,
                       shape = 4) {
  if(is.null(median)){
    MinMax <- getPertMinMaxFromQuant(mode, lowq, highq,
                                     lowp = lowp, highp = highp,
                                     shape = shape)
  }else{
    MinMax <- getPertMinMaxModeFromQuant(median, lowq, highq,
                                         lowp = lowp, highp = highp,
                                         shape = shape)
    mode <- MinMax$mode
  }
  rpert(n, min = MinMax$min, mode = mode, max = MinMax$max, shape = shape)
}


getPertMinMaxFromQuant <- function(mode, lowq, highq,
                                   lowp = 0.05, highp = 0.95,
                                   shape = 4){
  if(lowq > mode || highq < mode || lowq > highq){
    stop('Invalid distributional parameters. Arguments must satisfy lowq < mode < highq')
  }
  if(lowp > highp || lowp < 0 || highp > 1){
    stop('Invalid distributional parameters. Arguments must satisfy  0 < lowp < highp < 1')
  }

  f <- function(x){
    sum((qpert(c(lowp, highp), min = lowq - x[1], max = highq + x[2], mode = mode, shape = shape) - c(lowq, highq))^2)
  }
  opt <- optim(c(1, 1), f , lower = 0, method = "L-BFGS-B")
  if(opt$convergence | sqrt(opt$value) > 0.001){
    stop(paste('Distributional parameters may be invalid, or very extreme.',
               '    Sometimes, though the parameters satisfy:',
               '        lowq < mode < highq',
               '    and',
               '        0 < lowp < highp < 1',
               '    these parameters can imply a distribution which is extremely',
               '    (or impossibly) left or right skewed.',sep = '\n'))
  }
  list(min = lowq - opt$par[1], max = highq + opt$par[2])
}

getPertMinMaxModeFromQuant <- function(median, lowq, highq,
                                   lowp = 0.05, highp = 0.95,
                                   shape = 4){

  if(lowq > median || highq < median || lowq > highq){
    stop('Invalid distributional parameters. Arguments must satisfy lowq < median < highq')
  }
  if(lowp > 0.5 || highp < 0.5 || lowp > highp || lowp < 0 || highp > 1){
    stop('Invalid distributional parameters. Arguments must satisfy 0 < lowp < 0.5 < highp < 1')
  }

  f <- function(x){
    sum((qpert(c(lowp, 0.5, highp), min = lowq - x[1], max = highq + x[2], mode = x[3], shape = shape) - c(lowq, median, highq))^2)
  }
  opt <- optim(c(1, 1,0), f , lower = c(0,0,lowq), upper = c(Inf, Inf, highq), method = "L-BFGS-B")
  if(opt$convergence | sqrt(opt$value) > 0.001){
    stop(paste('Distributional parameters may be invalid, or very extreme.',
               '    Sometimes, though the parameters satisfy:',
               '        lowq < median < highq',
               '    and',
               '        0 < lowp < 0.5 < highp < 1',
               '    these parameters can imply a distribution which is extremely',
               '    (or impossibly) left or right skewed.',sep = '\n'))
  }

  list(min = lowq - opt$par[1], max = highq + opt$par[2], mode = opt$par[3])
}

## log normal
dlnorm_alt <- function(x, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), log = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  dlnorm(x, meanlog, sdlog, log = log)
}

qlnorm_alt <- function(p, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), lower.tail = TRUE, log.p = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  qlnorm(p, meanlog, sdlog, lower.tail = lower.tail, log.p = log.p)
}

plnorm_alt <- function(q, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1), lower.tail = TRUE, log.p = FALSE){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  plnorm(q, meanlog, sdlog, lower.tail = lower.tail, log.p = log.p)
}

rlnorm_alt <- function(n, mean = exp(0.5), sd = exp(0.5)*sqrt(exp(1)-1)){
  sdlog <- sqrt(log(1 + (sd/mean)^2))
  meanlog <- log(mean) - log(1 + (sd/mean)^2)/2
  rlnorm(n, meanlog, sdlog)
}

# discrete distribution over finite set
rdiscrete <- function(n, x, prob = NULL){
  if(length(x) == 1){
    return(rep(x,))
  }
  sample(x, n, TRUE, prob)
}



