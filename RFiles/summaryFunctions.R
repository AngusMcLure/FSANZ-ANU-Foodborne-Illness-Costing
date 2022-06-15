##### Some helper functions for adding and summarising lists of lists #########
# add lists of numeric vectors elementwise across the list
add <- function(...){
  args <- list(...)
  if(nargs() == 0){
    return(NULL)
  }else if(nargs() == 1){
    return(args[[1]])
  }else if(nargs() == 2){
    pmap(args, `+`)
  }else{
    reduce(args,add)
  }
}

#add lists of lists of numeric vectors element-wise across the list
add2 <- function(...){
  args <- list(...)
  if(length(args) == 0){
    return(NULL)
  }else if(length(args) == 1){
    return(args[[1]])
  }else  pmap(args,add)
}

addn <- function(...,.n){
  if(!is.numeric(.n) || .n%%1 || .n < 0){
    stop('.n must be a non-negative integer')
  }
  args <- list(...)
  if(length(args) == 0){
    return(NULL)
  }else if(length(args) == 1){
    return(args[[1]])
  }else{
    if(.n == 0){
      reduce(args,`+`)
    }else if(.n == 1){
      add(...)
    }else{
      pmap(args,addn,.n = .n - 1)
    }
  }
}

laddn <- function(l,.n){do.call(addn,c(unname(l),.n = .n))}
ladd <- function(l){laddn(l,1)}

#make a n-level nested list into a rectangular tibble with the lowest level list stored in a column list
rectangle <- function(x, names_to, values_to){
  x %>%
    rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
    lapply(eval) %>%
    as.data.frame(check.names = F) %>%
    summarise(across(.fns = list)) %>% #make the draws list columns so they take up less space with we lengthen the dataframe
    pivot_longer(everything(), #lengthen dataframe
                 names_sep = "\\.",
                 names_to = names_to,
                 values_to = values_to)
}


#Traverse n-level nested lists, calculate quantiles, then reformat nested list to a dataframe
quantilesNestedList <- function(x,depth,names_to,probs = c(0.5,0.05,0.95),quant_names = c('median', '5%','95%')){
  if(length(names_to)!=depth) stop('Each nesting level of the list needs a name')
  x %>%
    map_depth(depth, ~unname(quantile(.x,probs = probs))) %>% #get median and intervals
    rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
    lapply(eval) %>%
    as.data.frame(check.names = F) %>%
    mutate(Quantile = quant_names) %>% #label quantiles
    pivot_longer(-Quantile,
                 names_sep = if(depth > 1){"\\."}else{NULL},
                 names_to = names_to) %>%
    pivot_wider(names_from = "Quantile", values_from = 'value')

}
