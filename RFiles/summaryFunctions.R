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

#add lists of lists of numeric vectors elementwise across the list
add2 <- function(...){
  args <- list(...)
  if(nargs() == 0){
    return(NULL)
  }else if(nargs() == 1){
    return(args[[1]])
  }else  pmap(args,add)
}

#Traverse n-level nested lists, calculate quantiles, then reformat nested list to a dataframe
quantilesNestedList <- function(list,depth,names_to,probs = c(0.5,0.05,0.95),quant_names = c('median', '5%','95%')){
  if(length(names_to)!=depth) stop('Each nesting level of the list needs a name')
  list %>%
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
