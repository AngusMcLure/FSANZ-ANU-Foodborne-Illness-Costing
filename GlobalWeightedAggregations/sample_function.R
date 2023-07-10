#sample from a dist constructed based on expert's 3 percentiles
#and specified bounds 
#export given number of samples for all vbls

#import data
setwd("C:/Users/ahanea/Dropbox/work/projects/FSANZ/SEJ/deliverables/4report")


quantile_data = read.csv("GWperc.csv")

nvbls= nrow(quantile_data)

#vectors that store the info about the intrinsic range
#for now all the lower bounds are set to 0
#all the upper bounds are set to 100

Larg_vect = rep(0,nvbls)
Uarg_vect = rep(100,nvbls)

#function to sample, from given percentiles, U=0, L=100, and for specified number of sample points
sample_exp_dist <- function(numSamplePoints,exp_quantiles,Larg,Uarg)
{
  y = c(Larg,exp_quantiles,Uarg)
  
  #y points used for sampling
  #for sampling using approx, I need to swap x and y
  x = c(0, 0.05, 0.5, 0.95, 1)
  
  sample_points = approx(x,y,n=numSamplePoints)$y
  
  return (sample_points)
}

#now get the sample data for all vbls
sample_data = function(numSamplePoints,Larg_vect,Uarg_vect)
  {
  
stored_data = matrix(0, nvbls, numSamplePoints)

for (i in 1: nvbls)
{
  stored_data[i,] = sample_exp_dist(numSamplePoints,quantile_data[i,2:4],Larg_vect[i],Uarg_vect[i])
}

return(t(stored_data))
}



sample_data(1000,Larg_vect,Uarg_vect)

#export the csv file
write.csv(sample_data(1000,Larg_vect,Uarg_vect),"sample_dataGW.csv", row.names = FALSE)


