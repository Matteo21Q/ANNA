########################################
####  Sample size calculation ANNA  ####
########################################

set.seed(2020)    # For replicability
n<-seq(50,250,50) # Sample sizes to be compared
n.sim<-1100       # Number of simulations. 1100 because I guess nobody ever ran 1100 simulations

pow<-matrix(0,n.sim,length(n))    #Initialise matrix of powers

for (i in 1:n.sim) {
  for (j in 1:length(n)) {
    
    talk<-rbinom(n[j],1,0.5)  #Generate "treatment" variable as binomial p=0.5
    lambda<-0.75+talk*0.5        #Mean of poisson: 0.75 if silence, 1.25 if talking
    y<-rpois(n[j],lambda)     #Generate outcome: number of fetal moves
    
    fit<-glm(y ~talk, family = poisson)
    if ( coef(summary(fit))[2,4]<0.05) {
      pow[i,j]<-1
    }
  }
  if (i%%100==0) cat("sim ",i, "completed\n")
}

powers<-apply(pow,2,mean) # Power of different sample sizes 
data.frame(n,powers)      # Results