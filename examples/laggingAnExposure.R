# Suppose you have a dataset that is in counting process, which means 
# that there is one record for each time period. There is a vector of 
# exposures x, a disease outcome y, a patient  pids. 
exp<-c(0,1,1,0,0,0,1,0,0,0) # exposure
out<-c(0,0,0,0,1,0,0,0,0,0) # the outcome (1 = event, 0 = no event)
pid<-c(1,1,1,1,1,2,2,2,2,2) # a unique patient id
day<-c(1,2,3,4,5,1,2,3,4,5) # the day 

# To calculate the impact of exposure x on outcome y
# Do the exposure lagging thing 
lagMax1.allot(pid,exp)
lagMax.allot(pid,exp,lag=c(0,2));

# Save one lagged exposure
expLag<-lagMax1.allot(pid,exp); expLag

# Now you can calculate the incidence rate of disease 
# for exposed and unexposed individuals
table(expLag,y)
tapply(y,expLag,mean)