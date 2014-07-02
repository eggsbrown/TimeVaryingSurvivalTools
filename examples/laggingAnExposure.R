# Suppose you have a vector of exposures x, among patients with unique pids
x   <-c(0,1,1,0,0,0,1,0,0,0)
pid <-c(1,1,1,1,1,2,2,2,2,2)
time<-c(1,2,3,4,5,1,2,3,4,5)
y   <-c(0,0,0,0,1,0,0,0,0,0)

# To calculate the impact of exposure x on outcome y
# First calculate the time-advanced variable xlag 
xlag<-lag5By(pid,x); 

