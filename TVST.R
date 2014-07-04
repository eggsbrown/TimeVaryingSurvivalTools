# Functions for input into grouping functions
sumna<-function(x) sum(x,na.rm=T)
meanna<-function(x) mean(x,na.rm=T)
head1<-function(x) head(x,n=1) 
tail1<-function(x) tail(x,n=1)
nunique<-function(x) length(unique(x))
nuniquenb<-function(x) length(unique(x))-sum(unique(x)=="") # not blank
maxna<-function(x) max(x,na.rm=T)

# Grouping functions (these functions output at group level)
count.g<-function(level) tapply(level,level,length)
sum.g<-function(level,to) tapply(to,level,sumna)
mean.g<-function(level,to) tapply(to,level,meanna)
nunique.g<-function(level,to) tapply(to,level,nunique)
first.g<-function(level,to) tapply(to,level,head1)
nuniquenb.g<-function(level,to) tapply(to,level,nuniquenb)
max.g<-function(level,to) tapply(to,level,maxna)

# Generic function that outputs at individual level (repeater function) 
# These should be called windowing functions (see ddply manual)
agg.allot<-function(level,fun,...){
  agg<-fun(level,...)
  
  levs<-names(agg)
  n<-length(levs)
  x<-level  
  
  x <- agg[ match(level, levs) ]
  x
}

# Specific functions that output at individual level
count.allot<-count.<-function(level) agg.allot(level,count.g)
nunique.allot<-nunique.<-function(level,to) agg.allot(level,nunique.g,to)
nuniquenb.allot<-nuniquenb.<-function(level,to) agg.allot(level,nuniquenb.g,to)
first.allot<-first.<-function(level,to) agg.allot(level,first.g,to)
sum.allot<-sum.<-function(level,to) agg.allot(level,sum.g,to)
mean.allot<-mean.<-function(level,to) agg.allot(level,mean.g,to)
max.allot<-max.<-function(level,to) agg.allot(level,max.g,to)

# Lagging functions
between <- function(x, a, b) x >= a & x <= b
index.lagged<-function(time,lag,i) between(time,time[i]-lag[2],time[i]-lag[1]) 

# Create a generic lagging function, this applies the function "fun" to the 
# lagged variable "to". 
lagFun<-function(to,lag=c(0,5),fun,replace=NA,...){
  l<-length(to)
  y<-rep(NA,length(to))
  time<-1:length(to)
    
  for(i in 1:l){
    index<-index.lagged(time,lag,i)
    y[i]<-ifelse(sum(index)>0,fun(to[index],...),replace)      
  }
  y
}

lagMax<-function(to,lag=c(0,5),replace=NA) lagFun(to,lag,maxna,replace=replace)
lagMax.1<-function(x) lagMax(x,c(0,1))
lagMax.5<-function(x) lagMax(x,c(0,5))
lagMax.10<-function(x) lagMax(x,c(0,10))

lagMean<-function(to,lag=c(0,5),replace=NA) lagFun(to,lag,meanna,replace=replace)
lagSum<-function(to,lag=c(0,5),replace=NA) lagFun(to,lag,sumna,replace=replace)

# feed it a logical vector x, returns logical vector of indices greater the first True val
after<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[as.logical(x)]))
  y<-ind>m
  y  
}

afterIn<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[as.logical(x)]))
  y<-ind>=m
  y  
}

before<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[as.logical(x)]))
  y<-ind<m
  y  
}

beforeIn<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[as.logical(x)]))
  y<-ind<=m
  y  
}

lagMax.allot<-function(level,to,lag=c(0,5)) unlist(tapply(to,level,lagMax,lag=lag))
lagMax1.allot<-function(level,to) unlist(tapply(to,level,lagMax.1))
lagMax5.allot<-function(level,to) unlist(tapply(to,level,lagMax.5))
lagMax10.allot<-function(level,to) unlist(tapply(to,level,lagMax.10))

lagMean.allot<-function(level,to,lag=c(0,5)) unlist(tapply(to,level,lagMean,lag=lag))
lagSum.allot<-function(level,to,lag=c(0,5)) unlist(tapply(to,level,lagSum,lag=lag))

before.allot<-function(level,to) unlist(tapply(to,level,before))
beforeIn.allot<-function(level,to) unlist(tapply(to,level,beforeIn))
after.allot<-function(level,to) unlist(tapply(to,level,after))
afterIn.allot<-function(level,to) unlist(tapply(to,level,afterIn))

# Get infection pressure
getInfectionPressure<-getIP<-function(pid,spaceTime,infectious,infectiousLag=c(-1,7),incubationLag=c(0,5)){
  infectiousPeriod<-lagMax.allot(pid,infectious,infectiousLag) # lag infectious patien-time 
  pressure<-(sum.allot(spaceTime,infectious)-infectious)/count.allot(spaceTime)  # sum it up by ward and normalize by occupancy
  pressureLag<-lagMean.allot(pid,pressure,incubationLag) # lag the infectious patient by incubation time
  pressureLag
}