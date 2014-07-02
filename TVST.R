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

# returns an equal length vector
lag.max<-function(x,length=5){
  y<-vector(mode="numeric",length=length(x))
  for(i in 1:length(x)){ 
    start.index<-max(1,(i-length))
    x.lagged<-x[start.index:i]
    y[i]<-max(x.lagged)
  }
  y
}

lag5<-function(x) lag.max(x,5)
lag10<-function(x) lag.max(x,10)

# feed it a logical vector x, returns logical vector of indices greater the first True val
after<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[x]))
  y<-ind>m
  y  
}

before<-function(x) { 
  ind<-1:length(x)
  m<-min(c(Inf,ind[x]))
  y<-ind<m
  y  
}

lag5By<-function(level,to) unlist(tapply(to,level,lag5))
lag10By<-function(level,to) unlist(tapply(to,level,lag10))
beforeBy<-function(level,to) unlist(tapply(to,level,before))
afterBy<-function(level,to) unlist(tapply(to,level,after))


# based on a datetime
lag.dt.max<-function(x,dt,length=5){
  y<-vector(length=length(x))
  for(i in 1:length(x)){    
    dt.index<-dt %in% dt[i]-0:5
    x.lagged<-x[start.index:i]
    y[i]<-max(x.lagged)
  }
  y
}
