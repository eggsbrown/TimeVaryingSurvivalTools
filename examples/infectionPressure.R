# Calculate disease pressure
daily$infectious<-with(daily,lagMax.allot(peid,medany,c(0,3)))
describe(daily$infectious)
daily$dp<-with(daily,sum.allot(ward %+% datetime,infectious)-infectious)
describe(daily$dp)
daily$occ<-with(daily,count.allot(ward %+% datetime))
describe(daily$occ)
daily$dp.occ<-with(daily,dp/occ) # normalize by ward-level occupancy
describe(daily$dp.occ)

# Incidence rates in the exposure categories;
with(subset(daily,sc==1),getIRs(cdi_event,lagMax5.allot(peid,dp.occ>0),weight))
daily$dp.abx<-with(daily,lagMean.allot(peid,dp.occ))
with(subset(daily,sc==1),getIRs(cdi_event,cut2(dp.abx,c(.25,.5,.75)),weight))
with(subset(daily,sc==1),getIRs(cdi_event,lagMax10.allot(peid,medany) %+% cut2(dp.abx,c(.5)),weight))

# Insanely statistically significant effects
m<-glm(cdi_event ~ dp.abx*lagMax5.allot(peid,medany),  family=poisson("log"), data=daily, subset=sc==1); 
summary(m)
