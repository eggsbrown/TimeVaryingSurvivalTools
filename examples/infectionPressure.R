# Suppose you have a dataset that is in counting process format, which means 
# that there is one record for each day. There is a disease outcome y, 
# and patient id's
out<-c(0,0,1,0,0,0,0,0,0,0) # the outcome (1 = event, 0 = no event)
pid<-c(1,1,1,1,1,2,2,2,2,2) # a unique patient id
day<-c(1,2,3,4,5,1,2,3,4,5) # the day 

# Suppose that patients developing disease are considered infectious from the 
# day before they develop disease until two days afterwards (there's a 1 day
# diagnostic delay). Let's make a variable called infectiousness that captures 
# when a given patient is exerting infectiousnes
infectious<-lagMax.allot(pid,out,c(-1,2)); infectious 

# Now, let's calculate the disease pressure "received" by these patients from others 
dp<-sum.allot(day,infectious) - infectious; dp # remove disease pressure from self

# Now we want to measure the association between disease pressure on risk.
# We will restrict the data to "at-risk" follow-up time (prior to symptom
# onset) using the "beforeIn.allot" function and tabulate
hospit<-data.frame(out,pid,day,infectious,dp) # Create a data frame 
with(hospit[beforeIn.allot(pid,out),],table(out,dp)) # Subset the data and make a table
