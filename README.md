TimeVaryingSurvivalTools
========================

A set of R functions for analyzing multilevel models and cohort studies with time-varying exposures.

The analysis of cohort studies with repeated measurements per individual, or multi-level models with multiple individuals per group, often requires that exposure measurements be lagged in time, or aggregated across groups. When working with time-varying survival models, a plethora of functions that lag observarions across time windows in a variety of ways may be necessary in order to both structure data, and test scientifically relevant hypotheses of how exposures impact risk through time. Similarly, when dealing with multilevel data, aggregated group-level characteristics of individuals may impact other individuals. However, aggregation functions that are applied over groups of individuals, such as the mean, the median, the minimum, the maximum are returned at the individual level while multi-level regression functions requires that group-level characteristics be repeated across individuals, in the individual-level format of the analysis dataset. Such data manipulation algorithms that apply statistical functions over groups of inidividuals but then return values for each individual observation are called "windowing" functions. The purpose of this R package is to provide a coherent set of windowing functions that are useful for analysis of data with time-varying covariates and multi-level data.

The functions are of 2 groups: lagging windowing function and aggregation windowing functions.

Aggregation functions
---------------------
* agg.allot  - A generic aggregation function that works over the levels of a categorical "group" variable
* mean.allot
* count.allot 
* min.allot / max.allot
* etc...

Lagging functions
-----------------
* lagFun.allot - A generic lagging function (can insert own function) that work over the levels of a group variable
* lagMax5.allot - Finds the maximum over a 5 day window starting on day 0 and going back 5 days
* etc...

Other functions
---------------
* %*% - An operator that pastes two vectors together
