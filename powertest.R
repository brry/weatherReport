# power test

# with 15 participants from 4 groups:
source("functions.R")
vis_slope("sit", list(mean))

agg <- apply(motiondata$sit[,"m",,], MARGIN=2:3, mean, na.rm=TRUE)
diffs <- agg[,"c"] - agg[,"b"]
hist(diffs)
sd(diffs) # 0.76

# how many participants do we need?
power.t.test(delta=0.5,      # true difference in acceleration (m/s²), smallest meaningful relevant diff - medical decision!
             sd=0.8,         # standard deviation of differences (pre-study to determine)
             sig.level=0.05, # Type 1 Error probability (falsely reject H0) - your choice (common: 0.01, 0.05, 0.10) 
             power=0.9,      # probability of rejecting a false hypothesis  - your choice (common: 0.80, 0.90, 0.95)
             type="paired", alternative="one.sided")
 
# Monte Carlo simulation
# input parameter ranges:
posnorm <- function(mean,sd) {res <- -1; while(res<0.02) res <- rnorm(1,mean,sd); return(res)}
replicate(1e4, posnorm(0.5, 0.5))    |> hist(breaks=80)
replicate(1e4, posnorm(0.8, 0.5))    |> hist(breaks=80)
replicate(1e4, runif(1, 0.01, 0.10)) |> hist(breaks=80)
replicate(1e4, rbeta(1, 19, 3))      |> hist(breaks=80) # mode = (a-1)/(a+b-2) = 18/20 = 0.90
# actually run power test with different inputs
nsim <- pbapply::pbreplicate(1e4, power.t.test(
                                    delta    =posnorm(0.5, 0.5), 
                                    sd       =posnorm(0.8, 0.5), 
                                    sig.level=runif(1, 0.01, 0.10), 
                                    power    =rbeta(1, 19, 3), 
                                    type="paired", alternative="one.sided")$n)

berryFunctions::logHist(nsim, breaks=50, main="needed study size", xlab="n")


# shiny app with sliders in powertest_shiny.r
