

## Facts
exponential_dist_mean <- 1/0.2


## Taking 1000 random samples of n=40
averages <- NULL

for (i in 1:1000)
{ 
  averages <- c(averages, mean(rexp(40, 0.2)))
}

mean(averages)

df <- data.frame(x = averages )
