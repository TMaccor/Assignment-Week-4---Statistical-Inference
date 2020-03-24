library(ggplot2)
library(ggpubr)

## Facts
exponential_dist_mean <- 1/0.2


## 1.- Taking 1000 random samples of n=40
averages <- NULL

for (i in 1:1000)
{ 
  averages <- c(averages, mean(rexp(40, 0.2)))
}

averages_df <- data.frame(x_average = averages )

mean_averages <- mean(averages)


## 2. Taking 1000 random Nrs. in exponential distribution
exponential <- rexp(1000, 0.2)
expo_df <- data.frame(x_expo = exponential)

## Plotting
theoreticalMean <- 1 / 0.2
theoreticalVariance <- (1/0.2)^2 / 40

expo_plot <- ggplot(expo_df, aes(x_expo)) + geom_histogram(color = "black", fill = "red") + 
             ggtitle("Exponential Distribution - 1000 random values") +
             theme(axis.title.x = element_blank())


sample_plot <- ggplot(averages_df, aes(x_average)) +
                geom_histogram(aes(y = ..density..), binwidth = 0.15, color = "black", fill = "lightblue") +
                ggtitle("Exponential Distribution - 1000 means from random samples (n=40)") +
                xlab("Sample Means") +
                stat_function(fun = dnorm, args = list(mean = theoreticalMean, sd = sqrt(theoreticalVariance)), 
                                size = 1.2) +
                geom_vline(xintercept = mean_averages, size = 1.5, color = "red") + 
                geom_text(x = 8, y = 0.40, label = "- AVERAGE of the MEAN of samples", color="red") +
                geom_text(x = 8, y = 0.45, label = "- Curve of a Normal Distribution", color="black") 




