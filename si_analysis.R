
# create vector of length 1,000 all of value 0.2 (lambda)
lambda <- rep(0.2, 1000)

# for each vector position, create 40 values from an
#  exponential distribution with value lambda (0.2)
sim <- lapply(lambda, function(x) rexp(40, x))

# theoretical mean is 1/lambda = 1/0.2 = 5
# theoretical sd is 1/lambda = 1/0.2 = 5
#  so each of the 1,000 vectors of length 40
#  have theoretical mean = 5, sd = 5

# the distribution of the sample means follows the 
#  central limit theorem, with states:
#  mean of sample means = population mean
#  sd of sample means = population mean/sqrt(n)
#  where n is the sample size
#  and sample means are normally (gaussian) distributed
#  regardless of the population distribution, exponential in this case

# the theoretical/actual mean of sample means is:
#```r 
means <- sapply(sim, mean) # vector of length 1,000 (length(means))
mean(means) # approx 1/lambda = 1/0.8 = 5
sd(means) # approx 5/sqrt(40) = 5/6.25 = 0.8
#```

getPlot <- function(histx, mu, sigma, plotlimits=seq(2,8)) {
        df <- data.frame(vals=histx)
        ggplot() + 
                geom_histogram(data = df, 
                               mapping = aes(x=vals, y=..density..)) + 
                stat_function(data = data.frame(x=plotlimits), 
                              mapping = aes(x), 
                              fun = dnorm, 
                              args = list(mean=mu, sd=sigma), 
                              color = "red")
}
getPlot(means, 5, 0.8)

