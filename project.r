
# Uniform Distribution
uniformdistrib <- function(n)
{
    repetitions <- 100  # repeated 100 times
    list        <- c()
    data_list   <- c()

    for(i in 1:repetitions)
    {
        sample <- runif(n, min=0, max=100)   # (r)andom (unif)orm
        sample_mean <- mean(sample)         # calculate the mean
        list <- c(list, sample_mean)        # push mean to list
        data_list <- c(data_list , sample)
    }

    # output the histogram
    pdf(paste("uniform_mean_", n, ".pdf", sep = ""))
    hist(list, freq=TRUE, main="Histogram of Mean Uniform Distribution", xlab="Sample Means")
    pdf(paste("uniform_data_", n, ".pdf", sep = ""))
    hist(data_list, freq=TRUE, main="Histogram of Uniform Distribution", xlab="Sample")
}

uniformdistrib(5)
uniformdistrib(30)
uniformdistrib(100)

# Weibull Distribution
weibulldistrib <- function(n)
{
    repetitions <- 100  # repeated 100 times
    list        <- c()
    data_list   <- c()

    for(i in 1:repetitions)
    {
        sample <- rweibull(n, shape=2, scale=10)
        sample_mean <- mean(sample)         # calculate the mean
        list <- c(list, sample_mean)        # push mean to list
        data_list <- c(data_list , sample)
    }

    # output the histogram
    pdf(paste("weibull_mean_", n, ".pdf", sep = ""))
    hist(list, freq=TRUE, main="Histogram of Mean Weibull Distribution", xlab="Sample Means")
    pdf(paste("weibull_data_", n, ".pdf", sep = ""))
    hist(data_list, freq=TRUE, main="Histogram of Weibull Distribution", xlab="Sample")
}

weibulldistrib(5)
weibulldistrib(30)
weibulldistrib(100)

# Standard Beta Distribution
stdbetadistrib <- function(n)
{
    repetitions <- 100  # repeated 100 times
    list        <- c()
    data_list   <- c()

    for(i in 1:repetitions)
    {
        sample <- rbeta(n, shape1=2, shape2=3)
        sample_mean <- mean(sample)         # calculate the mean
        list <- c(list, sample_mean)        # push mean to list
        data_list <- c(data_list , sample)
    }

    # output the histogram
    pdf(paste("standardbeta_mean_", n, ".pdf", sep = ""))
    hist(list, freq=TRUE, main="Histogram of Mean Standard Beta Distribution", xlab="Sample Means")
    pdf(paste("standardbeta_data_", n, ".pdf", sep = ""))
    hist(data_list, freq=TRUE, main="Histogram of Standard Beta Distribution", xlab="Sample")
}

stdbetadistrib(5)
stdbetadistrib(30)
stdbetadistrib(100)


# Log Normal Distribution
lognormaldistrib <- function(n)
{
    repetitions <- 100  # repeated 100 times
    list        <- c()
    data_list   <- c()

    for(i in 1:repetitions)
    {
        sample <- rlnorm(n)
        sample_mean <- mean(sample)         # calculate the mean
        list <- c(list, sample_mean)        # push mean to list
        data_list <- c(data_list , sample)
    }

    # output the histogram
    pdf(paste("lognormal_mean_", n, ".pdf", sep = ""))
    hist(list, freq=TRUE, main="Histogram of Mean Log Normal Distribution", xlab="Sample Means")
    pdf(paste("lognormal_data_", n, ".pdf", sep = ""))
    hist(data_list, freq=TRUE, main="Histogram of Log Normal Distribution", xlab="Sample")
}

lognormaldistrib(5)
lognormaldistrib(30)
lognormaldistrib(100)
