# Bootstrapping is a common resampling technique used to assess uncertainty in variables. This can be
# especially relevant when the variable of interest is already a summary of a statistical population, such as a
# coefficient of variation (CV). As a first example, we will use bootstrapping to obtain the standard error of
# the mean, which we have already seen is given theoretically by SE= Var/n= SD/√n.
# To ensure reproducibility of the results (i.e. that we will get the same result every time, even when we work
# on different computers), we set the “seed” of the random number generator using the set.seed() function.
set.seed(1)
x = rnorm(50, 10, 2)
se_x = sqrt(var(x)/length(x))

# We will do a non-parametric bootstrap, where we resample the data many times. For each resampling, we
# maintain the original sample size (here 50), but we draw from the data with replacement, so that by chance
# some values will be sampled several times and others not at all. Before running the for-loop, we have to
# define the variable (here called out) that we will use to store the results for each iteration of our loop.

out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}
mean(x)

# The variable out now contains what we can call the sampling distribution of the mean of x. The standard
# deviation of the sampling distribution gives an approximation of the standard error (and this is very different
# from the standard deviation of the original data!).
mean(out)
hist(out, las=1, main="")
sd(out)#≈ to se_x=0.235
quantile(out, c(0.025, 0.975))


# Recall that we could also have obtained an approximation of the 95% confidence interval as ±1.96SE. This
# follows from the properties of the standard normal distribution (with a mean of zero and a variance of one),
# for which the 2.5 and 97.5 percentiles falls ~1.96 standard deviations from the mean. The quantiles of the
# standard normal distribution are available in R through the qnorm function. The same is true for other
# probability distributions, e.g. qbinom for the binomial distribution etc.
qnorm(c(0.025, 0.975))

mean(x)- 1.96*se_x
mean(x)+ 1.96*se_x
