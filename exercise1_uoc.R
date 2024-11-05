set.seed(100)

# Generate a random sample of 2000 elements
s = rnorm(n = 2000, mean = 10, sd = 2)
# Generate a random poisson distribution of 2000 elements
# s <- rpois(2000, lambda = 5)


#
# 1 - Confidence interval of the random sample s
#
conf_interval = t.test(s, conf.level = 0.9)$conf.int
cat("Confidence interval at 90%:", conf_interval, "\n")


#
# 2 - Test if it belongs to normal or Poisson distribution
#
# TEST FOR NORMAL DISTRIBUTION
# Define the number of bins for chi-square test
num_bins = 10
breaks = seq(min(s), max(s), length.out = num_bins + 1)
observed = hist(s, breaks = breaks, plot = FALSE)$counts

# Calculate mean and standard deviation
mu = mean(s)
sigma = sd(s)

# Calculate expected counts using normal distribution
bin_centers = (breaks[-1] + breaks[-length(breaks)]) / 2
pd = dnorm(bin_centers, mean = mu, sd = sigma)
nprime = sum(observed) * pd / sum(pd)

# Chi-square statistic
chi_sqr = sum((observed - nprime)^2 / nprime)

# Degrees of freedom adjusted for normal (num_bins - 3 for mean and variance)
df = num_bins - 3
chistat = qchisq(0.95, df)

if (chi_sqr < chistat) {
  print("The sample likely follows a normal distribution.")
} else {
  print("The sample does not follow a normal distribution.")
}

# TEST FOR POISSON DISTRIBUTION
# Get the observation values and counts
observed = table(s)
observed_counts = as.numeric(observed)
observed_values = as.numeric(names(observed))

pd = dpois(observed_values, 5)
nprime = sum(observed_counts) * pd
chi_sqr = sum(((observed_counts - nprime) ** 2) / nprime)

df = length(observed_values) - 1
chistat = qchisq(0.95, df)

if (chi_sqr < chistat) {
  print("The sample likely follows a Poisson distribution.")
} else {
  print("The sample does not follow a Poisson distribution.")
}


