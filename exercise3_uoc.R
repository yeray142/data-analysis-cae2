# Given lambda the number of columns and nA, nB, nC the counts of each sample
lambda = 3
n = 18

# H0: nu1 = nu2 = nu3
# H1: some population means are different

alpha = 0.05

sample_A <- c(643, 655, 702, 682, 647, 638, 697)
sample_B <- c(469, 427, 525, 489, 452, 510)
sample_C <- c(484, 456, 402, 431, 493)
data <- data.frame(
  values = c(sample_A, sample_B, sample_C),
  group = factor(rep(c("A", "B", "C"), times = c(length(sample_A), length(sample_B), length(sample_C))))
  )
anova_result <- aov(values ~ group, data = data)

# Using p_value
p_value = summary(anova_result)[[1]]$`Pr(>F)`[1]
if (p_value < alpha) {
  print("These samples do NOT belong to the same population.")
} else {
  print("These samples belong to the same population.")
}

# Using contrast statistic
df1 = lambda - 1
df2 = n - 3
f_critical = qf(0.05, df1, df2, lower.tail = FALSE)
f = summary(anova_result)[[1]]$`F value`[1]
if (f > f_critical) {
  print("These samples do NOT belong to the same population.")
} else {
  print("These samples belong to the same population.")
}