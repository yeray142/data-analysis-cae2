s1 = c(371, 352, 397, 378, 347, 366, 387, 366, 378, 359, 361, 382)
s2 = c(409, 382, 361, 394, 361, 382, 387, 359, 397, 359, 373, 382)
alpha = 0.05

test_result = wilcox.test(s1, s2, alternative = "two.sided", conf.int = TRUE, exact = FALSE)
p = test_result$p.value
h = ifelse(p < alpha, 1, 0)

if (h == 0) {
  print("Both samples belong to the same population.")
} else {
  print("The samples do NOT belong to the same population.") 
}