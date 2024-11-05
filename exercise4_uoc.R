x = c(2.75, 5.50, 8.25, 11.00, 13.75)
y = c(663, 1247, 1453, 2010, 2371)

# Model the linear regression
model = lm(y ~ x)

# Extract the coefficients
a = coef(model)[1]  # Intercept
b = coef(model)[2]  # Slope

# Print the coefficients
print(cat("Intercept (a):", a, "\n"))
print(cat("Slope (b):", b, "\n"))

# Get the predicted values (polyval equivalent)
y_pred = predict(model, newdata = data.frame(x = x))
print(y_pred)

# Print the R-squared value
r_squared <- summary(model)$r.squared
print(cat("R-squared:", r_squared, "\n"))

# Plot the scatter and the line
plot(x, y, main="Linear regression", xlab="x", ylab="y")
abline(model, col="blue")