?cars

# View data set in a new tab
View(cars)

# See first six rows of data in the console
head(cars)

# Display internal structure of an R object
str(cars)

# Dimension of the data frame or matrix
dim(cars)

# Number of rows of the data frame or matrix
nrow(cars)

# Number of rows of the data frame or matrix
ncol(cars)

# Plot the relationship between speed and stopping distance
plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")

# Least squares regression function
?lm
model <- lm(dist ~ speed, data = cars)
model
summary(model)
coef(model)

plot(dist ~ speed, data = cars,
     xlab = "Speed (in Miles Per Hour)",
     ylab = "Stopping Distance (in Feet)",
     main = "Stopping Distance vs Speed",
     pch  = 20,
     cex  = 2,
     col  = "grey")
abline(model, lwd = 3, col = "darkorange")


