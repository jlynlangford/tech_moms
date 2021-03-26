
#  Assignment Operator: Reads "n gets 15"
n <- 15
n

#  R is case sensitive
x <- 1
X <- 10
x
X

#  If any object already exists, its previous value is overwritten
n
n <- 10 + 2
n

#  Generate 100 observations from a normal distribution with mean = 100, standard deviation = 10
?rnorm
dist = rnorm(n = 100, mean = 100, sd = 10)

#  Density plot the observations we just generated
plot(density(dist), type = "l")

#  Lets write a function to generate observations and return a plot
myFirstFunction = function(my_n, my_mean, my_sd) {
  my_dist = rnorm(n = my_n, mean = my_mean, sd = my_sd)
  my_plot = plot(density(my_dist), type = "l")
  
  return(my_plot)
}

myFirstFunction(my_n = 10000, my_mean = 100, my_sd = 10)
myFirstFunction(my_n = 10000, my_mean = 100, my_sd = 1)
myFirstFunction(my_n = 10000, my_mean = 50, my_sd = 10)

