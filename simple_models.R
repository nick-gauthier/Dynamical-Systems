library(ggplot2) # for plotting
library(reshape2) # to reshape simulation outputs to make plotting easier
library(magrittr) # allows for pipe commands (%>%) to enhance readability

# plotting a sine function
t <- seq(-pi, pi, 0.01) # define a time vector t

sin(t)

qplot(x = t, y = sin(t), geom = 'line')


################################################
# simple population growth model

# define the model
pops <- function(A, x, N){ # define a function for running the population model w/ 3 parameters
  n <- rep(NA, N) # create an empty vector of length N
  n[1] <- x # set the first entry in the vector to x
  for(i in 2:N){ # loop over the remaining indices in N
    n[i] <-  A * n[i - 1] # exponential growth model
  }
  return(n) # return the output vector n of populations
}

# define some parameters
nsim <- 25 # define simulation length here so easy to change
lambdas <- c(0.8, 1, 1.1) # iterate over several values of lambda

# run the model and process the results for plotting
sim <- sapply(lambdas, pops, x = 1, N = nsim) %>% # run the pop function with each value of lambda
  set_colnames(lambdas) %>% # set the column names accordingly
  melt # use reshape to melt the datafram so ggplot can read it easier

# now plot with ggplot
qplot(x = Var1, y = value, color = as.factor(Var2), data = sim, geom = 'line') + 
  labs(title = 'Simple Exponential Growth Model', x = 'Time', y = 'Population')+
  scale_color_discrete(name = 'Lambda') +
  theme_minimal()

