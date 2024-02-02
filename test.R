
bw_rule <- function(delta, rule = 'ucv') {
  # Values must be in range 1, Inf. Take inverse if values are in range 0, 1
  if (min(delta) < 1) {
    delta <- 1/delta
  }
  # Values == 1 are artifacts of DEA method and should be removed for h
  delta_m <- delta[delta > 1]
  delta_2m <- c(delta_m, 2 - delta_m)
  # See Daraio & Wilson (2007) for a discussion on methods for determining bandwidth
  h <- switch(rule,
    silverman = bw.nrd0(delta_2m),
    scott = bw.nrd(delta_2m),
    ucv = suppressWarnings({ h <- bw.ucv(delta_2m) }),
    suppressWarnings({ h <- bw.ucv(delta_2m) })
  )
  # See Daraio & Wilson (2007), p. 61, eq. 3.26
  h <- h * 2^(1/5) * (length(delta_m)/length(delta))^(1/5) * (sd(delta)/sd(delta_2m))
  h
}

# Generate a dataset of 100 random numbers between 0 and 2
set.seed(123) # for reproducibility
delta <- runif(100, min = 0, max = 2)

# Use the bw_rule function on the dataset
h <- bw_rule(delta, rule = 'ucv')

# Print the calculated bandwidth
print(h)

data(iris)
head(iris)

library(dplyr)
library(stringr)

# Filter the iris dataframe
iris_filtered <- iris %>% filter(str_starts(Species, "s"))

# Print the first few rows of the filtered dataframe
head(iris_filtered)

library(ggplot2)

ggplot(iris_filtered, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_point() +
    labs(title = "Scatter plot of Sepal.Length vs Sepal.Width",
             x = "Sepal Length",
             y = "Sepal Width",
             color = "Species")
