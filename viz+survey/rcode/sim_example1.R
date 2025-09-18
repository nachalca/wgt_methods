
# simulated example

library(tidyverse)
library(survey)
N <- 10e3

# xplicative: size of the store, asimetric 
x <- rgamma(N, 10, .5) 
ggplot()+geom_density(aes(x=x))

# selection prob: prop to size in x
pik <- x/sum(x)  # ver samplin inclusion prob


# response: sales, affected by x in exponential relationship
y = exp(.08*x) + rnorm(N, sd=2)
#ggplot() + geom_line(aes(x,y))

ggplot() + geom_point( aes(x,y, color=pik) )


# take a sample with un-uqual probability
n <- 100 
idx <- sample(1:N, size=n, prob=pik) 

pop <- data.frame( y, x ) |>  mutate(smp = row_number() %in% idx)

dd <- data.frame(ys=y, xs=x, ws= (1/pik)/n , fpc = n/N) |> slice(idx) 

ggplot() + 
  geom_point(data=pop, aes(x,y)) + 
  geom_point(data=dd, aes(xs, ys), color='red')



# set up design and get esitimates 
? svydesign
des <- svydesign(ids=~1, weights = ~ws , data = dd, fpc= ~fpc) 

# Estimate ty 
svytotal(~ys, des)
svymean(~ys, des)

mean( dd$ys ) # biased estimate 
mean(y) # true mean

ggplot(dd) + geom_point(aes(xs, ys, size = ws), shape=1 )
ss <- ggplot(dd) + geom_point(aes(xs, ys, alpha = ws)  )

? svyplot

library(patchwork)
pp+ss

ggplot(dd) + geom_point(aes(xs, ys, color=))

