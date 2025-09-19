
# Plot hwy vs cyl from the web of geoom_jitter 
# https://ggplot2.tidyverse.org/reference/geom_jitter.html

p <- ggplot(mpg, aes(cyl, hwy))
p + geom_jitter()

# jitter by hand 
mpg |>
  mutate(hwy = hwy + runif( n(), -.5, .5) ) |>
  mutate( cyl = cyl + runif(n(), -.5, .5) ) |> 
  ggplot() + geom_point( aes(cyl, hwy) ) + 
  geom_jitter(data=mpg, aes(cyl, hwy), color='red', alpha=.3)


# bivariate normal jitter 
# instead or runif use mvnorm() for the noise
vv <- mpg |> select(hwy, cyl) |> var()
library(mvtnorm)
noise <- rmvnorm( nrow(mpg), sigma = vv )

mpg |>
  mutate(hwy = hwy + .3*noise[,1] ) |>
  mutate( cyl = cyl + .3*noise[,2]  ) |> 
  ggplot( ) + geom_point( aes(cyl, hwy) ) + 
  geom_jitter(data=mpg, aes(cyl, hwy), color='red', alpha=.3 ) 


#  model assisted  jitter ? 
p + geom_point() + geom_smooth(method = 'lm')
mm <- lm(hwy ~ cyl, data=mpg)
mm

