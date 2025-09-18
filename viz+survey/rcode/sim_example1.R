
# initial simulated examples 

pacman::p_load(tidyverse, survey, sampling, patchwork)
source('viz+survey/rcode/funs.R')

# ================================================
# First try ---------------------------------
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

# ================================================


# Second example ---------------------------------

N <- c(5000,5000)
n <- c( 100, 500)

sigma <- 5

x1 <- runif(N[1], 10, 20)
x2 <- runif(N[2], 20, 30)

y <- 10 + .5*c(x1,x2) + rnorm(sum(N), mean = 0, sd = sigma)

u <- data.frame(y=y, x=c(x1,x2)) |> 
  mutate(estrato=ifelse(x<20,1,2),
           aux = 1/(1+abs(x-22) )) 
           #aux=5+abs(rnorm(sum(N),x,x))) # para pik

# u %>% ggplot(aes(y=y, x=x)) + geom_point()
# u %>% ggplot(aes(y=aux, x=x)) + geom_point()

# muestra
s <- strata(u, stratanames = "estrato", size = n, 
            method = "systematic", pik = u$aux)

sdt <- u |> slice( s$ID_unit )

wt_plots(pop=u, smp=sdt, wts= 1/s$Prob, xs='x', ys='y')

rr <- sim_case1(p=12, b_alt = 1, b_null = .5, sigma = 5)

head(rr[[1]])

ggplot(data=rr[[1]], aes(x=x, y=y) )  +
  geom_point(aes(alpha=wts)) + facet_wrap(~.sample) 

ggplot(data=rr[[1]], aes(x=x, y=y) )  +
  geom_point(aes(size=wts),shape=1) + facet_wrap(~.sample) 


#    theme(aspect.ratio = 1) + labs(x = '', y = '') 
    