# Function plotting all scatersplots options with wts 
# need: population, design, sample (is in design object? )

#' @param pop population 
#' @param smp sample data
#' @param wts sampling weights 
#' @param xs  name of x variable
#' @param ys  name of y variable
#' @param jj jitter percentage

wt_plots <- function( pop, smp, wts, xs, ys, jj=.05 ) {

  nn <-  nrow(smp)
  NN <- nrow(pop)
  xsym <- sym(xs)
  ysym <- sym(ys)

  rgx <- smp[, xs] |> range() |> diff()
  rgy <- smp[, ys] |> range() |> diff()
  rgx <- jj*rgx
  rgy <- jj*rgy

  popuplot <- ggplot(pop) + 
    geom_point(aes(x= !!xsym , y= !!ysym ), alpha= 1/(NN*.005) ) + 
    theme(aspect.ratio = 1) + labs(title = 'population', x = '', y = '')

  lms <- ggplot_build(popuplot)$layout$panel_params[[1]][c("x.range", "y.range")]


  baseplot <- ggplot(data=smp, aes(x= !!xsym , y= !!ysym ) )  +
    theme(aspect.ratio = 1) + labs(x = '', y = '') + 
    scale_x_continuous(limits = lms$x.range) + 
    scale_y_continuous(limits = lms$y.range) 

  p1 <- baseplot + geom_point(aes(size=wts), shape=1) + labs(title='buble')

  p2 <- baseplot + geom_point(aes(alpha=wts)) + labs(title='transparency')
  
  p3 <- baseplot + geom_hex(aes(weight=wts)) + labs(title = 'hex')

  ids <- sample(1:nn, replace = TRUE, prob = wts )

  p4 <- smp |> slice(ids) |> 
    ggplot() + geom_jitter(aes(y = !!ysym, x = !!xsym),width = rgx, height =rgy  ) +
    theme(aspect.ratio = 1) +
    scale_x_continuous(limits = lms$x.range) + 
    scale_y_continuous(limits = lms$y.range) + 
    labs(title='subsampling', x = '', y = '')
  
  p5 <-  smp |> mutate(wts= round(wts)) |> uncount(weights=wts) |> 
    ggplot() + 
    geom_jitter(aes(y = !!ysym, x = !!xsym), width = rgx, height = rgy, alpha=1/50) +
    scale_x_continuous(limits = lms$x.range) + 
    scale_y_continuous(limits = lms$y.range) +
    theme(aspect.ratio = 1) + 
    labs(title='expanded', x = '', y = '') 
  
  (p1 + p2 + p3) / (popuplot + p5+ p4)  + 
    plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") & 
    theme(legend.position = "none")
}


#library(tidyverse)
#library(survey)
#library(patchwork)
#data(api)
#dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
#wt_plots(pop=apipop, smp=apistrat,wts=1/dstrat$prob, xs='avg.ed',ys='growth' )

#=======================================================

# function to plot nulls from 2 strata linear relation 
# function reutrns p simulated data sets: 1 alternative and p-1 from null 
sim_case1 <- function(p = 12, b_alt= 1, b_null=0, sigma=5 ) { 
  N <- c(5000,5000)
  n <- c( 100, 500)
  x1 <- runif(N[1], 10, 20)
  x2 <- runif(N[2], 20, 30)
  x <- c(x1,x2)

  y_alt  <- 10 + b_alt*(x-mean(x))  + rnorm(sum(N), mean = 0, sd = sigma)
  y_null <- 10 + b_null*(x-mean(x)) + rnorm(sum(N), mean = 0, sd = sigma)
  
  u <- data.frame(y_alt=y_alt, y_null=y_null, x=x) |> 
    mutate( estrato=ifelse(x<20, 1, 2), 
  aux = dnorm(x, mean=24, sd= 4))
  #aux = 1/(1+abs(x-22) ) ) 
  #ggplot(u) + geom_line( aes(x, aux))

#  samples  
pos <- sample(1:p, 1)

smps <- vector(mode = 'list', length=p)
  for (i in 1:p) {
  s <- strata(u, stratanames = "estrato", size = n, method = "systematic", pik = u$aux)

  smps[[i]] <- u |> slice( s$ID_unit ) |> 
    mutate( wts = 1/s$Prob, .sample=i ) |> 
    mutate(y =  (i==pos)*y_alt + (i!=pos)*y_null) |> 
    select(.sample, y, x, estrato, wts)
  }  
  o <- list( sdt = smps |> bind_rows(), pos=pos)

  return(o)
}

