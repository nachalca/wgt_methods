

#####################################################################
## true random stuff ############ 
#####################################################################

# trying a simulation  ----------------------


p = 10
n = 10000
gamma = seq(-2,2,length.out=p)
X.y = matrix(rnorm(n*p),p, n)

beta = rep(1/sqrt(p),p)
EX.d = beta%*%X.y
X.d = EX.d + 0.5*rnorm(n)

q = exp(sin(X.d))/(1+ exp(sin(X.d))) # unequal probability of selection
Y = sin(gamma%*%X.y)  - 5*EX.d # response 

# I guess the key is Xd impact both Y and q 

dt <- data.frame(Y=as.numeric(Y), 
                  Xd = as.numeric(X.d[1,]), 
                  q=q[1,])

ggplot(dt) + geom_point(aes(Xd, Y, color=q))


# Get a SRS from population 

idx <- sample(1:n, size = 500, prob = dt$q)

dt.smp <- dt[idx, ]

p6 <- ggplot(dt) + geom_point(aes(y = Y, x = Xd), alpha=1/50 ) + 
  theme(aspect.ratio = 1) + labs(title = 'population', x = '', y = '')

p <- ggplot(dt.smp, aes(x=Xd, y=Y)) + theme(aspect.ratio = 1)
  
p5 <- p + geom_point() + labs(title='Exp sample', x = '', y = '') 


p2 <- ggplot(apistrat) + geom_point(aes(y = growth, x = avg.ed, size = wts), shape = 1 ) + theme(aspect.ratio = 1) +
  labs(title = 'bubble', , x = '', y = '')

p3 <- ggplot(apistrat) + geom_point(aes(y = growth, x = avg.ed, alpha = wts)) + theme(aspect.ratio = 1) +
  labs(title ='transparency', x = '', y = '')

p4 <- ggplot(apistrat, aes(y = growth, x = avg.ed, z = wts)) +
  theme(aspect.ratio = 1 ) + stat_summary_hex(  ) + labs(title = 'hex', x = '', y = '')

ids <- sample(1:nrow(apistrat), replace = TRUE, prob = wts )
p1 <- ggplot(apistrat) + geom_point(aes(y = growth, x = avg.ed) ) +
  theme(aspect.ratio = 1) +
  labs(title='subsampling', x = '', y = '')

(p6 + p1 + p2)/ (p5+p3+p4)  + 
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") & 
  theme(legend.position = "none")





#####################################################################
# api example --------------------------------
library(patchwork)
library(tidyverse)
library(survey)

data(api)

# Entire population
glimpse(apipop)

p6 <- ggplot(apipop) + geom_point(aes(y=api00, x=avg.ed) ) + labs(title='population', x = '', y = '')

ggplot(apipop) + geom_smooth(aes(y=api00, x=avg.ed) )

with(apipop, cor(api00, avg.ed, use = 'complete.obs') )


# Stratified sample 
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

rr <-  svyvar(~api00+avg.ed, dstrat) |> as.matrix() |>cov2cor() 
rr[2,1]


stys <- c("bubble", "hex", "grayhex","subsample","transparent")
plts <- vector(mode = 'list', length=5)
for (i in 1:5) {
plts[[i]] <- svyplot(api00~api99, design=dstrat, style=stys[i], xlab="1999 API",ylab="2000 API",legend=0)
}

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

rr <-  svyvar(~api00+avg.ed, dstrat) |> as.matrix() |>cov2cor() 
rr[2,1]

wts <- 1/dstrat$prob 

p6 <- ggplot(apipop) + geom_point(aes(y=api00, x=avg.ed) ) + labs(title='population', x = '', y = '')

 p5 <- ggplot(apistrat) + geom_point(aes(y=api00, x=avg.ed) ) + labs(title='sample', x = '', y = '') 

p2 <- ggplot(apistrat) + geom_point(aes(y=api00, x=avg.ed, size=wts), shape=1 ) + labs(title='bubble', , x = '', y = '')

p3 <- ggplot(apistrat) + geom_point(aes(y=api00, x=avg.ed, alpha=wts)) + labs(title ='transparency', x = '', y = '')

p4 <- ggplot(apistrat, aes(y=api00, x=avg.ed, z=wts)) + stat_summary_hex(  ) + labs(title='hex', x = '', y = '')

ids <- sample(1:nrow(apistrat), replace = TRUE, prob = wts )
p1 <- ggplot(apistrat) + geom_point(aes(y=api00, x=avg.ed) ) + labs(title='subsampling', x = '', y = '')

(p6 + p1 + p2)/ (p5+p3+p4)  + 
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") & 
  theme(legend.position = "none")
#=========================================================


#### compare weighted and unweighted analyses in linear model for dietary sodium
sodiumwt<-svyglm(sodium~age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),design=des)
sodiumunwt<-glm(sodium~age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),data=model.frame(des))
sodiumclus<-lmer(sodium~(1|realpsu)+age1+age2+age3+factor(RIDRETH1)*factor(RIAGENDR),data=model.frame(des))

betas<- data.frame( wt=coef(sodiumwt), unwt=coef(sodiumunwt),clus=fixef(sodiumclus)) 

betas |> mutate( rr = wt/unwt) |> arrange(desc( abs(rr) ) )


ses<-round(cbind(SE(sodiumwt),SE(sodiumunwt),sqrt(diag(as.matrix(vcov(sodiumclus))))),3)


wts <- 1/des$prob 

dd <- nhanes |> 
  filter(!is.na(WTDRD1)) |> 
  mutate(wts = 1/des$prob) |>  filter( RIDRETH1==2) 

pp <- ggplot(dd, aes(y = BPXSAR, x = DR1TSODI)) +  theme(aspect.ratio = 1) + labs(title = 'sample', x = '', y = '') + theme(legend.position = 'none')

 pp + geom_point()

 pp + geom_point(aes(size=wts), shape=1) + labs(title='bubble')

pp + geom_point(aes(alpha=wts) ) + labs(title='transparency')

p3 <- ggplot(apistrat) + geom_point(aes(y = growth, x = avg.ed, alpha = wts)) + theme(aspect.ratio = 1) +
  labs(title ='transparency', x = '', y = '')

p4 <- ggplot(apistrat, aes(y = growth, x = avg.ed, z = wts)) +
  theme(aspect.ratio = 1 ) + stat_summary_hex(  ) + labs(title = 'hex', x = '', y = '')

ids <- sample(1:nrow(apistrat), replace = TRUE, prob = wts )
p1 <- ggplot(apistrat) + geom_point(aes(y = growth, x = avg.ed) ) +
  theme(aspect.ratio = 1) +
  labs(title='subsampling', x = '', y = '')

(p6 + p1 + p2)/ (p5+p3+p4)  + 
  plot_layout(guides = "collect", axes = "collect", axis_titles = "collect") & 
  theme(legend.position = "none")


#######################################################


library(survey)

getNamespaceExports( 'survey' )

methods('svyplot')

getAnywhere("svyplot")

install.packages('ggsurvey')

library(cowplot)

library(ggsurvey) # has scatter plot functions we can acces the code

data(api)
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

rr <-  svyvar(~api00+avg.ed, dstrat) |> as.matrix() |>cov2cor() 
paste('True Correlation', rr[2,1])

wts <- 1/dstrat$prob 

ww <- weights(dstrat, "sampling")

gghexweight(apistrat, avg.ed,api00, wts)  + theme(aspect.ratio = 1) 

ggplot(apistrat, aes(y = api00, x = avg.ed)) +
   geom_hex(aes(weight = wts)) +
  labs(title = 'hex') + 
  scale_fill_gradient(low = '#dadada', high='#333333') +
  theme(aspect.ratio = 1) +  theme_bw()

svyplot(api00~avg.ed,style="grayhex",design=dstrat,legend=0, xlab='avg.ed', ylab='api00')

svyplot(api00~api99,style="transparent",design=dclus2wr,legend=0, xlab='avg.ed', ylab='api00', pch=19)


#=================================

library(tidyverse)


ggplot(apistrat) + 
  geom_jitter(aes(snum,pw, color=stype), width=10)


ggplot(apipop) + 
  geom_point(aes(dnum, growth, color=stype) ) + 
  facet_wrap(~stype)


# =====================================