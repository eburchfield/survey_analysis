rm(list = ls())

if ( ! require (rjags )) stop ("Could not load rjags")
if ( ! require (lme4 )) stop ("Could not load lme4")
if ( ! require (ggplot2 )) stop ("Could not load ggplot2")
if ( ! require (ggmcmc )) stop ("Could not load ggmcmc")
if ( ! require (stringr )) stop ("Could not load stringr")
if ( ! require (xtable )) stop ("Could not load xtable")
if ( ! require (BEST )) stop ("Could not load BEST")
if ( ! require(foreign)) stop ("Could not load foreign")


d = "C:\\Users\\Emily Burchfield\\Box Sync\\WF\\Survey\\SEADS HH Pilot Cohort_for emily.sav"

#missing data at the end dropped for now (up to 278)
mydata <- read.spss(d, to.data.frame=T)
ses <- mydata$SESIndex_3[0:278]
position <- mydata$LAN2B.1[0:278]
pos <- addNA(position)
GN <- mydata$HIQ1_CODE[0:278]

#working with factors
#levels(pos)
#table(pos)
#boxplot
#plot(mydata$SESIndex_3~mydata$HIQ1_CODE, xlab = " ", ylab = "SES", main = "Boxplots of SES by Community", las=2)


#prepare data
n <- length(ses)

###############
#replace GN with position
##############
county.name <- as.vector(GN) #NAs

uniq <- unique(county.name)
n.counties <- length(uniq)
county <- rep(NA,n.counties)
for (i in 1:n.counties){
  county[county.name == uniq[i]] <- i
  sample.size <- as.vector(table(county))
}


ybarbar = mean(ses)

#add some noise to sample size for plotting
sample.size.jittered <- sample.size*exp (runif (n.counties, -.1, .1))
#group mean and variance
cty.mns <- tapply(ses, county, mean)
cty.vars <- tapply(ses, county, var)
cty.sds <- mean ( sqrt (cty.vars[! is.na(cty.vars )])) / sqrt ( sample.size )
cty.sds.sep <- sqrt ( tapply (ses,county , var ) / sample.size )
y.limits <- c (1 ,35)

y <- c1$agrowell_user  #replace later

#lmer
M0 <- lmer(y ~ 1 + (1|c1$HI4_Name))
summary(M0)
#coef(M0)$

#jags
library(rjags)

#model specifications
model_string <- "model {
  for (i in 1:n){
    y[i] ~ dnorm (a[county[i]], tau.y)
}

tau.y <- pow(sigma.y , -2)
sigma.y ~ dunif (0, 100)
for (j in 1: n.counties ){
  a[j] ~ dnorm (mu.a , tau.a)
}
mu.a ~ dnorm (0, .0001)
tau.a <- pow(sigma.a , -2)
sigma.a ~ dunif (0, 100)
}"

#cauchy prior

#initial values randomly drawn from distributions
#jags initiates but jg added initiation; not required
ses.inits <- function(chain){
  list (a=rnorm(n.counties), mu.a = rnorm(1),
        sigma.y = runif(1), sigma.a = runif(1))
}


#prepare list of data to pass to JAGS model
#building dataframe (jagsname = localvariable)
ses.data <- list(n=n, n.counties = n.counties, y=y, county = county)


#tell jags parameter names
ses.parameters <- c("a", "mu.a", "sigma.y", "sigma.a")

#compile model and initialize data/parameters
mlm.ses.nopred.model <- jags.model(textConnection(model_string),
                                   data = ses.data,
                                   inits = ses.inits,
                                   n.chains = 3,
                                   n.adapt = 1000)
###
#chains - MCMC, start with different initial values 3 times - Gibbs in Kruschke
#potential scale reduction parameter (rhat) Gelman - measures ratio of win chain variance over between chain variance
#rhat should be really close to 1 (no more than 1.01); how many iterations for MCMC before confident
#that explored enough of spcae to be true representation of posterior (posterior is histogram of all values chain visited)
#chain spends more time in high probability density places
###
#adapt - how far do you want to look in the space and what probability to use; first 1000 steps spent
#tweaking the MC algorithm so that at everystep MC parameters changed; tuning the MC


#take 2000 random samples
update(mlm.ses.nopred.model, n.iter = 2000)  #throws away 1st half

mlm.ses.nopred <- coda.samples(mlm.ses.nopred.model,
                               variable.names = ses.parameters,
                               n.iter = 2000)

#coda - take output and turn it into format R can handle

#discuss results from#
summary(mlm.ses.nopred)
#ggmcmc plotting outputs


#get data from jags
post.nopred <- as.matrix(mlm.ses.nopred)  #belief in parameters given data
mean.a.nopred <- rep(NA, n.counties)  #mean for each county
sd.a.nopred <- rep(NA, n.counties)  #sd for each county
for (i in 1:n.counties) {
  mean.a.nopred[i] <- mean(post.nopred[ , paste ('a[',i,']', sep='')])
  sd.a.nopred[i] <- sd(post.nopred [ , paste ('a[',i,']', sep='')])
}

#if autocorrelation, up iteratoins - rhat
#thin parameter in update (helps with memory) - keeps fewer iterations; thin = 20 if ac goes out to 20
#better not to thin if possible

################################################################
#adding predictor
###############################################################

#need to replace NAs in POS with something else

lm.pooled <- lm(y ~ pos)
lm.unpooled <- lm(y~pos + factor(county) -1)

#jags with individual level predictor
#####instead of a only, y now includes effect of pos in mean estimate 
#county level info does not change.
model_string_pred <- "model {
  for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)  
    y.hat[i] <- a[county[i]] + b*pos[i]
  }
  b ~ dnorm(0,0.0001)
  tau.y <- pow(sigma.y, -2)
  sigma.y ~ dunif(0,100)

  for (j in 1:n.counties){
    a[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0,0.0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif(0,100)}"


#################function chain?  jg.seeds##########################


#jg seeds is list of random number seeds

#pi is function that takes argument chain
predictor.inits <- function(chain){
    list (a=rnorm( n.counties ), b=rnorm(1),
          mu.a= rnorm(1), sigma.y=runif(1), sigma.a=runif(1))
  }

ses.pred.data <-  list(n=n, n.counties=n.counties, y=y, 
                       county=county, pos=pos)

ses.pred.parameters <- c("a", "b", "mu.a", "sigma.y", "sigma.a")

mlm.ses.pred.model <- jags.model(textConnection(model_string_pred),
                                 data = ses.pred.data,
                                 inits = predictor.inits,
                                 n.chains = 3,
                                 n.adapt=1000)

update(mlm.ses.pred.model, niter=2000)
mlm.ses.pred <- coda.samples(mlm.ses.pred.model, variable.names = ses.pred.parameters,
                               n.iter = 2000)

#extract useful data
post.pred <- as.matrix(mlm.ses.pred)  #one row for every sample in chain
alphavarvar <- mean(post.pred[, 'mu.a'])
mean.a.pred <- rep(NA, n.counties)
sd.a.pred <- rep(NA,n.counties)

#syn
for (i in 1:n.counties) {
  mean.a.pred[i] <- mean(post.pred[ ,paste('a[',i,']', sep='')])
  sd.a.pred[i] <- sd(post.pred[ ,paste('a[',i,']', sep='')])
}
 
#look into:
#dplyr <- working with dataframes; select which columns working with, grouping
#summarize, filter data
#grouping descriptive stats based on groups
#data structure, formatting 
#tidyr <- reshape data, look at all crop yield together tho in 2 columns
#gather function in tidyr, gathers all data to take columns Maha, Yala, etc 
#and put all values into one column called values
#mean(data, na.rm=TRUE)                      