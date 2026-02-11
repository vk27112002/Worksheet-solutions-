########## Q1a ########
running_mean <- function(all.samples){
  n <- length(all.samples)
  rtn <- cumsum(all.samples)/(1:n)
  return(rtn)
}
n <- 1e4
samples <- rt(n ,df = 3)
run_mean <- running_mean(samples)
true_mean <- 0
plot(1:n,run_mean,col="blue",type = "l")
abline(h=true_mean, lty =2)

########## 1b #######

n <- 1e4
samples <- rt(n ,df = 2)
run_mean <- running_mean(samples)
true_mean <- 0
plot(1:n,run_mean,col="blue",type = "l")
abline(h=true_mean, lty =2)

######## 1c #######

n <- 1e4
samples <- rt(n ,df = 1)
run_mean <- running_mean(samples)
true_mean <- 0
plot(1:n,run_mean,col="blue",type = "l")
abline(h=true_mean, lty =2)

######### Q2 ###########

n <- 100
r <- 100
samples <- matrix(0,n,r)
for(i in 1:r){
  samples[,i] <- rnorm(n,0,1)
}
true <- 0
means_reps <- as.numeric(colMeans(samples))
plot(1:r,colMeans(samples),type="l",col="red",ylim = c(-0.5,0.5))
abline( h=true,lty = 2)
obs_mean <- mean(colMeans(samples))
obs_var <- var(means_reps)
cat(obs_mean,"and true mean is 0 and obs var is",obs_var,"true var is 0.05")

####### Q2 ##########

n <- 20
samples <- rnorm(n)
mean(samples)

reps <- 100
store_means <- length(reps)
for(r in 1:reps){
  samp <- rnorm(n)
  store_means[r] <- mean(samp)
}
mean(store_means)

###### variance #########

est_var <- var(store_means)
var <- 1/n
est_var
var


########### Q3 ##############
n <- 100
reps <- 500
true_mean <- 0
true.sd <- 1
store_means <- length(reps)
for(r in 1:reps){
  samp <- rnorm(n)
  foo <- mean(samp)
  store_means[r] <- sqrt(n) * (foo-0)
}
grid <- seq(-5,5,length=100)
true.den <- dnorm(grid,sd=1)
hist(store_means,freq = FALSE)
lines(grid,true.den,col="blue")

######## t dist ##########

n <- 100
reps <- 500
true_mean <- 0
true.sd <- sqrt(3/(3-2))
store_means <- length(reps)
for(r in 1:reps){
  samp <- rt(n,3)
  foo <- mean(samp)
  store_means[r] <- sqrt(n) * (foo-true_mean)
}
grid <- seq(-5,5,length=100)
true.den <- dnorm(grid,sd=true.sd)
hist(store_means,freq = FALSE)
lines(grid,true.den,col="blue")





























