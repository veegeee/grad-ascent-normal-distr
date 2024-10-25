#generating 100 datapoints from normal distribution with mean=6 and sd=4
a <- rnorm(100, 6, 4)
n <- 100
#size of the step in the direction gradient points at
step_size <- 0.001
#starting values mean and sd
mu <- 1
sig <- 1
#number of iterations algorithm
iter<-10000

#vector with starting values mean and sd
vec_mu_sig <- c(mu, sig)
#create dataframe to save estimates
#number of iterations + 1 because of starting values
all_estimates<-data.frame(mu = rep(0,(iter+1)), sig = rep(0,(iter+1)))
#setting first row equal to starting values
all_estimates[1,]<-vec_mu_sig
#data frame containing gradient of each iteration
gradients <- data.frame(mu=rep(0,iter), sig=rep(0,iter))

for(i in 1:iter){
#partial derivative mean
  deriv_mu <-  1/vec_mu_sig[2]^2 * sum(a - vec_mu_sig[1])
#partial derivative sd
  deriv_sig <- -n/vec_mu_sig[2] + 1/vec_mu_sig[2]^3 * sum((a - vec_mu_sig[1])^2)

  gradient <- c(deriv_mu, deriv_sig)
  vec_mu_sig <- vec_mu_sig + step_size * gradient
  
  gradients[i,] <- gradient
  all_estimates[(1+i),] <- vec_mu_sig
}

plot(all_estimates$mu, type = 'l', ylim = c(0,10), xlim = c(0, (iter+1)))
plot(all_estimates$sig, type = 'l', ylim = c(0,10), xlim = c(0, iter+1))
