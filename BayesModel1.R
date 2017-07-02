library(rjags)
library(R2WinBUGS)
model.bugs <- function()
{
  for (i in 1:N) {
    mean.Y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta[1] + 
      (beta[2] + beta[3] * X[i])* Age[i]
  }
  for (k in 1:N.beta) {
    beta[k] ~ dunif(-1.0E+4, 1.0E+4)
  }
  tau <- 1 / (sd * sd)
  sd ~ dunif(0, 1.0E+4)
}

file.model <- "model.bug.txt"
write.model(model.bugs, file.model)



load("data.RData")
N.beta <- 3
list.data <- list(
  mean.Y = d$mean.Y,
  X = d$X, Age = d$Age,
  N = nrow(d), N.beta = N.beta
)
list.inits <- list(beta = rep(0, N.beta),
                   sd = 1)

n.burnin <- 1000
n.chain <- 3
n.thin <- 10
n.iter <- n.thin * 3000

model <- jags.model(
  file = file.model,
  data = list.data,
  inits = list.inits,
  n.chain = n.chain
)
update(model, n.burnin)

post.mcmc.list <- coda.samples(
  model = model,
  variable.names = c("mu", names(list.inits)),
  n.iter = n.iter,
  thin = n.thin
)
