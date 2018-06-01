if (!require("pacman")){install.packages("pacman")}
pacman::p_load(ggplot2) 

#Setting seed for testing purposes
set.seed(11)

#Setting up fake data
N_sim = 5000
n = 100
p = 5
beta_vec = c(5,4,3,2,1)
alpha = 0.95
X = matrix(runif(n*p),nrow = n, ncol = p)

#Adding z column (unknown features) to X
z = runif(n)
beta_z = 3.4
Xz = cbind(X, z)

#Setting up inidcator t (CR)
indic_t = sample(c(rep(1, n/2), rep(0, n/2)))
beta_t = 6.3

#Creating y vector
y = X %*% (alpha*beta_vec) + z * beta_z + indic_t * beta_t

#Testing R^2 of X
x_mod = lm(y ~ X, data.frame(X))
cat(summary(x_mod)$r.squared)

#Testing R^2 of X + z
xz_mod = lm(y ~ Xz, data.frame(Xz))
cat(summary(xz_mod)$r.squared)

#Creating matrix of indic_t's to find max eigenvalue (CR)
results = matrix(NA, nrow = N_sim, ncol = n)
max_lambda = vector(mode = "numeric", length = N_sim)
for(i in 1:N_sim){
  results[i, ] = sample(c(rep(1, n/2), rep(0, n/2)))
  #var-cov matrix has issues if there aren't enough rows. Need to replace na's with 0's
  s = cor(results[(1:i), , drop = FALSE], use = "complete.obs")
  s = replace(s, is.na(s), 0)
  max_lambda[i] = max(eigen(s)$values)
}

#Plotting N_sim vs max lambda to look for convergence

ggplot(data.frame(max_lambda), aes(x = 1:N_sim, y = max_lambda)) + geom_point(size = 0.5) + 
  xlab("Number of Simulations") + ylab("Maximum Eigenvalue of var-cov Matrix") +
  ggtitle("Number of Simulations vs Maximum Eigenvalue", subtitle = "For Complete Randomization Allocation Vectors")+
  ylim(1, 5)

#ggsave("plot.png")
#system("open plot.png")

#It would seem that the max eigenvalue does not converge for 5000 simulations