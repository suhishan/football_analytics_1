# Generative model.
# for every home team,
  # pair with every other team except itself. 
  # 20 * 19 = 380


df <- tibble(
  HomeTeam = 1:20,
) |> crossing(AwayTeam = 1:20) |> 
  filter(HomeTeam != AwayTeam) |> 
  mutate(gameweek = rep(1:38, times = 10)) |> 
  sample_frac(gameweek, size = 1, replace = FALSE) |> 
  arrange(gameweek)
View(df)








# Most Likely Score code.
tibble(
  s1_mostlikely = apply(m2post[,1:10], 2, function(x) which.max(tabulate(x))), # finding the mode.
  s2_mostlikely = apply(m2post[,11:20], 2, function(x) which.max(tabulate(x))) # finding the mode.
) %>% bind_cols(data %>% tail(n=10))


a <- matrix(nrow = 12, ncol = 2)






# ------ Sample example of fixture list ------ #

d <- matrix(data = 0, nrow = 0, ncol = 2)
colnames(d) <- c("HomeTeam", "AwayTeam")

store_h <- c(1)
store_a <- c(2)
d <- rbind(d, c(store_h, store_a))
for (i in 1:11) {
  h <- sample(1:4, 1)
  a <- sample(1:4, 1)
  d <- rbind(d, c(h, a))
}

gameweek_1 <- matrix(sample(1:20, size = 20, replace = FALSE), 10, 2)

overall <- matrix(data = 0, nrow = 0, ncol = 2)
overall <- rbind(overall, gameweek_1)


# ROugh work to understand Bivariate Poisson distribution

sample_d <- rbp(380, lambda = c(1.2, 1, 0.2))
cov(sample_d[,1], sample_d[,2])

x <- 1; y <- 1; l_1 <- 1; l_2 <- 2; l_3 <- 3
mn <- min(x, y)
  con <- -l_1 - l_2 - l_3 + (x * log(l_1)) + (y * log(l_2)) - lfactorial(x) - lfactorial(y) 

  f <- numeric(length(mn) + 1)
  for (k in 0:mn) {
    f[k+1] <- choose(x, k)  * choose(y, k) * factorial(k) * (l_3/ (l_1 * l_2))^k
  }

  f <- log(sum(f))

  a <- con + f;a



# Optimized algorithm

lambda<- c(1, 2, 3)
x<-1; y<- 2
l_1 <- log(lambda[1]); l_2 <- log(lambda[2]); l_3 <- log(lambda[3]);
    mn <- min(x, y)

    f <- numeric()
    # when l_3 is 0
    f[1] <- dpois(x, exp(l_1), log = TRUE) + dpois(y, exp(l_2), log = TRUE) -
        exp(l_3) # log(exp(-exp(l_3)))
    if (mn > 0) {
        cons <- -l_1 - l_2 + l_3
        for (i in 1:mn) {
            f[i+1] <- f[i] + log(x-i+1) + log(y-i+1) + cons - log(i)
        }
    }
    a <- sum(exp(f))



#Trying sapply functions for likelihood.

d <- data.frame(rbp(100, lambda = c(1, 2, 1)))
colnames(d) <- c("x", "y")

l <- tibble(l_1 = seq(0.1, 4, by = .1)) |> expand_grid(l_2 = seq(0.1, 4, .1)) |> 
  expand_grid(l_3 = seq(0.1, 4, .1))


lik_calc <- function(data, l_1, l_2, l_3, func) {
  a <- sum(pmap_dbl(data, func, lambda = c(l_1, l_2, l_3)))
  return(a)
}


l <- list(
  l_1 = l$l_1, l_2 = l$l_2, l_3 = l$l_3,
)
a <- list(x = d$x, y = d$y)

# sum(pmap_dbl(a, bivar_poisson_lpmf, lambda = c(1, 2, 3)))

library(tictoc)
tic("Model1")
log_lik <- pmap_dbl(l, lik_calc, data = a, func = bivar_poisson_lpmf)
toc()

tic("Model2")
log_lik_2 <- pmap_dbl(l, lik_calc, data = a, func = bivar_poisson_lpmf_2)
toc()

tic("Model 3")
log_lik_3 <- pmap_dbl(l, lik_calc, data = a, func = bivar_poisson_lpmf_3)
toc()


log_lik <- pmap_dbl(l, lik_calc, data = a, func = bivar_poisson_lpmf_3)
cbind(log_lik, l)[which(log_lik == max(log_lik)),]






# Understanding lambda_3 in a bivariate poisson disitrbution.

 
bivar_rng = rbp(100, lambda = c(1.7, 1.4, 0.02))
hg = bivar_rng[,1] ;ag = bivar_rng[,2]
cor(hg, ag)




# Geom Contours.

library(mvtnorm)
m <- c(.1, -.2)
sigma <- matrix(c(0.26,.04,.04,0.245), nrow=2)
data.grid <- expand.grid(s.1 = seq(-1, 1, length.out=100), 
s.2 = seq(-1, 1, length.out=100))
q.samp <- cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m, sigma = sigma))
ggplot(q.samp, aes(x=s.1, y=s.2, z=prob)) + 
    geom_contour_filled() +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1), ratio = 1) 



m <- c(mean(m_corr_ad$att_bar), mean(m_corr_ad$def_bar))
cov <- with(m_corr_ad, sigma_team.1. * sigma_team.2. * Rho.1.2.)
sigma <- matrix(c(mean(m_corr_ad$sigma_team.1.), mean(cov), mean(cov), mean(m_corr_ad$sigma_team.2.)), nrow = 2)

d.grid <- expand.grid(att = seq(-0.8, 0.8, length.out = 100), 
  def = seq(-.8, .8, length.out = 100))

d.contour <- cbind(d.grid, p =dmvnorm(d.grid, m, sigma))

ggplot()+
  geom_contour_filled(data = d.contour,aes(x = att, y = def, z = p,
  alpha = 0.5, color = "white"), bins = 8)+
  scale_fill_brewer(palette = "RdPu")+
  theme_bw()+
  theme(legend.position = "none")
