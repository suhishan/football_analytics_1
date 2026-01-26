# packages.
packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior", "qs","bivpois",
"mvtnorm")
lapply(packages, library, character.only = TRUE)


d <- data.frame(
  # data from the actual fixture list.
  ht = ht, 
  at = at,
  home = 0.3 # 22% more goals scored at home than away.
)

# Joint distribution of attack and defence parameters.

att_bar <- 0.1
def_bar <- -0.05
att_sig <- 0.05
def_sig <- 0.05
rho <- 0.7 # better attack teams have better defenses.

cov_ad <- att_sig * def_sig* rho
MU <- c(att_bar, def_bar)
Sigma <- matrix(c(att_sig^2, cov_ad, cov_ad, def_sig^2), ncol = 2)

ad_effects <- mvrnorm(20, MU, Sigma)
att_effect <- ad_effects[,1]
def_effect <- ad_effects[,2]


# Simulating match scores from bivariate poisson.

d$lambda_3 <- 0 # Additional 5% more goals come is game related effect on average.
d$lambda_1 <- exp(d$home + att_effect[d$ht] - def_effect[d$at])
d$lambda_2 <- exp(att_effect[d$at] - def_effect[d$ht])

scores <- rbp(n = 380, lambda = c(d$lambda_1, d$lambda_2, d$lambda_3))

d$s1 <- scores[,1]
d$s2 <- scores[,2]


# Let's look at the PL table then

d <- d |> mutate(
  home_points = case_when(
    s1 > s2 ~ 3,
    s1 == s2 ~ 1,
    .default = 0
  ),
  away_points = case_when(
    s2 > s1 ~ 3,
    s2 == s1 ~ 1,
    .default = 0
  )
)

home_table <- d |> group_by(ht) |> summarize(hp = sum(home_points))
away_table <- d |> group_by(at) |> summarize(ap = sum(away_points))

cbind(home_table, away_table) |> mutate(tp = hp + ap) |> arrange(desc(tp))

d |> pivot_longer(c(s1, s2)) |> 
  ggplot(aes(x = value))+
  geom_bar(aes(color = name, fill = name),position = "dodge")+
  theme_classic()

d |> summarize(mean(s1), mean(s2))


# Bivariate Poisson Log Probability Mass function

# Checking for normal distribution
normal_pdf <- function(x, mu, s, l) {
  if (l == TRUE) {
    a <- log(1 / sqrt(2 * pi * s^2)) - (((x - mu)^2)/ (2 * s^2))
  } else {
    a <- (1 / sqrt(2 * pi * s^2) ) * exp(-(x - mu)^2 / (2 * s^2)) 
  }
  return (a)
}


bivar_poisson_lpmf <- function(x, y, l_1, l_2, l_3) {
  mn <- min(x, y)
  con <- -l_1 - l_2 - l_3 + (x * log(l_1)) + (y * log(l_2)) - lfactorial(x) - lfactorial(y) 

  f <- numeric(length(mn) + 1)
  for (k in 0:mn) {
    f[k+1] <- choose(x, k)  * choose(y, k) * factorial(k) * (l_3/ (l_1 * l_2)) ^ k
  }

  f <- log(sum(f))
  a <- con + f

  return (a)
}

# A different way to write the same thing. (A cleaner and simpler way)

bivar_poisson_lpmf_2 <- function(x,y, lambda) {
  l_1  <- lambda[1] ;l_2 <- lambda[2]; l_3 <- lambda[3]
  mn  <- min(x, y)
  f <- numeric(length(mn) + 1)

  for (k in 0:mn) {
    f[k + 1] <- dpois(k, l_3) * dpois(x-k, l_1) * dpois(y-k, l_2)
  }
  a <- log(sum(f))
  return (a)
}

# Better more efficient way

bivar_poisson_lpmf_3 <- function(x, y, lambda) {
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
    a <- log(sum(exp(f)))
    return(a)
}


# Checking if this returns values as expected.

sapply(seq(0, 5), function(x) dbp(x1 = x, x2 = rep(2, 6), c(1, 2, 3)))
sapply(seq(0, 5), function(x) bivar_poisson_lpdf_2(x = x, y = 2, c(1, 2, 3)))

# Workss.


# Calculating log-likelihood

d <- data.frame(rbp(10, lambda = c(1, 2, 1)))
colnames(d) <- c("x", "y")

l <- tibble(l_1 = seq(0, 4, by = 1)) |> expand_grid(l_2 = seq(0, 4, 1)) |> 
  expand_grid(l_3 = seq(0, 4, 1))

lik_calc <- function(data, l_1, l_2, l_3, func) {
  a <- sum(pmap_dbl(data, func, lambda = c(l_1, l_2, l_3)))
  return(a)
}


l <- list(l_1 = l$l_1, l_2 = l$l_2, l_3 = l$l_3)
a <- list(x = d$x, y = d$y)

#
# 
# sum(pmap_dbl(a, bivar_poisson_lpmf, lambda = c(1, 2, 3)))

pmap_dbl(l, lik_calc, data = a, func = bivar_poisson_lpmf) |> cbind(data.frame(l))
