# packages.
packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior", "qs","bivpois",
"MASS")
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
att_sig <- 0.25
def_sig <- 0.25
rho <- 0.7 # better attack teams have better defenses.

cov_ad <- att_sig * def_sig* rho
MU <- c(att_bar, def_bar)
Sigma <- matrix(c(att_sig^2, cov_ad, cov_ad, def_sig^2), ncol = 2)

ad_effects <- mvrnorm(20, MU, Sigma)
att_effect <- ad_effects[,1]
def_effect <- ad_effects[,2]

#plot the correlation between att and def effects.
plot(att_effect, def_effect)

# Simulating match scores from bivariate poisson.

d$lambda_3 <- 0.02 # Additional 5% more goals come is game related effect on average.
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


## Let's see if the bivariate Poisson complex model retrodicts the 
## of our model.

dat_complex_bivar <-  list(
  Nt = 20,
  N = 370,
  ht = d$ht[1:370],
  at = d$at[1:370],
  s1 = d$s1[1:370],
  s2 = d$s2[1:370],
  
  # for predictions
  Np = 10,
  htp = ht[371:380],
  atp = at[371:380]
)


model_bivar_complex <- cmdstan_model("code/bivariate_poisson_complex.stan")
model_bivar_complex_fit <- model_bivar_complex$sample(
  data = dat_complex_bivar,
  chains = 4, parallel_chains = 4,
  refresh = 500
)


# Posterior Predictive Check:

model_bivar_complex_fit$draws(variables = c("att", "def"), format = "df") |> 
  summarize(across(1:20, .fns = mean, .names = "{.col}")) |> 
  pivot_longer(everything())  |> 
  rename(bivar = value, x = name) |> 
  bind_cols(att_effect = att_effect, teams = seq(1, 20, 1)) |> 
  as_tibble() |> 
  pivot_longer(c(bivar, att_effect)) |> 
  rename(model = name) |> 
  ggplot() +
  geom_point(aes(x = teams, y = value, color = model), size = 4)+
  scale_color_manual(
    values = c("bivar" = "black", "att_effect" = "red")
  )+
  theme_classic()




# TODO [] Write and check for a simpler model.
# TODO [] Document the difference in attack and defence params in the bivariate poisson
# model as an explanation for crowding out of the parameter space.
# TODO [] Have goal difference determine the lambda_3 i.e. covariance in goals. 

