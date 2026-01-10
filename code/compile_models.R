packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior", "qs","bivpois")
lapply(packages, library, character.only = TRUE)

#--- Load the data and make it usable ---#

data <-  read.csv("data/premfull23_24.csv")

data = data %>% select(HomeTeam, HomeGoals, AwayGoals, AwayTeam) %>% 
  rename(
    Home = HomeTeam,
    s1 = HomeGoals,
    s2 = AwayGoals,
    Away = AwayTeam
  )

# Recode team names.
lookup <- c(
     "Brighton and Hove Albion" = "Brighton",
     "Manchester Utd" = "Manchester United",
     "Newcastle Utd" = "Newcastle United",
     "Nott'ham Forest" = "Nottingham Forest",
     "Sheffield Utd" = "Sheffield United",
     "Tottenham Hotspur" = "Tottenham",
     "West Ham United" = "West Ham",
     "Wolverhampton Wanderers" = "Wolves"
)

data = data %>% mutate(
  Home = recode(Home, !!!lookup),
  Away = recode(Away, !!!lookup)
)


#--- Model fit (complete pooling) ---#

teams = unique(data$Home) # Team names
ng = nrow(data) # number of games
nt = length(unique(data$Home)) # number of teams
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g]))) # Home team index
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g]))) # Away team index

np = 10 # Number of games to be predicted i.e. test sample length
ngd = ng - np # Number of games used to train i.e. train sample length.


dat <-  list(
  nt = nt,
  ng = ngd,
  ht = ht[1:ngd],
  at = at[1:ngd],
  s1 = data$s1[1:ngd],
  s2 = data$s2[1:ngd],
  
  # for predictions
  np = np,
  htp = ht[(ngd+1):ng],
  atp = at[(ngd+1):ng]
)

#--- Compile and Sample the model ---#
m1 <- cmdstan_model("code/normal_model.stan")

m1fit <- m1$sample(
  data = dat, chains = 8, parallel_chains = 8, refresh = 500
)

# Save the fitted model
#qsave(x = m1fit, file = 'fits/normal_model.qs')
#m1fit <- qread("fits/normal_model.qs")
#--- Working with posterior draws ---#

m1post = m1fit$draws(variables = c("s1p", "s2p"), format = "df")

# Score matrix function for a game

score_matrix <- function(post, game){
  n_sample <- length(post[[game]])
  home_p <- table(post[[game]])/n_sample # Posterior proportions of scores
  away_p <- table(post[[game + 10]])/n_sample
  
  # Matrix of results.
  m = round(home_p %o% away_p, 4)
  return (m)
}

# Win/Loss/Draw Percentages.

result_prob <- function(post, game) {
  m = score_matrix(post, game)
  hw = sum(m[lower.tri(m)])
  d = sum(diag(m))
  aw = sum(m[upper.tri(m)])
  
  return(c(hw = hw, d = d, aw = aw))
}


pred <- sapply(1:10, function(x) result_prob(m1post, x)) %>% 
  data.frame() %>% t() 
  

#--- Compile the hierarchical Model --- #
m2 <- cmdstan_model("code/multilevel_model.stan")
m2fit <- m2$sample(data = dat, chains = 8, parallel_chains = 8,
                   refresh = 500)
# Save the model for extraction:
#qsave(x = m2fit, file = "fits/multilevel_model.qs")
#m2fit <- qread("fits/multilevel_model.qs")

m2post <- m2fit$draws(variables = c("s1p", "s2p"), format = "df")
pred2 <- sapply(1:10, function(x) result_prob(m2post, x)) %>% 
  data.frame() %>% t()

# Overall prediction results
cbind(data %>% tail(n = 10), pred, pred2)

# Draw relative competencies of teams using both models.

# Model 1 attack and defence parameters posterior.
m1ad <- m1fit$draws(variables = c("att", "def"), format = "df") %>%
  data.frame()

# Model 2 attack and defence parameters posterior.
m2ad <- m2fit$draws(variables = c("att", "def", "att_bar", "def_bar"), format = "df") %>% 
  data.frame()



d_m1 <- tibble(
  att1 = apply(m1ad[1:20], 2, mean),
  att1_lc = apply(m1ad[1:20], 2, function(x) quantile(x, probs = 0.03)),
  att1_uc = apply(m1ad[1:20], 2, function(x) quantile(x, probs = 0.97)),
  def1 = apply(m1ad[21:40], 2, mean),
  def1_lc = apply(m1ad[21:40], 2, function(x) quantile(x, probs = 0.03)),
  def1_uc = apply(m1ad[21:40], 2, function(x) quantile(x, probs = 0.97)),
  teams = teams
)
  
d_m1 %>% ggplot(aes(x = att1, y = def1))+
  geom_point(shape = 1, size = 3)+
  geom_errorbar(aes(xmin = att1_lc, xmax = att1_uc), alpha = 0.1)+
  geom_errorbar(aes(ymin = def1_lc, ymax = def1_uc), alpha = 0.1)+
  geom_vline(xintercept = 0, linetype = 2, alpha = 0.5)+
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5)+
  geom_text(data = d_m1[c(1, 2,3, 10, 13, 16, 20),], aes(label = teams),
            hjust = -0.15, vjust = 0.15)+
  labs(x = "Attack Strength", y = "Defence Strength",
       title = "PL 2023/24 Teams Att and Def Strengths",
       subtitle = "Complete Pooling Model")+
    coord_cartesian(xlim = c(-0.8, 0.8), y = c(-0.8, 0.8))+
  theme_classic()


d_m2 <- tibble(
  att2 = apply(m2ad[1:20], 2, mean),
  att2_lc = apply(m2ad[1:20], 2, function(x) quantile(x, probs = 0.03)),
  att2_uc = apply(m2ad[1:20], 2, function(x) quantile(x, probs = 0.97)),
  def2 = apply(m2ad[21:40], 2, mean),
  def2_lc = apply(m2ad[21:40], 2, function(x) quantile(x, probs = 0.03)),
  def2_uc = apply(m2ad[21:40], 2, function(x) quantile(x, probs = 0.97)),
  teams = teams
)
  
d_m2 %>% ggplot(aes(x = att2, y = def2))+
  geom_point(shape = 1, size = 3)+
  geom_errorbar(aes(xmin = att2_lc, xmax = att2_uc), alpha = 0.2)+
  geom_errorbar(aes(ymin = def2_lc, ymax = def2_uc), alpha = 0.2)+
  geom_vline(xintercept = mean(m2ad$att_bar),
             linetype = 2, alpha = 0.5)+
  geom_hline(yintercept = mean(m2ad$def_bar), linetype = 2, alpha = 0.5)+
  geom_text(data = d_m2[c(1, 2,3, 10, 13, 16, 20),], aes(label = teams),
            hjust = -0.15, vjust = 0.15)+
  annotate("text", -0.75, -0.05, label = expression(mu[def]))+
  annotate("text", 0.15, 0.75, label = expression(mu[att]))+
  
  labs(x = "Attack Strength", y = "Defence Strength",
       title = "PL 2023/24 Teams Att and Def Strengths",
       subtitle = "Hierarchical Model")+
  coord_cartesian(xlim = c(-0.8, 0.8), y = c(-0.8, 0.8))+
  theme_classic()





# Comparison

tibble(
  att = c(d_m1$att1, d_m2$att2),
  def = c(d_m1$def1, d_m2$def2),
  Model = factor(rep(c("Normal","Multilevel"), each = 20)),
  team = factor(rep(seq(1, 20), 2))
) %>% ggplot(aes(x = att, y = def))+
  geom_point(aes(color = Model), size = 3)+
  scale_color_manual(
    values = c(
      "Normal" = "black",
      "Multilevel" = "red"
    )
  )+
  geom_line(aes(group = team), linewidth = 0.6)+
  geom_vline(xintercept = mean(m2ad$att_bar),
             linetype = 2, alpha = 0.5)+
  geom_hline(yintercept = mean(m2ad$def_bar), linetype = 2, alpha = 0.5)+
  annotate("text", -0.5, -0.05, label = expression(mu[def]))+
  annotate("text", 0.15, 0.75, label = expression(mu[att]))+
  labs(title = "Comparison between complete pooling and multilevel model")+
  theme_classic()


# Generate a smaller sample.
nog <- 50
small_dat <-  list(
  nt = nt,
  ng = nog,
  ht = ht[1:nog],
  at = at[1:nog],
  s1 = data$s1[1:nog],
  s2 = data$s2[1:nog],
  
  # for predictions
  np = 10,
  htp = ht[(nog+1):60],
  atp = at[(nog+1):60]
)

# Normal Model (Small sample)
m3fit <- m1$sample(data = small_dat, chains = 4, parallel_chains = 4, refresh = 500)
# Hierarchical Model (Small sample)
m4fit <- m2$sample(data = small_dat, chains = 4, parallel_chains = 4, refresh = 500)


# Covariance Model 
m_corr <- cmdstan_model("code/correlated_features.stan")
dat_corr <-  list(
  Nt = nt,
  N = ngd,
  Np = np,
  ht = ht[1:ngd],
  at = at[1:ngd],
  hg = rep(1, ngd), # home game intercept so that I can have one more parameter mu.
  s1 = data$s1[1:ngd],
  s2 = data$s2[1:ngd],
  htp = ht[(ngd+1) : ng],
  atp = at[(ngd+1) : ng]
  
)

m_corr_fit <- m_corr$sample(data = dat_corr, chains = 4, parallel_chains = 4,
refresh = 500 )

m_corr_post <- m_corr_fit$draws(variables = c("att", "def", "att_bar",
"def_bar", "Rho"), format = "df")

tibble(
  att = apply(m_corr_post[1:20], 2, mean),
  def = apply(m_corr_post[21:40], 2, mean)
) |> ggplot(aes(x = att, y = def))+
  geom_point(size = 3, shape = 1 )+
  coord_cartesian(xlim = c(-0.8, 0.8), y = c(-0.8, 0.8))+
  geom_vline(xintercept = mean(m_corr_post$att_bar))+
  geom_hline(yintercept = mean(m_corr_post$def_bar))+
  theme_bw()


tibble(
  prior_corr = rethinking::rlkjcorr(4000, K = 2, eta = 2)[, 1, 2],
  post_corr = m_corr_post$`Rho[1,2]`
) |> pivot_longer(everything()) |> 
  ggplot(aes(x = value))+
  geom_density(aes(color = name))+
  theme_classic()

# Comparison of all models:

loo_1 <- loo(m1fit$draws("log_lik"))
loo_2 <- loo(m2fit$draws("log_lik"))

loo_3 <- loo(m3fit$draws("log_lik"))
loo_4 <- loo(m4fit$draws("log_lik"))

loo_corr <- loo(m_corr_fit$draws("log_lik"))



# Model Comparison TODO : How far is too far? How different is different in 
# model comparison.

# Comparing big sample models.
print(loo_compare(loo_1, loo_2), simplify = F)
-3.2 + c(-1, 1) * 2.6 * 2.6

# Comparing small sample models

print(loo_compare(loo_3, loo_4), simplify = F)
-9.6 + c(-1, 1) * 3.1 * 2.6

# Comparing models to the correlated features model.
print(loo_compare(loo_1, loo_2, loo_corr), simplify = F)
-5.1 + c(-1, 1) * 2.9 * 2.6


# --- Bivariate Poisson Distribution. ---#

## Understanding Bivariate Poisson Distribution ##

a <- tibble(
  x = rbp(1000, lambda = c(1, 2, 2))
) |> mutate(
  s1 = x[,1],
  s2 = x[, 2]
) 

a |> pivot_longer(c(s1, s2)) |> 
  ggplot(aes(x = value))+
  geom_bar(aes(color = name, fill = name),position = "dodge")+
  theme_classic()

# Lambda 3 is covariance, meaning, in a football sense, if one team scores,
# the other one is more likely to score.

# Bivariate Poisson Model:

m_bivar <- cmdstan_model("code/bivariate_poisson.stan")
m_bivar_fit <- m_bivar$sample(data = dat_corr, chains = 4, parallel_chains = 8, 
refresh = 500 )

# Plot the attack and defense paramters from bivariate poisson model.

m_bivar_ad <- m_bivar_fit$draws(variables = 
  c("att", "def"), format = "df") %>%
  data.frame()

d_bivar <- tibble(
  att = apply(m_bivar_ad[1:20], 2, mean),
  att_lc = apply(m_bivar_ad[1:20], 2, function(x) quantile(x, probs = 0.03)),
  att_uc = apply(m_bivar_ad[1:20], 2, function(x) quantile(x, probs = 0.97)),
  def = apply(m_bivar_ad[21:40], 2, mean),
  def_lc = apply(m_bivar_ad[21:40], 2, function(x) quantile(x, probs = 0.03)),
  def_uc = apply(m_bivar_ad[21:40], 2, function(x) quantile(x, probs = 0.97)),
  teams = teams
)

plot_bivar <- d_bivar %>% ggplot(aes(x = att, y = def))+
  geom_point(shape = 1, size = 3)+
  geom_errorbar(aes(xmin = att_lc, xmax = att_uc), alpha = 0.1)+
  geom_errorbar(aes(ymin = def_lc, ymax = def_uc), alpha = 0.1)+
  #geom_vline(xintercept = mean(m_corr_ad$att_bar),
             #linetype = 2, alpha = 0.5)+
  #geom_hline(yintercept = mean(m_corr_ad$def_bar), linetype = 2, alpha = 0.5)+
  geom_text(data = d_bivar[c(1, 2,3, 10, 13, 16, 20),], aes(label = teams),
            hjust = -0.15, vjust = 0.15)+
  labs(x = "Attack Strength", y = "Defence Strength",
       title = "PL 2023/24 Teams Att and Def Strengths",
       subtitle = "Bivariate Poisson Model (94% credible intervals)")+
    coord_cartesian(xlim = c(-0.8, 0.8), y = c(-0.8, 0.8))+
  theme_classic()

plot_bivar



# Plot the win/loss/draw for all 4 models.
m_corr_post <- m_corr_fit$draws(variables = c("s1p","s2p"), format = "df")
pred_corr <- sapply(1:10, function(x) result_prob(m_corr_post, x)) %>% 
  data.frame() %>% t() 
colnames(pred_corr) <- c("hw_c", "d_c", "aw_c")


m_bivar_post <- m_bivar_fit$draws(variables = c("s1p", "s2p"), format = "df")
pred_bivar <- sapply(1:10, function(x) result_prob(m_bivar_post, x)) %>% 
  data.frame() %>% t()
colnames(pred_bivar) <- c("hw_b", "d_b", "aw_b")

cbind(data %>% tail(n = 10), pred_corr, pred_bivar) %>% as_tibble()
