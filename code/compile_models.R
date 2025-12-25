packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior")
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

#--- Working with posterior draws ---#

m1post = m1fit$draws(variables = c("s1p", "s2p"), format = "df")

# Score matrix function for a game

score_matrix <- function(post, game){
  n_sample <- length(post[[game]])
  home_p <- tabulate(post[[game]])/n_sample # Posterior proportions of scores
  away_p <- tabulate(post[[game + 10]])/n_sample
  
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





  





  