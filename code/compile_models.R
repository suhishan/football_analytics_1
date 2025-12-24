packages <- c("tidyverse","cmdstanr","bayesplot","loo","posterior", "flextable")
lapply(packages, library, character.only = TRUE)


#--- Load the data and make it usable ---#

data = read.csv("data/premfull23_24.csv")

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

pred <- data %>% tail(n = 10) %>% cbind(
sapply(1:10, function(x) result_prob(m1post, x)) %>% data.frame() %>% t() )
  

