m1post

tibble(
  x = rpois(1000, lambda = 2)
) %>% count(x) %>% mutate(p = n/sum(n))


home <-  tibble(
  x = m1post[3]
) %>% count(x) %>% mutate(p = n/sum(n)) %>% pull(p)

h <- table(m1post[[3]])/8000
a <- table(m1post[[13]])/8000

away <-  tibble(
  x = m1post[13]
) %>% count(x) %>% mutate(p = n/sum(n)) %>% pull(p)

m = round(home %o% away, 4)
sum(m[lower.tri(m)])
sum(m[upper.tri(m)])
sum(diag(m))

score_matrix <- function(post, game){
  n_sample <- length(post[[game]])
  home_p <- table(post[[game]])/n_sample # Posterior proportions of scores
  away_p <- table(post[[game + 10]])/n_sample
  
  # Matrix of results.
  m = round(home_p %o% away_p, 4)
  return (m)
}


result_prob <- function(post, game) {
  m = score_matrix(post, game)
  hw = sum(m[lower.tri(m)])
  d = sum(diag(m))
  aw = sum(m[upper.tri(m)])
  
  return(c(hw = hw, d = d, aw = aw))
}

data %>% tail(n = 10) %>% cbind(
sapply(1:10, function(x) result_prob(m1post, x)) %>% data.frame() %>% t() )
  



