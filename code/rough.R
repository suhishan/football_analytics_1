# Generative model.

tibble()



# Most Likely Score code.
tibble(
  s1_mostlikely = apply(m2post[,1:10], 2, function(x) which.max(tabulate(x))), # finding the mode.
  s2_mostlikely = apply(m2post[,11:20], 2, function(x) which.max(tabulate(x))) # finding the mode.
) %>% bind_cols(data %>% tail(n=10))




