library(tidyverse)
library(BayesFactor)

e1 <- read.csv('data/exp1.csv')

e1_summary <- group_by(e1, target, setsize, dyn, sub) %>% summarize(rt=mean(rt))
e1_summary$setsize <- factor(e1_summary$setsize)
e1_summary$sub <- factor(e1_summary$sub)

# Run a Bayesian ANOVA with rt as DV, with dyn, target and setsize as IV's and with sub as a random factor
search_BFs <- e1_summary %>% anovaBF(rt ~ target + setsize + dyn + sub, whichRandom = "sub", data = .) 

# Find the model with the highest Bayes factor
top_BF <- head(search_BFs, 1)

# Calculate the Bayes factor between this "best" model, and the full model
top_BF / search_BFs[18]

# Plot all Bayes factors
plot(search_BFs)

# The top 11 Bayes factors seem noticable larger than the rest: what do these models all have in common?
head(search_BFs, 11) # Seems like they all include target + dyn
tail(search_BFs, 7) # And the remaining models lack target, dyn or both

# Which models have a BayesFactor of more than 1/3 compared to the "best" model?
head(search_BFs/top_BF) # Apparently none, although the model without the target:dyn interaction is close!
# Note: because the Bayes factors are approximate, your answer to the above question could possibly be different!
