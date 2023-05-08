#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(gtools))
suppressPackageStartupMessages(library(tidyverse))

# source: https://github.com/nrwade0/edX/blob/master/3-Probability/discrete_probability_questions.R

## Useful Information
# Permutations: # of combinations when order matters
#  formula: P_(k,n) = n!/(n-k)!
#   when n = group size, k = subset size
# Combinations: # of combinations when order does not matter
#  formula: (n k) = P_(k,n)/k! = n!/k!(n-k)!
#   when n = group size, k = subset size

print("####### QUESTION 1 #######")
print("In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).")
print("####### QUESTION 1a #######")
print("How many different ways can the 3 medals be distributed across 8 runners?")
nrow(permutations(8, 3))

print("####### QUESTION 1b #######")
print("How many different ways can the three medals be distributed among the 3 runners from Jamaica?")
nrow(permutations(3, 3))

print("####### QUESTION 1c #######")
print("What is the probability that all three medals are won by Jamaica?")
# 56 total combinations, only one with 1, 2, 3
1 / nrow(combinations(8, 3))

print("####### QUESTION 1d #######")
# Run Monte Carlo 10k
print("Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:")
print("runners <- c('Jamaica', 'Jamaica', 'Jamaica', 'USA', 'Ecuador', 'Netherlands', 'France', 'South Africa')")
print("For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop. Calculate the probability that all the runners are from Jamaica.")

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})
mean(results)

print("####### QUESTION 2 #######")
print("A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices. A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.")
print("####### QUESTION 2a #######")
print("How many meal combinations are possible with the current menu?")

print("Different meals = ⁶C₁")
print("Different sides = ⁶C₂")
print("Different drinks = ²C₁")
6 * nrow(combinations(6, 2)) * 2

print("####### QUESTION 2b #######")
print("The manager has one additional drink he could add to the special. How many combinations are possible if he expands his original special to 3 drink options?")
print("Different meals = ⁶C₁")
print("Different sides = ⁶C₂")
print("Different drinks = ³C₁")

6 * nrow(combinations(6, 2)) * 3

print("####### QUESTION 2c #######")
print("How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?")

print("Different meals = ⁶C₁")
print("Different sides = ⁶C₃")
print("Different drinks = ³C₁")
6 * 3 * nrow(combinations(6, 3))

print("####### QUESTION 2d #######")
print("The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.")
print("Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.")

f <- function(entree) {
  3 * 15 * entree
}

print("Use sapply to apply the function to entree option counts ranging from 1 to 12.")
print("What is the minimum number of entree options required in order to generate more than 365 combinations?")
options <- seq(1:12)
sel <- sapply(options, f)
which(sel >= 365)[1]

print("####### QUESTION 2e #######")
print("Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.")

ff <- function(sides) {
  3 * 6 * nrow(combinations(sides, 2))
}

print("Use sapply to apply the function to side counts ranging from 2 to 12.")
print("What is the minimum number of side options required in order to generate more than 365 combinations?")
options <- 2:12
sel <- sapply(options, ff)
which(sel >= 365)[1] + 1

print("####### QUESTION 3a/b/c #######")
head(esoph)
print("How many groups are in the study?")
nrow(esoph)

print("How many cases are there?")
all.cases <- sum(esoph$ncases)
print(all.cases)
print("How many controls are there?")
all.controls <- sum(esoph$ncontrols)
print(all.controls)

print("List the age groups:")
esoph %>% distinct(agegp)
print("List the alcohol groups:")
esoph %>% distinct(alcgp)
print("List the tobacco groups:")
esoph %>% distinct(tobgp)
print("List all the groups:")
esoph %>% distinct(agegp, alcgp, tobgp)

print("####### QUESTION 4a/b/c/d #######")
print("What is the probability that a subject in the highest alcohol consumption group is a cancer case?")
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(sum_cases = sum(ncases), tot = sum(ncontrols) + sum(ncases), probability = sum_cases / tot)

print("What is the probability that a subject in the lowest alcohol consumption group is a cancer case?")
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases = sum(ncases), tot = sum(ncontrols) + sum(ncases), probability = sum_cases / tot)

print("Given that a person is a case, what is the probability that they smoke 10g or more a day?")
tot_cases <- esoph %>% summarize(tot_cases = sum(ncases))
smoking10_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
smoking10_cases[["smoking10_cases"]] / tot_cases[["tot_cases"]]

print("Given that a person is a control, what is the probability that they smoke 10g or more a day?")
tot_cases <- esoph %>% summarize(tot_cases = sum(ncontrols))
smoking10_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
smoking10_cases[["smoking10_cases"]] / tot_cases[["tot_cases"]]

print("####### QUESTION 5a/b/c/d #######")
print("For cases, what is the probability of being in the highest alcohol group?")
sum_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(sum_cases = sum(ncases))
sum_cases[["sum_cases"]] / all.cases

print("For cases, what is the probability of being in the highest tobacco group?")
sum_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  summarize(sum_cases = sum(ncases))
sum_cases[["sum_cases"]] / all.cases

print("For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?")
sum_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  summarize(sum_cases = sum(ncases))
sum_cases[["sum_cases"]] / all.cases

print("For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?")
sum_cases <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(sum_cases = sum(ncases))
sum_cases[["sum_cases"]] / all.cases

print("####### QUESTION 6a/b/c/d/e/f #######")
print("For controls, what is the probability of being in the highest alcohol group?")
contr_sum <- esoph %>%
  filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols))
contr_sum[["contr_sum"]] / all.controls

print("How many times more likely are cases than controls to be in the highest alcohol group?")
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(
    co_prob = sum(ncontrols) / all.controls, ca_prob = sum(ncases) / all.cases,
    ratio = ca_prob / co_prob
  )

print("For controls, what is the probability of being in the highest tobacco group?")
esoph %>%
  filter(tobgp == "30+") %>%
  summarize(probability = sum(ncontrols) / all.controls)

print("For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?")
esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(probability = sum(ncontrols) / all.controls)

print("For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?")
esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(probability = sum(ncontrols) / all.controls)

print("How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?")
esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(
    co_prob = sum(ncontrols) / all.controls, ca_prob = sum(ncases) / all.cases,
    ratio = ca_prob / co_prob
  )
