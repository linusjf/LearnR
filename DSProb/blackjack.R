#!/usr/bin/env Rscript
library("gtools")

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
kings <- paste("King", suits)
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[, 1] %in% aces & hands[, 2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[, 1] %in% aces & hands[, 2] %in% facecard) | (hands[, 2] %in% aces & hands[, 1] %in% facecard))

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
