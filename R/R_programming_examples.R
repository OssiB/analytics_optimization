
## Based on the book Garrett Grolemund: Hands-On Programming with R
## Example 1. Simulating a die

die <- 1:6

## Side note: operations elementwise, for example mulitplication and 
## inner and outer vector product 
# die * die
# die %*% die
# die %o% die

## Random element (roll the die once)

sample(die, size = 1)

## Sampling with replacement

sample(die, size = 2, replace = TRUE)

## Creating simple functions

roll <- function(){
    die <- 1:6
    dice <- sample(die, size = 2, replace = TRUE)
    sum(dice)
}

roll2 <- function(die = 1:6) {
    dice <- sample(die, size = 2, replace = TRUE)
    sum(dice)
}

library(ggplot2)

## Sidenote: help function
# ?plot

## Plotting repeated experiments

rolls <- replicate(10000, roll())

qplot(rolls, binwidth = 1)

## Weighted/biased die

roll3 <- function() {
    die <- 1:6
    dice <- sample(die, size = 2, replace = TRUE,
                   prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
    sum(dice)
}

## Histogram based on biased die
rolls <- replicate(10000, roll3())
qplot(rolls, binwidth = 1)

## Example 2. Playing blackjack

deck <- data.frame(
    face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
             "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",
             "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace",
             "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five",
             "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine",
             "eight", "seven", "six", "five", "four", "three", "two", "ace"),
    suit = c("spades", "spades", "spades", "spades", "spades", "spades",
             "spades", "spades", "spades", "spades", "spades", "spades", "spades",
             "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs",
             "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds",
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds",
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts",
             "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts",
             "hearts", "hearts", "hearts", "hearts", "hearts"),
    value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8,
              7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11,
              10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
)

## Loading/saving data from/to file 

# deck <- read.csv("deck.csv")
# write.csv(deck, file = "cards.csv", row.names = FALSE)

# Dealing a card

deal <- function(cards, position) {
    cards[position, ]
}

deal(deck, 1)

## Shuffle the deck

shuffle <- function(cards){
    random <- sample(1:52, size = 52)
    cards[random, ]
}

deal(deck, 1)

deck2 <- shuffle(deck)

deal(deck2, 1)

## Setting values 

deck2 <- deck

# Specifying aces with position

deck2$value[c(13, 26, 39, 52)] <- 14

# With logical subsetting

deck3 <- deck

deck3$value[deck3$face == "ace"] <- 14

# Setting the face cards

deck4 <- deck

facecard <- deck4$face %in% c("king", "queen", "jack")

deck4$value[facecard] <- 10

## Setting an unspecified value

deck4$value[deck4$face == "ace"] <- NA

# Dealing a blackjack hand

setup <- function(deck) {
    DECK <- deck
    DEAL <- function() {
        card <- deck[1, ]
        assign("deck", deck[-1, ], envir = parent.env(environment()))
        card
    }
    SHUFFLE <- function(){
        random <- sample(1:52, size = 52)
        assign("deck", DECK[random, ], envir = parent.env(environment()))
    }
    list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)

deal <- cards$deal

shuffle <- cards$shuffle

shuffle()

deal()

deal()

## Exercise 3. Slot machines

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

get_symbols <- function() {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(wheel, size = 3, replace = TRUE,
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function (symbols) {
    # identify case
    same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
    bars <- symbols %in% c("B", "BB", "BBB")
    # get prize
    if (same) {
        payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0)
        prize <- unname(payouts[symbols[1]])
    } else if (all(bars)) {
        prize <- 5
    } else {
        cherries <- sum(symbols == "C")
        prize <- c(0, 2, 5)[cherries + 1]
    }
    # adjust for diamonds
    diamonds <- sum(symbols == "DD")
    prize * 2 ^ diamonds
}

play <- function() {
    symbols <- get_symbols()
    print(symbols)
    score(symbols)
}

## Calculating the expected gain

# Creating a data frame with all the possible combinations

combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

# Assigning probabilities

combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

sum(combos$prob)

# Calculating the rpize for a combination with a for loop

for (i in 1:nrow(combos)) {
    symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
    combos$prize[i] <- score(symbols)
}

sum(combos$prize * combos$prob)

## When will we loose all the starting money? With a while loop

plays_till_broke <- function(start_with) {
    cash <- start_with
    n <- 0
    while (cash > 0) {
        cash <- cash - 1 + play()
        n <- n + 1
    }
    n
}

plays_till_broke(100)

## Vectorized operations

test_1 <- function(){
    output <- rep(NA, 1000000)
    for (i in 1:1000000){
        output[i] <- i + 1
    }
}
system.time(test_1())
## user  system elapsed 
## 1.26    0.00    1.28

test_2 <- function(){
    output <- NA
    for (i in 1:1000000){
        output[i] <- i + 1
    }
}

system.time(test_2())
## user system elapsed
## 1689.537 560.951 2249.927

## At home: look at apply function
## https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

