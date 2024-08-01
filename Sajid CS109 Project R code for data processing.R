# Sajid Farook
# CS109 Probability Challenge
# June 4, 2023
# This is where I processed the data. The tool was built in Python

rm(list = ls())

getwd()
setwd("/Users/sajidfarook/Desktop")
d <- read.csv("fpl_training.csv")
head(d)
names(d)

# cleaning data
df <- subset(d, select = c(round, assists, total_points, goals_conceded, goals_scored, opponent_team, minutes, player_name, was_home, player_team, 
                           xG, shots, xA, npg, npxG, xGA_player_team, xG_player_team, xG_opponent_team))
df <- df[(df$total_points  %% 1 == 0) & (df$total_points > 0) & (df$minutes > 45) & (df$total_points < 35),]


## ADDING AN XGC FOR OPPONENTS TEAM ###
teamdiff <- read.csv("EPL Standings 2000-2022.csv")
teamdiff <- teamdiff[teamdiff$Season == "2016-17" | teamdiff$Season == "2017-18" | teamdiff$Season == "2018-19" | teamdiff$Season == "2019-20", ]
unique_teams <- unique(teamdiff$Team)
team_avg_ga <- data.frame(
  Team = character(),    # First column for unique teams
  Average_GA = double()  # Second column for average GA values
)
for (team in unique_teams) {
  avg_ga <- mean(teamdiff$GA[teamdiff$Team == team])
  
  # Add team and average GA to the dataframe
  team_avg_ga <- rbind(team_avg_ga, data.frame(Team = team, Average_GA = avg_ga))
}
team_avg_ga$Average_GA <- team_avg_ga$Average_GA / 38 # Final vector mapping team names to GC


# Modifying team names in original df to align
df[df == "Tottenham"] <- "Tottenham Hotspur"
df[df == "Stoke"] <- "Stoke City"
df[df == "Hull"] <- "Hull City"
df[df == "Swansea"] <- "Swansea City"
df[df == "Leicester"] <- "Leicester City"
df[df == "West Ham"] <- "West Ham United"
df[df == "Cardiff"] <- "Cardiff City"
df[df == "Brighton"] <- "Brighton & Hove Albion"
df[df == "Huddersfield"] <- "Huddersfield Town"
df[df == "Norwich"] <- "Norwich City"


# Create an empty column for Opp_GC_90
df$Opp_GC_90 <- NA

# Iterate through each row of df
for (i in 1:nrow(df)) {
  team <- df$opponent_team[i]
    row_index <- match(team, team_avg_ga$Team)
  
  if (!is.na(row_index)) {
    avg_ga <- team_avg_ga$Average_GA[row_index]
        df$Opp_GC_90[i] <- avg_ga
  }
}

## NOW, OPPONENT DIFFICULTY IS ADDED TO THE DATASET :D

## Creating a form indicator
df$form <- NA
for (i in 1:nrow(df)) {
  player <- df$player_name[i]
  gw <- df$round[i]
  pts_last5 <- 0
  counter <- 0
  for (n in 1:5) {
    if (gw - n > 0) {
      if (i - n >= 1) {  # Add condition to skip iteration when i - n < 1
        pts <- df$total_points[i - n]
        pts_last5 <- pts_last5 + pts
        counter <- counter + 1
      }
    }
  }
  if (counter > 0) {
    avg <- pts_last5 / counter
    df$form[i] <- avg
  }
  else {
    df$form[i] <- NA
  }
}
# WE NOW HAVE A FORM INDICATOR :)

# Repeating the same thing, but for all season (Q input)
df$prev_tp_avg <- NA
for (i in 1:nrow(df)) {
  player <- df$player_name[i]
  gw <- df$round[i]
  tp <- 0
  counter <- 0
  for (n in 1:38) {
    if (gw - n > 0) {
      if (i - n >= 1) {  # Add condition to skip iteration when i - n < 1
        pts <- df$total_points[i - n]
        tp <- tp + pts
        counter <- counter + 1
      }
    }
  }
  if (counter > 0) {
    avg <- tp / counter
    df$prev_tp_avg[i] <- avg
  }
  else {
    df$prev_tp_avg[i] <- NA
  }
}
# We now have a total points per game so far metric :)

# New column adding XG and XA for XGI (X input)
df$xGi <- df$xG + df$xA

df$xGipg_last7 <- NA
for (i in 1:nrow(df)) {
  player <- df$player_name[i]
  gw <- df$round[i]
  xGipast7 <- 0
  counter <- 0
  for (n in 1:7) {
    if (gw - n > 0) {
      if (i - n >= 1) {  # Add condition to skip iteration when i - n < 1
        xGi_curr <- df$xGi[i - n]
        xGipast7 <- xGipast7 + xGi_curr
        counter <- counter + 1
      }
    }
  }
  if (counter > 0) {
    avg <- xGipast7 / counter
    df$xGipg_last7[i] <- avg
  }
  else {
    df$xGipg_last7[i] <- NA
  }
}
#NOW WE HAVE XGI INDICATOR :)

#Cleaning and rounding off input variable data
data <- subset(df, select = c(player_name, total_points, Opp_GC_90, was_home, form, prev_tp_avg, xGipg_last7))
data <- data[complete.cases(data), ]
data$Opp_GC_90 <- round(data$Opp_GC_90, digits = 1)
data$form <- round(data$form * 2) / 2 # rounding so there are less discrete values
data$prev_tp_avg <- round(data$prev_tp_avg * 2) / 2 # rounding so there are less discrete values
data$xGipg_last7 <- round(data$xGipg_last7 / 0.05) * 0.05

# Randomly reserving 20% for testing
set.seed(123)
sample_index <- sample(1:nrow(data), floor(0.2 * nrow(data)))
test_data <- data[sample_index, ]
data <- data[-sample_index, ] # reserving 20% of data as testing data



########################## ALL VARIABLE ARE IN :)###############################

#P(O|Y)
calc_OgY <- function(O, Y) {
  # Subset the dataset based on Y
  subset_Y <- data[data$total_points == Y, ]
  
  # Count the number of entries where varname is equal to A and total_points is equal to Y
  subset_O_Y <- subset_Y[subset_Y$Opp_GC_90 == O,]
  
  # Calculate the conditional probability P(A|Y)
  probability_O_given_Y <- nrow(subset_O_Y) / nrow(subset_Y)
  
  # Return the result
  if (is.finite(probability_O_given_Y)) {
    return(probability_O_given_Y)
  }
  return(0)
}

#P(G|Y)
calc_FgY <- function(F_val, Y) {
  # Subset the dataset based on Y
  subset_Y <- data[data$total_points == Y, ]
  
  # Count the number of entries where varname is equal to A and total_points is equal to Y
  subset_F_Y <- subset_Y[subset_Y$form == F_val,]
  
  # Calculate the conditional probability P(A|Y)
  probability_F_given_Y <- nrow(subset_F_Y) / nrow(subset_Y)
  
  # Return the result
  if (is.finite(probability_F_given_Y)) {
    return(probability_F_given_Y)
  }
  return(0)
}

#P(Q|Y)
calc_QgY <- function(Q, Y) {
  # Subset the dataset based on Y
  subset_Y <- data[data$total_points == Y, ]
  
  # Count the number of entries where varname is equal to A and total_points is equal to Y
  subset_Q_Y <- subset_Y[subset_Y$prev_tp_avg == Q,]
  
  # Calculate the conditional probability P(A|Y)
  probability_Q_given_Y <- nrow(subset_Q_Y) / nrow(subset_Y)
  
  # Return the result
  if (is.finite(probability_Q_given_Y)) {
    return(probability_Q_given_Y)
  }
  return(0)
}

#P(X|Y)
calc_XgY <- function(X, Y) {
  # Subset the dataset based on Y
  subset_Y <- data[data$total_points == Y, ]
  
  # Count the number of entries where varname is equal to A and total_points is equal to Y
  subset_X_Y <- subset_Y[subset_Y$xGipg_last7 == X,]
  
  # Calculate the conditional probability P(A|Y)
  probability_X_given_Y <- nrow(subset_X_Y) / nrow(subset_Y)
  
  # Return the result
  if (is.finite(probability_X_given_Y)) {
    return(probability_X_given_Y)
  }
  return(0)
}

#P(O)
calc_O <- function(O) {
  total_count <- nrow(data)
  O_count <- sum(data$Opp_GC_90 == O)
  probability <- O_count / total_count
  return(probability)
}

#P(F)
calc_F <- function(F_val) {
  total_count <- nrow(data)
  F_count <- sum(data$form == F_val)
  probability <- F_count / total_count
  return(probability)
}

#P(Q)
calc_Q <- function(Q) {
  total_count <- nrow(data)
  Q_count <- sum(data$prev_tp_avg == Q)
  probability <- Q_count / total_count
  return(probability)
}

#P(X)
calc_X <- function(X) {
  total_count <- nrow(data)
  X_count <- sum(data$xGipg_last7 == X)
  probability <- X_count / total_count
  return(probability)
}

#P(P)
calc_Y <- function(Y) {
  total_count <- nrow(data)
  Y_count <- sum(data$total_points == Y)
  probability <- Y_count / total_count
  return(probability)
}

# ALL INTERMEDIARY FUNCTIONS ARE IN - NOW TO BUILD THE FINAL PROBABILITY #


####################### NAIVE EXPECTATION APPROACH ###################

#P(Y|O,F,Q,X)
prob_YgOFQX <- function(Y, O, F_val, Q, X) {
  O_round <- round(O, digits = 1)
  if (O_round == 1.2) {
    avg = (prob_YgOFQX(Y, 1.3, F_val, Q, X) + prob_YgOFQX(Y, 1.1, F_val, Q, X)) /2
    return(avg)
  }
  if (O_round == 1.9) {
    avg = (prob_YgOFQX(Y, 1.8, F_val, Q, X) + prob_YgOFQX(Y, 2.0, F_val, Q, X))/2
    return(avg)
  }
  if (O_round >= 0.5 & O_round < 0.8) {
    O_round = 0.8
  }  
  if (O_round > 2.1 & O_round <= 2.8) {
    O_round = 2.1
  } 
  
  F_round <- round(F_val * 2) / 2
  Q_round <- round(Q * 2) / 2
  X_round <- round(X / 0.05) * 0.05
  
  OgY <- calc_OgY(O_round, Y)
  FgY <- calc_FgY(F_round, Y)
  QgY <- calc_QgY(Q_round, Y)
  XgY <- calc_XgY(X_round, Y)
  
  pO <- calc_O(O_round)
  pF <- calc_F(F_round)
  pQ <- calc_Q(Q_round)
  pX <- calc_X(X_round)
  pY <- calc_Y(Y)
  
  numerator = OgY * FgY * QgY * XgY * pY
  denominator = pO * pF * pQ * pX 
  
  prob = numerator / denominator
  return(prob)
}

# E(Y|O,F,Q,X); iterating through P(Y|O,F,Q,X) and multiplying by y for all possible y values
E_YgOFQX <- function(O, F_val, Q, X) {
  count = 0
  for (y in 1:30) {
    prob = prob_YgOFQX(y, O, F_val, Q, X)
    count = count + (y * prob)
  }
  if (is.finite(count)) {
    return(count)
  }
  return("Parameters are not possible given the training data")
}


# Testing
test_data_save = test_data # saving the unmodified testing set in case something goes wrong

#populating test dataframe with naive expectation predictions
test_data$prediction_naiveE <- NA
for (i in 1:nrow(test_data)) {
  O <- test_data[i, "Opp_GC_90"]
  F_val <- test_data[i, "form"]
  Q <- test_data[i, "prev_tp_avg"]
  X <- test_data[i, "xGipg_last7"]
  prediction <- E_YgOFQX(O, F_val, Q, X)
  test_data[i, "prediction_naiveE"] = prediction
}

#removing anomalous data that did not work
test_data <- test_data[test_data$prediction_naiveE != "Parameters are not possible given the training data", , drop = FALSE]

#rounding off the prediction
test_data$prediction_naiveE <- as.numeric(test_data$prediction_naiveE)
test_data$prediction_naiveE <- round(test_data$prediction_naiveE, 2)

#Calculating MSE for naive exp.
squared_errors <- (test_data$prediction_naiveE - test_data$total_points)^2
mean_se_naiveE <- mean(squared_errors)

##We are done with approach 1. Mean squared error of this approach is stored under 'mean_se_naiveE' ad is around 16.17
##We can see all the predictions in test_data
##A prediction can be made through the E_YgOFQX(O, F_val, Q, X) function


####################### LATENT VARIABLE APPROACH ###################
# All of this below was replicated in Python so it can be integegrated with the final tool
data_lat <- data

# Converting each variable into latent variable score
convertOppToLatScore <- function(O) {
  breaks = c(0.5, 0.8, 1.1, 1.3, 1.5, 1.7, 2.1, 2.8)
  labels = c(1,2,3,4,5,6,7)
  compVal = cut(O, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.numeric(compVal))
}
convertFormToLatScore <- function(F_val) {
  breaks = c(1.0, 3.0, 4.0, 6.0, 8.0, 10.0, 17.0)
  labels = c(1,2,3,4,5,6)
  compVal = cut(F_val, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.numeric(compVal))
}
convertTPToLatScore <- function(Q) {
  breaks = c(1.0, 3.0, 4.0, 6.0, 8.0, 10.0, 17.0)
  labels = c(1,2,3,4,5,6)
  compVal = cut(Q, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.numeric(compVal))
}
convertXToLatScore <- function(X) {
  breaks = c(0, 0.15, 0.35, 0.5, 0.7, 1, 1.75)
  labels = c(1,2,3,4,5,6)
  compVal = cut(X, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.numeric(compVal))
}
convertXandFToLatScore <- function(X, F_val) {
  sum <- X + F_val
  breaks = c(1, 3, 5, 7, 10, 13, 18.75)
  labels = c(1,2,3,4,5,6)
  compVal = cut(sum, breaks = breaks, labels = labels, include.lowest = TRUE)
  return(as.numeric(compVal))
}

# Converting original training data into latent variable inputs
data_lat$Opp_GC_90<-convertOppToLatScore(data$Opp_GC_90)
data_lat$form<-convertFormToLatScore(data$form)
data_lat$prev_tp_avg<-convertTPToLatScore(data$prev_tp_avg)
data_lat$xGipg_last7<-convertXToLatScore(data$xGipg_last7)
data_lat$comp_form<- convertXandFToLatScore(data_lat$form, data_lat$xGipg_last7)
data_lat <- subset(data_lat, select = -c(xGipg_last7, form, was_home))
data_lat # Now we have a modified dataset using latent variables

# P(Y|O,Q,R) using definition of conditional probability
calc_prob_YgOQR <- function(Y, O, Q, F_val, X) {
  O <- convertOppToLatScore(O)
  Q <- convertTPToLatScore(Q)
  R_temp1 <- convertFormToLatScore(F_val)
  R_temp2 <- convertXToLatScore(X)
  R <- convertXandFToLatScore(R_temp1, R_temp2)
  
  subset_Y <- data_lat[data_lat$total_points == Y, ]
  subset_YOQR <- subset_Y[subset_Y$Opp_GC_90 == O & subset_Y$prev_tp_avg == Q & subset_Y$comp_form == R, ]
  subset_OQR <- data_lat[data_lat$Opp_GC_90 == O & data_lat$prev_tp_avg == Q & data_lat$comp_form == R, ]
  numerator <- nrow(subset_YOQR)
  denomenator <- nrow(subset_OQR)
  return(numerator/denomenator)
}

#E(Y|O,Q,R) by iterating through P(Y|O,Q,R) for plausible Y values
E_YgOQR <- function(O, F_val, Q, X) {
  count = 0
  for (y in 1:30) {
    prob = calc_prob_YgOQR(y, O, F_val, Q, X)
    count = count + (y * prob)
  }
  if (is.finite(count)) {
    return(count)
  }
  return("Parameters are not possible given the training data - try modifying inputs to be less extreme")
}


# NOW WE HAVE A FUNCTION TO PREDICT POINTS USING LATENT VARIABLE APPROACH :)

# Testing #
test_data_save = test_data
test_data$prediction_lat = NA
for (i in 1:nrow(test_data)) {
  O <- test_data[i, "Opp_GC_90"]
  F_val <- test_data[i, "form"]
  Q <- test_data[i, "prev_tp_avg"]
  X <- test_data[i, "xGipg_last7"]
  
  prediction <- E_YgOQR(O, F_val, Q, X)
  test_data[i, "prediction_lat"] = prediction
}

#seeing how many were rejected (9 were - noted in the write-up)
length(test_data[test_data$prediction_lat == "Parameters are not possible given the training data - try modifying inputs to be less extreme",])

#rounding off the prediction
test_data$prediction_lat <- ifelse(is.na(as.numeric(test_data$prediction_lat)), test_data$prediction_lat, round(as.numeric(test_data$prediction_lat), 2))

# Calculating MSE
squared_errors <- (test_data$prediction_lat[!is.na(as.numeric(test_data$prediction_lat))] - test_data$total_points[!is.na(as.numeric(test_data$prediction_lat))])^2
mean_se_lat <- mean(squared_errors)

## Latent variable approach is complete. We have the expected value function stored in E_YgOQR(). 
## Predictions are stored in test_data final column
## MSE is 9.00

# This data shows all the final predictions :)
test_data_display <- data.frame(Player_Name = test_data$player_name, O = test_data$Opp_GC_90, Q = test_data$prev_tp_avg, F = test_data$form, X = test_data$xGipg_last7, Actual_Points_Outcome = test_data$total_points, Prediction_NaiveExp = test_data$prediction_naiveE, Prediction_LatVars = test_data$prediction_lat)

