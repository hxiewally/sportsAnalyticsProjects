library(tidyverse)

plays.df <- read.csv("plays.csv")
games.df <- read.csv("games.csv")
players.df <- read.csv("players.csv")


######################### Functions for getting passing frames #########################

GetPlayPassFrame <- function(tracking.third.passing.df, this.playId) {
  # Get the passing frame for one play during a game
  #
  # Args:
  # tracking.third.passing.df: data frame of all third passing plays during a game
  # this.playId: playId
  #
  # Returns:
  # data frame for the passing time frame or NULL if no forward pass is found
  
  tracking.this.play.df <- tracking.third.passing.df[tracking.third.passing.df$playId == this.playId, ]
  # find the frame when the ball is thrown
  is.pass <- tracking.this.play.df$event == 'pass_forward'
  is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
  #print(sum(is.pass))
  if (sum(is.pass) == 0) {
    print('No forward pass found in this play:')
    print(this.playId)
    return(NULL)
  } else {
    pass.frame.id <- tracking.this.play.df[is.pass, ]$frame.id[1] - 1  # get frame id
    print(this.playId)
    tracking.frame.df <- tracking.this.play.df[tracking.this.play.df$frame.id == pass.frame.id, ]
    #tracking.frame.df <- tracking.frame.df %>% left_join(roster %>% select(nflId, PositionAbbr), by = 'nflId')
    return(tracking.frame.df)
  }
}

GetGamePassFrames <- function(plays.df, game.tracking.df) {
  # Get the passing frames for all third passing plays during a game
  #
  # Args:
  # plays.df: data frame of play level data from plays.csv
  # game.tracking.df: data frame of tracking data for a game
  #
  # Returns:
  # list of data frames for the passing time frames (including occasional NULLs)
  
  current.gameId <- game.tracking.df$gameId[1]
  # filter out plays in this game 
  current.plays <- plays.df[plays.df$gameId == current.gameId, ]
  # use plays data to third down passing plays in this game
  is.third.passing <- (current.plays$down == 3) & (!is.na(current.plays$PassLength))
  third.passing.plays <- current.plays$playId[is.third.passing]
  # tracking data for third down passing plays
  tracking.is.third.passing <- game.tracking.df$playId %in% third.passing.plays
  tracking.third.passing.df <- game.tracking.df[tracking.is.third.passing, ]
  
  df.list <- list()
  for (i in 1:length(third.passing.plays)) {
    this.playId <- third.passing.plays[i]
    df.list[[i]] <- GetPlayPassFrame(tracking.third.passing.df, this.playId)
  }
  return(df.list)
}


######################### Calculate distances from passing frame (deprecated) ########################

CalculateDistances <- function(tracking.frame) {
  # location of ball
  x.ball = filter(tracking.frame, team == "ball")$x
  y.ball = filter(tracking.frame, team == "ball")$y
  
  # filter distance away from ball calculation
  
  tracking.frame$dBall <- NA
  for (i in 1:length(tracking.frame$jerseyNumber)) {
    tracking.frame$dBall[i] = sqrt((tracking.frame$x[i] - x.ball)^2 + (tracking.frame$y[i] - y.ball)^2)
  }
  
  # identify offense - find quarterback in frame, quarterback team is offense
  
  tracking.frame$offense <- NA
  tracking.frame.qb <- filter(tracking.frame, team != "ball" & dBall == min(tracking.frame$dBall[tracking.frame$dBall > 0]))
  qbTeam <- tracking.frame.qb$team  
  
  for (i in 1:length(tracking.frame$team)) {
    if (tracking.frame$team[i] == qbTeam) {
      tracking.frame$offense[i] = T
    }
    else if (tracking.frame$team[i] == "ball") {
      tracking.frame$offense[i] = "ball"
    }
    else {
      tracking.frame$offense[i] = F
    }
  }
  
  # filter for eligible receivers
  
  tracking.frame$eligible <- NA
  
  for (i in 1:length(tracking.frame$jerseyNumber)) {
    if ((tracking.frame$jerseyNumber[i] < 50) & (tracking.frame$offense[i] == T) & (tracking.frame$dBall[i] > min(tracking.frame$dBall[tracking.frame$dBall > 0])) | (tracking.frame$jerseyNumber[i] > 79) & (tracking.frame$offense[i] == T) & (tracking.frame$dBall[i] > 0.9)) {
      tracking.frame$eligible[i] <- T
    }
    else {
      tracking.frame$eligible[i] <- F
    }
  }
  
  # if eligible receiver, calculate distance of closest defender
  
  tracking.frame$dDefMean <- NA
  tracking.frame$dDefMin <- NA
  
  for (i in 1:length(tracking.frame$jerseyNumber)) {
    if (tracking.frame$eligible[i] == T) {
      dDefList <- rep(NA, 11)  
      k <- 0
      for (j in 1:length(tracking.frame$jerseyNumber)) {
        if (tracking.frame$offense[j] == F) {
          k <- k + 1
          dDefList[k] = sqrt((tracking.frame$x[j] - tracking.frame$x[i])^2 + (tracking.frame$y[j] - tracking.frame$y[i])^2)
        }
      }
      tracking.frame$dDefMean[i] = mean(dDefList)
      tracking.frame$dDefMin[i] = min(dDefList)
    }
  }
  
  return(tracking.frame)
}


new.df <- CalculateDistances(df)
new.df$dDefMean[!is.na(new.df$dDefMean)]
new.df$dDefMin[!is.na(new.df$dDefMin)]


######################### Calculate distances from passing frame (new) ########################

CalculateDistances <- function(passing.frame, players.df) {
  # Calculate the minimum distances from the closest defender for all eligible receivers
  #
  # Args:
  # pass.frame: data frame for the passing time frame
  # players.df: data frame of player information
  #
  # Returns:
  # vector of minimum distances (length 5)
  
  #football <- passing.frame %>% filter(team == 'ball')
  min.dist <- rep(0, 5)
  passing.frame.merged <- passing.frame %>% inner_join(players.df)  # this gets rid of the ball
  qb <- passing.frame.merged %>% filter(PositionAbbr == 'QB')
  passing.frame.merged$offense <- passing.frame.merged$team == qb$team
  eligible.players <- passing.frame.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
  #if (dim(eligible.players)[1] != 5) {
  #  print(eligible.players)
  #}
  defensive.players <- passing.frame.merged %>% filter(offense == FALSE)
  for (i in 1:dim(eligible.players)[1]) {
    dist.vec <- rep(0, 11)
    x1 <- eligible.players$x[i]
    y1 <- eligible.players$y[i]
    for (j in 1:11) {
      x2 <- defensive.players$x[j]
      y2 <- defensive.players$y[j]
      dist.vec[j] <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    }
    min.dist[i] <- min(dist.vec)
  }
  return(min.dist)
}

CalculateDistMean <- function(passing.frame, players.df) {
  # Calculate the minimum distances from the closest defender for all eligible receivers
  #
  # Args:
  # pass.frame: data frame for the passing time frame
  # players.df: data frame of player information
  #
  # Returns:
  # vector of minimum distances (length 5)
  
  #football <- passing.frame %>% filter(team == 'ball')
  mean.dist <- rep(0, 5)
  passing.frame.merged <- passing.frame %>% inner_join(players.df)  # this gets rid of the ball
  qb <- passing.frame.merged %>% filter(PositionAbbr == 'QB')
  passing.frame.merged$offense <- passing.frame.merged$team == qb$team
  eligible.players <- passing.frame.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
  #if (dim(eligible.players)[1] != 5) {
  #  print(eligible.players)
  #}
  defensive.players <- passing.frame.merged %>% filter(offense == FALSE)
  for (i in 1:dim(eligible.players)[1]) {
    dist.vec <- rep(0, 11)
    x1 <- eligible.players$x[i]
    y1 <- eligible.players$y[i]
    for (j in 1:11) {
      x2 <- defensive.players$x[j]
      y2 <- defensive.players$y[j]
      dist.vec[j] <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    }
    mean.dist[i] <- mean(dist.vec)
  }
  return(mean.dist)
}

CalculateDistMax <- function(passing.frame, players.df) {
  # Calculate the minimum distances from the closest defender for all eligible receivers
  #
  # Args:
  # pass.frame: data frame for the passing time frame
  # players.df: data frame of player information
  #
  # Returns:
  # vector of minimum distances (length 5)
  
  #football <- passing.frame %>% filter(team == 'ball')
  max.dist <- rep(0, 5)
  passing.frame.merged <- passing.frame %>% inner_join(players.df)  # this gets rid of the ball
  qb <- passing.frame.merged %>% filter(PositionAbbr == 'QB')
  passing.frame.merged$offense <- passing.frame.merged$team == qb$team
  eligible.players <- passing.frame.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
  #if (dim(eligible.players)[1] != 5) {
  #  print(eligible.players)
  #}
  defensive.players <- passing.frame.merged %>% filter(offense == FALSE)
  for (i in 1:dim(eligible.players)[1]) {
    dist.vec <- rep(0, 11)
    x1 <- eligible.players$x[i]
    y1 <- eligible.players$y[i]
    for (j in 1:11) {
      x2 <- defensive.players$x[j]
      y2 <- defensive.players$y[j]
      dist.vec[j] <- sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
    }
    max.dist[i] <- max(dist.vec)
  }
  return(max.dist)
}



######################################## Test code ########################################

all.dist <- NULL
all.dist.mean <- NULL
all.dist.max <- NULL
play.list <- list()
k <- 0

for (gameId in unique(plays.df$gameId)) {
  print('Currently processing game:')
  print(gameId)
  game.tracking.df <- read.csv(paste("tracking_gameId_", gameId, ".csv", sep=""))
  test <- GetGamePassFrames(plays.df, game.tracking.df)
  for (i in 1:length(test)) {
    df <- test[[i]]
    if (!is.null(df)) {
      k <- k + 1
      current.dist <- CalculateDistances(df, players.df)  # minimum distance vector
      current.dist.mean <- CalculateDistMean(df, players.df)  # mean distance vector
      current.dist.max <- CalculateDistMax(df, players.df)  #max distance vector      
      all.dist <- rbind(all.dist, current.dist)
      all.dist.mean <- rbind(all.dist.mean, current.dist.mean)
      all.dist.max <- rbind(all.dist.max, current.dist.max)      
      play.list[[k]] <- df[1, ] %>% inner_join(plays.df)  # first row with merged play data
    }
  }
}

play.data.df <- do.call(rbind, play.list)

play.data.df$success <- play.data.df$PlayResult >= play.data.df$yardsToGo
play.data.df$distanceMinMin <- apply(all.dist, 1, min) #Min of mins
play.data.df$distanceMeanMin <- apply(all.dist, 1, mean) #Mean of mins
play.data.df$distanceMeanMean <- apply(all.dist.mean, 1, mean) #mean of means
play.data.df$distanceMeanMax <- apply(all.dist.max, 1, mean) #mean of maxes
play.data.df$distanceMaxMax <- apply(all.dist.max, 1, max) #max of maxes
play.data.df$distanceMaxMin <- apply(all.dist, 1, max) #max of mins

library(ggthemes)

# ggplot(play.data.df) + geom_violin(aes(y=distanceMeanMin, x=success, fill=success)) +
#   ggtitle('1734 Third Down Passing Plays') + 
#   theme_fivethirtyeight() + theme(plot.title=element_text(hjust=0.5)) + ylab('Mean of Min Yards To Eligible Receivers Per Play')

# ggplot(play.data.df) + geom_violin(aes(y=distanceMeanMean, x=success, fill=success)) +
#   ggtitle('1734 Third Down Passing Plays') + 
#   theme_fivethirtyeight() + theme(plot.title=element_text(hjust=0.5)) + ylab('Mean of Mean Yards To Eligible Receivers Per Play')

ggplot(play.data.df) + geom_violin(aes(y=distanceMeanMax, x=success, fill=success)) +
  ggtitle('1734 Third Down Passing Plays') + 
  theme_fivethirtyeight() + theme(plot.title=element_text(hjust=0.5))

ggplot(play.data.df) + geom_violin(aes(y=distanceMaxMax, x=success, fill=success)) +
  ggtitle('1734 Third Down Passing Plays') + 
  theme_fivethirtyeight() + theme(plot.title=element_text(hjust=0.5))

ggplot(play.data.df) + geom_violin(aes(y=distanceMaxMin, x=success, fill=success)) +
  ggtitle('1734 Third Down Passing Plays') + 
  theme_fivethirtyeight() + theme(plot.title=element_text(hjust=0.5))
