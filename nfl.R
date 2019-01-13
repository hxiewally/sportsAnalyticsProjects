# load plays data

current.gameId <- 2017092100
# load tracking data for the game between SF 49ers and LA Rams

# filter out plays in this game 
current.plays <- plays[plays$gameId == current.gameId, ]

# see if the plays in tracking data match plays data
current.plays$playId
unique(tracking_gameId_2017092100$playId)

# use plays data to third down passing plays in this game
is.third.passing <- (current.plays$down == 3) & (!is.na(current.plays$PassLength))
third.passing.plays <- current.plays$playId[is.third.passing]

# tracking data for third down passing plays
tracking.is.third.passing <- tracking_gameId_2017092100$playId %in% third.passing.plays
tracking.third.passing <- tracking_gameId_2017092100[tracking.is.third.passing, ]

# look at tracking data for one single play
tracking.single.play <- tracking.third.passing[tracking.third.passing$playId == 229, ]
# frames in this play
unique(tracking.single.play$frame.id)

# different events that can happen in a frame
levels(tracking.third.passing$event)

#tracking.frame <- tracking.single.play[tracking.single.play$frame.id == 1, ]
tracking.single.play$event

# find the frame when the ball is thrown
is.pass <- tracking.single.play$event == 'pass_forward'
is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
pass.frame.id <- tracking.single.play[is.pass, ]$frame.id[1]  # get frame id

tracking.frame <- tracking.single.play[tracking.single.play$frame.id == pass.frame.id, ]

library(ggplot2)

ggplot(tracking.frame) + geom_point(aes(x=x, y=y, colour=team), size=4)


GetPlayPassFrame <- function(tracking.third.passing.df, this.playId) {
  # Get the passing frame for one play during a game
  #
  # Args:
  # tracking.third.passing.df: data frame of all third passing plays during a game
  # this.playId: playId
  #
  # Returns:
  # data frame for the passing time frame
  
  tracking.this.play.df <- tracking.third.passing.df[tracking.third.passing.df$playId == this.playId, ]
  # find the frame when the ball is thrown
  is.pass <- tracking.this.play.df$event == 'pass_forward'
  is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
  #print(sum(is.pass))
  if (sum(is.pass) == 0) {
    print('No forward pass found in this play:')
    print(this.playId)
  }
  pass.frame.id <- tracking.this.play.df[is.pass, ]$frame.id[1] - 1  # get frame id
  print(this.playId)
  print(pass.frame.id)
  tracking.frame.df <- tracking.this.play.df[tracking.this.play.df$frame.id == pass.frame.id, ]
  return(tracking.frame.df)
}

GetGamePassFrames <- function(plays.df, game.tracking.df) {
  # Get the passing frames for all third passing plays during a game
  #
  # Args:
  # plays.df: data frame of play level data from plays.csv
  # game.tracking.df: data frame of tracking data for a game
  #
  # Returns:
  # list of data frames for the passing time frames
  
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

plays.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/plays.csv")
game.tracking.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_2017090700.csv")
test <- GetGamePassFrames(plays.df, game.tracking.df)

j <- 1
df <- test[[j]]
play.info <- plays.df[(plays.df$gameId == df$gameId[1]) & (plays.df$playId == df$playId[1]), ]
print(play.info)

ggplot(df) + geom_point(aes(x=x, y=y, colour=team), size=4)


#test.frame <- play.df[play.df$frame.id == 31, ]
#ggplot(test.frame) + geom_point(aes(x=x, y=y, colour=team), size=4)

library(dplyr)

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

all.gameIds <- unique(plays.df$gameId)

paste("Hello", "world", sep=" ")


plays.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/plays.csv")

for (gameId in unique(plays.df$gameId)) {
  print('Currently processing game:')
  print(gameId)
  game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", gameId, ".csv", sep=""))
  test <- GetGamePassFrames(plays.df, game.tracking.df)
  for (i in 1:length(test)) {
    df <- test[[i]]
    new.df <- CalculateDistances(df)
  }
}

gameId <- 2017092100
game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", gameId, ".csv", sep=""))
test <- GetGamePassFrames(plays.df, game.tracking.df)

play.list <- list()
dist.data <- matrix(nrow=10, ncol=10)
for (i in 1:10) {
  df <- test[[i]]
  play.list[[i]] <- plays.df[(plays.df$gameId == df$gameId[1]) & (plays.df$playId == df$playId[1]), ]
  new.df <- CalculateDistances(df)
  dist.data[i, ] <- c(new.df$dDefMean[!is.na(new.df$dDefMean)], new.df$dDefMin[!is.na(new.df$dDefMin)])
}

play.data.df <- do.call(rbind, play.list)

play.data.df$Mean.Dist <- NA
play.data.df$Max.Dist <- NA
play.data.df$Min.Dist <- NA

for (i in 1:10) {
  play.data.df$Mean.Dist[i] <- mean(dist.data[i, 6:10])  # mean of closest defender
  play.data.df$Max.Dist[i] <- max(dist.data[i, 6:10])  # max of closest defender
  play.data.df$Min.Dist[i] <- min(dist.data[i, 6:10])  # min of closest defender
}

play.data.df$Success <- (play.data.df$PlayResult - play.data.df$yardsToGo) > 0






##################
df <- test[[3]]
play.info <- plays.df[(plays.df$gameId == df$gameId[1]) & (plays.df$playId == df$playId[1]), ]
play.data[i, ] <- play.info
new.df <- CalculateDistances(df)
print(c(new.df$dDefMean[!is.na(new.df$dDefMean)], new.df$dDefMin[!is.na(new.df$dDefMin)]))

tracking.frame <- new.df
#tracking.frame <- tracking.frame[-23, ]
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