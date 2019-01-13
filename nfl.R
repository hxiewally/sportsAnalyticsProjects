# read in data
tracking_gameId_2017092100 <- read.csv("tracking_gameId_2017092100.csv")
plays <- read.csv("plays.csv")

# load plays data
# load tracking data for the game between SF 49ers and LA Rams
current.gameId <- 2017092100
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

ggplot(tracking.frame) + geom_point(aes(x=x, y=y, colour=team))

#expected yards after catch based on how open someone is

# functions for data processing

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
  if (sum(is.pass) == 0) {
    print('No forward pass found in this play:')
    print(this.playId)
  }
  pass.frame.id <- tracking.this.play.df[is.pass, ]$frame.id[1]  # get frame id
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

plays.df <- plays
game.tracking.df <- tracking_gameId_2017092100
test <- GetGamePassFrames(plays.df, game.tracking.df)

ggplot(test[[2]]) + geom_point(aes(x=x, y=y, colour=team), size=4)
