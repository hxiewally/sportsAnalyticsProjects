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
