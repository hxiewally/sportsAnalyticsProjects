GetRouteFrames <- function(tracking.third.passing.df, this.playId) {
  # Get the route frames for one play during a game
  #
  # Args:
  # tracking.third.passing.df: data frame of all third passing plays during a game
  # this.playId: playId
  #
  # Returns:
  # data frame for the route frames or NULL if no forward pass is found
  tracking.this.play.df <- tracking.third.passing.df[tracking.third.passing.df$playId == this.playId, ]
  # find the frame when the ball is thrown
  is.pass <- tracking.this.play.df$event == 'pass_forward'
  is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
  is.snap <- tracking.this.play.df$event == 'ball_snap'
  is.snap[is.na(is.snap)] <- FALSE
  is.arrival <- tracking.this.play.df$event == 'pass_arrived'
  is.arrival[is.na(is.arrival)] <- FALSE 
  if (sum(is.pass) == 0 | sum(is.arrival) == 0) {
    print('No valid pass in this play:')
    print(this.playId)
    return(NULL)
  } else {
    snap.frame.id <- tracking.this.play.df[is.snap, ]$frame.id[1]
    arrival.frame.id <- tracking.this.play.df[is.arrival, ]$frame.id[1]
    tracking.frames.df <- tracking.this.play.df[tracking.this.play.df$frame.id >= snap.frame.id, ]
    tracking.frames.df <- tracking.frames.df[tracking.frames.df$frame.id <= arrival.frame.id, ]
    return(tracking.frames.df)
  }
}


GetReceiverRoutes <- function(route.frames, players.df) {
  route.frames.merged <- route.frames %>% inner_join(players.df)  # this gets rid of the ball
  qb <- route.frames.merged %>% filter(PositionAbbr == 'QB')
  route.frames.merged$offense <- route.frames.merged$team == qb$team[1]
  eligible.players <- route.frames.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
  return(eligible.players)
}


CreateRouteFeatures <- function(eligible.players) {
  route.features <- matrix(0, 5, 5)
  for (i in 1:5) {
    current.player <- eligible.players %>% filter(nflId == unique(eligible.players$nflId)[i])
    current.features <- c(length(current.player$s), mean(current.player$s), sd(current.player$s),
                          mean(current.player$dir), sd(current.player$dir))
    route.features[i, ] <- current.features
  }
  return(route.features)
}


ggplot(eligible.players) + geom_point(aes(x=x, y=y, colour=as.factor(nflId)))
