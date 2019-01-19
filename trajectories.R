######################### Calculate receiver trajectories #####################################

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
  # find the frame when the ball is snapped
  is.snap <- tracking.this.play.df$event == 'ball_snap'
  is.snap[is.na(is.snap)] <- FALSE
  # find the frame when the ball is thrown
  is.pass <- tracking.this.play.df$event == 'pass_forward'
  is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
  #print(sum(is.pass))
  if (sum(is.pass) == 0) {
    print('No forward pass found in this play:')
    print(this.playId)
    return(NULL)
  } else {
    snap.frame.id <- tracking.this.play.df[is.snap, ]$frame.id[1]
    pass.frame.id <- tracking.this.play.df[is.pass, ]$frame.id[1] - 1  # get frame id
    print(this.playId)
    tracking.frames.df <- tracking.this.play.df[tracking.this.play.df$frame.id >= snap.frame.id, ]
    tracking.frames.df <- tracking.frames.df[tracking.frames.df$frame.id < pass.frame.id, ]
    return(tracking.frames.df)
  }
}


third.passing.plays.df <- plays.df %>% filter(gameId == game.tracking.df$gameId[1], down == 3, !is.na(PassLength))

current.play.df <- game.tracking.df %>% filter(playId == 680)

is.snap <- current.play.df$event == 'ball_snap'
is.snap[is.na(is.snap)] <- FALSE
is.pass <- current.play.df$event == 'pass_forward'
is.pass[is.na(is.pass)] <- FALSE 
snap.frame.id <- current.play.df[is.snap, ]$frame.id[1]
pass.frame.id <- current.play.df[is.pass, ]$frame.id[1] - 1  # get frame id
last.frame.id <- max(current.play.df$frame.id)
if (pass.frame.id + 10 < last.frame.id) {
  pass.frame.id <- pass.frame.id + 50  # get 1s after forward pass
} else {
  pass.frame.id <- last.frame.id
}

tracking.frames.df <- current.play.df[current.play.df$frame.id >= snap.frame.id, ]
tracking.frames.df <- tracking.frames.df[tracking.frames.df$frame.id < pass.frame.id, ]


CalculateRoutes <- function(route.frames, players.df) {
  # Calculate route trajectories for all eligible receivers
  #
  # Args:
  # route.frames: data frame for the passing time frame
  # players.df: data frame of player information
  #
  # Returns:
  # matrix of dim(t, 5)
  
  route.frames.merged <- route.frames %>% inner_join(players.df)  # this gets rid of the ball
  qb <- route.frames.merged %>% filter(PositionAbbr == 'QB')
  route.frames.merged$offense <- route.frames.merged$team == qb$team[1]
  eligible.players <- route.frames.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
  
  min.dist <- rep(0, 5)
  
  for (i in 1:dim(eligible.players)[1]) {
    dist.vec <- rep(0, 11)
    x1 <- eligible.players$x[i]
    y1 <- eligible.players$y[i]
    start.frame.id <- 
    end.frame.id <- 
    for (j in 1:11) {
      
    }
    min.dist[i] <- min(dist.vec)
  }
  return(min.dist)
}

route.frames <- tracking.frames.df

route.frames.merged <- route.frames %>% inner_join(players.df)  # this gets rid of the ball
qb <- route.frames.merged %>% filter(PositionAbbr == 'QB')
route.frames.merged$offense <- route.frames.merged$team == qb$team[1]
eligible.players <- route.frames.merged %>% filter(offense == TRUE & PositionAbbr != 'QB' & (jerseyNumber < 50 | jerseyNumber > 79))
start.frame.id <- min(eligible.players$frame.id)
end.frame.id <- max(eligible.players$frame.id)
time.length <- end.frame.id - start.frame.id + 1
trajectories <- matrix(0, time.length, 10)
receiver.ids <- unique(eligible.players$nflId)
for (t in start.frame.id:end.frame.id) {
  for (i in 1:length(receiver.ids)) {
    current.player <- eligible.players %>% filter(frame.id == t, nflId == receiver.ids[i])
    trajectories[t - start.frame.id + 1, i * 2 - 1] <- current.player$x 
    trajectories[t - start.frame.id + 1, i * 2] <- current.player$y
  }
}

origin <- trajectories[1, ]
for(j in 1:nrow(trajectories)) {
  trajectories[j, ] <- trajectories[j, ] - origin
}

traj.long <- rbind(trajectories[, 1:2], trajectories[, 3:4], trajectories[, 5:6], 
                   trajectories[, 7:8], trajectories[, 9:10])
traj.df <- as.data.frame(traj.long)
colnames(traj.df) <- c('x', 'y')

traj.df$player <- rep(1:5, each=time.length)

ggplot(traj.df) + geom_point(aes(x=x, y=y, colour=as.factor(player)))
