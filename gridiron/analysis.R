library(tidyverse)

plays.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/plays.csv")
games.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/games.csv")
players.df <- read.csv("~/Downloads/Big-Data-Bowl-master/Data/players.csv")

all.features <- NULL
play.list <- list()
k <- 0
for (gameId in unique(plays.df$gameId)) {
  print('Currently processing game:')
  print(gameId)
  game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", gameId, ".csv", sep=""))
  current.gameId <- game.tracking.df$gameId[1]
  # filter out plays in this game 
  current.plays <- plays.df[plays.df$gameId == current.gameId, ]
  # use plays data to third down passing plays in this game
  is.third.passing <- (current.plays$down == 3) & (!is.na(current.plays$PassLength))
  third.passing.plays <- current.plays$playId[is.third.passing]
  # tracking data for third down passing plays
  tracking.is.third.passing <- game.tracking.df$playId %in% third.passing.plays
  tracking.third.passing.df <- game.tracking.df[tracking.is.third.passing, ]
  for (i in 1:length(third.passing.plays)) {
    this.playId <- third.passing.plays[i]
    tracking.frames.df <- GetRouteFrames(tracking.third.passing.df, this.playId)
    if (!is.null(tracking.frames.df)) {
      tracking.players.df <- GetReceiverRoutes(tracking.frames.df, players.df)
      result <- try(CreateRouteFeatures(tracking.players.df))
      if (class(result) == "try-error") {
        print('Something is wrong!')
        next
      }
      k <- k + 1
      all.features <- rbind(all.features, result)
      play.list[[k]] <- tracking.players.df[1, ] %>% inner_join(plays.df)
    }
  }
}


play.data.combo <- do.call(rbind, play.list)

play.data.combo <- cbind.data.frame(play.data.combo, all.features)

play.data.combo <- play.data.combo %>% inner_join(play.data.df, by=c("gameId", 'playId'))

play.data.combo$success <- play.data.combo$PlayResult >= play.data.combo$yardsToGo

play.data.combo$yardsToGo.levels <- cut(play.data.combo$yardsToGo, c(0, 4, 7, 10, 30))

fit <- glm(success~fly+out+comeback+curl+dig+slant+corner+post+yardsToGo.levels, 
           family='binomial',
           data=play.data.combo)
summary(fit)

df <- data.frame(route=c("corner", "post", "curl"), 
                 oddsChange=c(exp(0.199605)-1, exp(0.157602)-1, exp(-0.669634)-1))
ggplot(df, aes(route, oddsChange, fill=route)) +
  geom_col() + 
  ggtitle('How Routes Impact Odds') +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text())
ggsave('/Users/linggeli/Desktop/odds_routes.png', width=6, height=4, dpi=500)

fit <- lm(distance~fly+out+comeback+curl+dig+slant+corner+post+yardsToGo.levels, 
           data=play.data.combo)
summary(fit)

colSums(all.features)

play.subset <- play.data.combo %>% filter(success == TRUE, yardsToGo.x == 4, distance > 4)


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


game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", 2017091011, ".csv", sep=""))
current.play.df <- game.tracking.df %>% filter(playId == 1373)
current.play.df <- current.play.df %>% filter(frame.id == 33)

this.play <- plays.df %>% filter(gameId == 2017091011, playId == 1373)

ggplot(current.play.df) + geom_point(aes(x=x, y=y, color=team), size=4) +
  ggtitle('Game 2017091011 Play 1373 \n B.Hoyer pass short right to M.Goodwin for 6 yards') +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text())
ggsave('/Users/linggeli/Desktop/good_separation.png', width=6, height=4, dpi=500)


game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", 2017101501, ".csv", sep=""))
current.play.df <- game.tracking.df %>% filter(playId == 3829)
current.play.df <- current.play.df %>% filter(frame.id == 34)

this.play <- plays.df %>% filter(gameId == 2017101501, playId == 3829)

ggplot(current.play.df) + geom_point(aes(x=x, y=y, color=team), size=4) +
  ggtitle('Game 2017101501 Play 3829 \n J.Flacco pass incomplete short right to C.Matthews') +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text())
ggsave('/Users/linggeli/Desktop/bad_separation.png', width=6, height=4, dpi=500)


game.tracking.df <- read.csv(paste("~/Downloads/Big-Data-Bowl-master/Data/tracking_gameId_", 2017100107, ".csv", sep=""))
frames <- GetRouteFrames(game.tracking.df, 214)
receivers <- GetReceiverRoutes(frames, players.df)

this.play <- plays.df %>% filter(gameId == 2017100107, playId == 214)

ggplot(receivers) + geom_point(aes(x=x, y=y, color=as.factor(nflId))) +
  ggtitle('Game 2017100107 Play 214 \n T.Brady pass short right to C.Hogan for 8 yards') +
  theme(plot.title=element_text(hjust=0.5), axis.title=element_text())
ggsave('/Users/linggeli/Desktop/bad_curl.png', width=6, height=4, dpi=500)


mean(play.data.df$distance[(play.data.df$success == TRUE) & (play.data.df$yardsToGo.levels) == "(4,7]"])
