#Requires nfl.R and trajectories.R

#Using 2017101501 as an example game

third.passing.df <- plays.df %>% filter(gameId == 2017101501, down == 3, PassResult == 'C')

current.play.df <- game.tracking.df %>% filter(playId == 2595)

#Obtain route frames

is.pass <- current.play.df$event == 'pass_forward'
is.pass[is.na(is.pass)] <- FALSE  # get rid of NAs
is.snap <- current.play.df$event == 'ball_snap'
is.snap[is.na(is.snap)] <- FALSE
is.arrival <- current.play.df$event == 'pass_arrived'
is.arrival[is.na(is.arrival)] <- FALSE 

snap.frame.id <- current.play.df[is.snap, ]$frame.id[1]
arrival.frame.id <- current.play.df[is.arrival, ]$frame.id[1]
route.frames.df <- current.play.df[current.play.df$frame.id >= snap.frame.id, ]
route.frames.df <- route.frames.df[route.frames.df$frame.id <= arrival.frame.id, ]

routes.2505 <- GetReceiverRoutes(route.frames.df, players.df)

ggplot(routes.2505) + geom_point(aes(x=x, y=y, colour=as.factor(nflId)))

# gameRoutes <- rep(NA, length(unique(third.passing.df$playId)))
# k <- 1
#
# for (i in unique(third.passing.df$playId)) {
#   gameRoutes[k] <- GetRouteFrames(third.passing.df, i)
#   k <- k + 1
# }

#Route break identification -- function aims to indentify breaks

#direct distance traveled, total distance traveled, vertical distance traveled, downfield/short, first break angle, name of first break, how many breaks

#iterate through time frame of play for each receiver 
#break is dramatic angle change within window of frames
#count break points

#360 / 8 = 45 degree increments

#Use player 2540200 as example - assemble information vector for player

test.player <- routes.2505 %>% filter(nflId == 2540200)

test.player.information.vector <- rep(NA, 7) 

last.element.count <- length(test.player$x)

player.direct.distance.traveled <- sqrt((test.player$x[last.element.count] - test.player$x[1])^2 + (test.player$y[last.element.count] - test.player$y[1])^2)

test.player.information.vector[1] <- player.direct.distance.traveled

player.total.distance.per.frame <- rep(NA, last.element.count - 1)

for (i in 1:length(player.total.distance.per.frame)) {
  player.total.distance.per.frame[i] <- sqrt((test.player$x[i + 1] - test.player$x[i])^2 + (test.player$y[i + 1] - test.player$y[i])^2) 
}
player.total.distance.traveled <- sum(player.total.distance.per.frame)

test.player.information.vector[2] <- player.total.distance.traveled

player.vertical.distance.traveled <- abs(test.player$x[last.element.count] - test.player$x[1])

test.player.information.vector[3] <- player.vertical.distance.traveled

if (player.vertical.distance.traveled <= 15) {
  test.player.information.vector[4] <- "short"
} else {
  test.player.information.vector[4] <- "downfield"    
}



getRouteBreaks <- function(eligible.players) {
    route.description <- matrix
}
