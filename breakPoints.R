#Requires nfl.R and trajectories.R

#####################
##                 ##
##    FUNCTIONS    ##
##                 ##
#####################

##DETERMINE FIRST ROUTE BREAK POINT ANGLES##

calcAngle <- function(v1,v2) {
  #Where v1 and v2 are two vectors pointing to the same origin
  #v1 temporally comes before origin, v2 after
  angle <- 180/pi * (atan2(v2[2],v2[1]) - atan2(v1[2],v1[1]))
  return(angle)
}

calcAngle <- function(v1,v2) {
  #Where v1 and v2 are two vectors pointing to the same origin
  #v1 temporally comes before origin, v2 after
  angle <- 180/pi * (atan2(v2[2],v2[1]) - atan2(v1[2],v1[1]))
  if (angle < 0) {
    angle <- angle + 360
  }
  return(angle)
}

calcAngleFromVector <- function(point.start, point.end, point.origin) {
  #Calculate angle based on 2-d points taken from route frames
  point.start <- as.matrix(point.start)
  point.end <- as.matrix(point.end)
  point.origin <- as.matrix(point.origin)
  v1 <- point.origin - point.start
  v2 <- point.origin - point.end
  angle <- calcAngle(v1,v2)
  return(angle)
}

calcRouteDistance <- function(playerRouteTrajectory) {
  #Outputs list of route features related to route distance
  playerRouteTrajectory.information.vector <- rep(NA,3)
  last.element.count <- nrow(playerRouteTrajectory)

  player.direct.distance.traveled <- sqrt((playerRouteTrajectory$x[last.element.count] - playerRouteTrajectory$x[1])^2 + (playerRouteTrajectory$y[last.element.count] - playerRouteTrajectory$y[1])^2)
  playerRouteTrajectory.information.vector[1] <- player.direct.distance.traveled

  player.total.distance.per.frame <- rep(NA, last.element.count - 1)
  for (i in 1:length(player.total.distance.per.frame)) {
    player.total.distance.per.frame[i] <- sqrt((playerRouteTrajectory$x[i + 1] - playerRouteTrajectory$x[i])^2 + (playerRouteTrajectory$y[i + 1] - playerRouteTrajectory$y[i])^2) 
}
  player.total.distance.traveled <- sum(player.total.distance.per.frame)
  playerRouteTrajectory.information.vector[2] <- player.total.distance.traveled

  player.vertical.distance.traveled <- abs(playerRouteTrajectory$x[last.element.count] - playerRouteTrajectory$x[1])
  playerRouteTrajectory.information.vector[3] <- player.vertical.distance.traveled
  if (player.vertical.distance.traveled <= 15) {
    playerRouteTrajectory.information.vector[4] <- "short"
  }   else {
    playerRouteTrajectory.information.vector[4] <- "downfield"    
  }
  return(playerRouteTrajectory.information.vector)
}

##COUNT BREAK POINTS##
#Iterate through points in receiver route path as origins. 
#Call angle function.
#Determine downfield distance traveled before first break point.
#If no break points, fly route.

# createAngleList <- function(player.route.frames, windowSize) {
#   #Args:
#   #player.route.frames: individual player route trajectory
#   #windowSize: number of frames to origin, must be integer
#   if (windowSize %% 1 != 0) {
#     print("Window size is not an integer. Please input an integer.")
#   }
#   angleList <- rep(NA, nrow(player.route.frames) - 2 * windowSize) 
#   for (i in 1:length(angleList)) {
#     point.start <- c(player.route.frames$x[i], player.route.frames$y[i])
#     point.origin <- c(player.route.frames$x[i + windowSize], player.route.frames$y[i + windowSize])      
#     point.end <- c(player.route.frames$x[i + 2 * windowSize], player.route.frames$y[i + 2 * windowSize])
#     angle <- calcAngleFromVector(point.start, point.origin, point.end)
#     angleList[i] <- angle
#   }
#   return(angleList)  
# }

#Problem with above code is that it does not calculate initial angle

createAngleList <- function(player.route.frames, windowSize = 5) {
  #Args:
  #player.route.frames: individual player route trajectory
  #windowSize: number of frames from origin to endpoint, must be integer
  #Suggest 5 for window size
  if (windowSize %% 1 != 0) {
    print("Window size is not an integer. Please input an integer.")
  }
  angleList <- rep(NA, nrow(player.route.frames) - windowSize) 
  for (i in 1:length(angleList)) {
    #point.start must be initiated farther back to calculate initial angle. 
    point.start <- c(player.route.frames$x[i], 0)
    point.origin <- c(player.route.frames$x[i], player.route.frames$y[i])      
    point.end <- c(player.route.frames$x[i + windowSize], player.route.frames$y[i + windowSize])
    angle <- calcAngleFromVector(point.start, point.origin, point.end)
    angleList[i] <- angle
  }
  return(angleList)  
}

##IDENTIFY FIRST BREAK POINT##

findBreakType <- function(angle, routeDistance) {
  if (routeDistance == "short") {
    if (findInterval(angle, c(90,270)) == 1) {
      return("dig")
    } else {
      return("slant")
    }
  } else {
      if (findInterval(angle, c(90 - 22.5,90 + 22.5)) == 1) {
        return("fly")
      } else if (findInterval(angle, c(135 - 22.5, 135 + 22.5)) == 1) {
        return("corner")
      } else if (findInterval(angle, c(180 - 22.5, 180 + 22.5)) == 1) {
        return("out")
      } else if (findInterval(angle, c(225 - 22.5, 225 + 45)) == 1) {
        return("comeback")
      } else if (findInterval(angle, c(315 - 45, 315 + 22.5)) == 1) {
        return("curl")
      } else if (findInterval(angle, c(360 - 22.5, 360)) == 1) {
        return("dig")
      } else if (findInterval(angle, c(0, 0 + 22.5)) == 1) {
        return("dig")
      } else if (findInterval(angle, c(45 - 22.5, 45 + 22.5)) == 1) {
        return("post")
  }
}

findBreak <- function(angleList, breakWindowSize) {
  #Args:
  #angleList: list of angles of player route from createAngleList
  #breakWindowSize: size of window to count presence of breaks in angle list. Not to be confused with windowSize for calculating angles. 
  breakList <- rep(NA, floor(length(angleList) / breakWindowSize))
  for (i in 1:length(breakList)) {
    angleVar <- var(angleList[(i - 1) * breakWindowSize + 1:i * breakWindowSize + 1])
    if (angleVar > 60) {
      breakType <- 
    } 
  }
}

##OBTAIN ROUTE DESCRIPTION VECTOR FOR ONE PLAYER PER PLAY##

routeDescript2 <- function(player.route.frames) {
  var(angleList)
}

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

#direct distance traveled, total distance traveled, vertical distance traveled, downfield/short, angle variance, first break point coordinates, name of first break, how many moves 

#iterate through time frame of play for each receiver 
#break is dramatic angle change within window of frames
#count break points

#360 / 8 = 45 degree increments

#Use player 2540200 as example - assemble information vector for player

test.player <- routes.2505 %>% filter(nflId == 2540200)
test.player2 <- routes.2505 %>% filter(nflId == 2559176)
test.player3 <- routes.2505 %>% filter(nflId == 2552428)

test.player.information.vector <- rep(NA, 7) 

last.element.count <- nrow(test.player)

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
