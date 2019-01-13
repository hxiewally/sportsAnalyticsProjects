# location of ball
x.ball = filter(tracking.frame, team == "ball")$x
y.ball = filter(tracking.frame, team == "ball")$y

tracking.frame$eligible <- NA

# filter distance away from ball calculation

tracking.frame$dBall <- NA
for (i in 1:length(tracking.frame$jerseyNumber)) {
  if (tracking.frame$eligible[i] != T) {
    tracking.frame$dBall[i] = sqrt((tracking.frame$x[i] - x.ball)^2 + (tracking.frame$y[i] - y.ball)^2)
  }
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

for (i in 1:length(tracking.frame$jerseyNumber)) {
  if ((tracking.frame$jerseyNumber[i] < 50) & (tracking.frame$offense[i] == T) & (tracking.frame$dBall[i] > 0.9) | (tracking.frame$jerseyNumber[i] > 79) & (tracking.frame$offense[i] == T) & (tracking.frame$dBall[i] > 0.9)) {
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
    dDefList <- rep(NA,11)                      
    for (j in 1:length(tracking.frame$jerseyNumber)) {
      if (tracking.frame$offense[j] == F) {
        dDefList[j] = sqrt((tracking.frame$x[j] - tracking.frame$x[i])^2 + (tracking.frame$y[j] - tracking.frame$y[i])^2)
      }
    tracking.frame$dDefMean[i] = mean(dDefList)
    tracking.frame$dDefMin[i] = min(dDefList)
    }
  }
}

#dDefMean will suggest relatively the type of defensive coverage. Mean theoretically should be lower for man coverage, higher for zone coverage. However, hard to differentiate from this between bad man coverage and zone.
