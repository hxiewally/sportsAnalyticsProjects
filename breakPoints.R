#Route break identification -- function aims to indentify breaks

iterate through time frame of play for each receiver 
break is dramatic angle change within window of frames
count break points
assign break type based on angle name
output list of breakpoints based on angle name

#360 / 8 = 45 degree increments

nameRoute <- function(player,
  total y distance traveled by player                   
    if y > 11
      look at mean angle of tan(y/x) at y > 12 yards
    if y < 11
      look at mean angle of tan(y/x) at y > 2 yards
