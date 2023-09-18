#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : sachin
# @created     : Sunday Sep 17, 2023 10:45:05 IST
#
# @description :
######################################################################
library(cricketr)
library(cricketdata)
bradman_id <- find_player_id("Donald Bradman")$ID
# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "bradman.csv", package = "cricketr")
bradman <- getPlayerData(bradman_id,dir=".",file="bradman.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
batsmanRunsFreqPerf("./bradman.csv","Donald Bradman")
batsmanMeanStrikeRate("./bradman.csv","Donald Bradman")
batsmanRunsRanges("./bradman.csv","Donald Bradman")
batsman4s("./bradman.csv","Bradman")
batsman6s("./bradman.csv","Bradman")
batsmanDismissals("./bradman.csv","Bradman")
battingPerf3d("./bradman.csv","Donald Bradman")
batsmanAvgRunsGround("./bradman.csv","Donald Bradman")
batsmanAvgRunsOpposition("./bradman.csv","Bradman")
batsmanRunsLikelihood("./bradman.csv","Donald Bradman")
