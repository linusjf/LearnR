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
tendulkar_id <- find_player_id("Sachin Tendulkar")$ID
# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
tendulkar <- getPlayerData(tendulkar_id,dir=".",file="tendulkar.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
batsmanRunsFreqPerf("./tendulkar.csv","Sachin Tendulkar")
batsmanMeanStrikeRate("./tendulkar.csv","Sachin Tendulkar")
batsmanRunsRanges("./tendulkar.csv","Sachin Tendulkar")
batsman4s("./tendulkar.csv","Tendulkar")
batsman6s("./tendulkar.csv","Tendulkar")
batsmanDismissals("./tendulkar.csv","Tendulkar")
battingPerf3d("./tendulkar.csv","Sachin Tendulkar")
batsmanAvgRunsGround("./tendulkar.csv","Sachin Tendulkar")
batsmanAvgRunsOpposition("./tendulkar.csv","Tendulkar")
batsmanRunsLikelihood("./tendulkar.csv","Sachin Tendulkar")
