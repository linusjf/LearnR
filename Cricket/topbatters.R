#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : topbatters
# @created     : Sunday Sep 17, 2023 12:29:37 IST
#
# @description :
######################################################################
library(cricketr)
library(cricketdata)
# Retrieve the file path of a data file installed with cricketr
pathToFile <- system.file("data", "tendulkar.csv", package = "cricketr")
tend_id = find_player_id("Sachin Tendulkar")$ID
print(tend_id)
tendulkar <- getPlayerData(tend_id,dir=".",file="tendulkar.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
kallis_id = find_player_id("Jacques Kallis")$ID
print(kallis_id)
kallis <- getPlayerData(kallis_id,dir=".",file="kallis.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
ponting_id = find_player_id("Ricky Ponting")$ID
print(ponting_id)
ponting <- getPlayerData(ponting_id,dir=".",file="ponting.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
sangakkara_id = find_player_id("Kumar Sangakkara")$ID
print(sangakkara_id)
sangakarra <- getPlayerData(sangakkara_id,dir=".",file="sangakkara.csv",type="batting",homeOrAway=c(1,2), result=c(1,2,4))
batsmanPerfBoxHist("./tendulkar.csv","Sachin Tendulkar")
batsmanPerfBoxHist("./kallis.csv","Jacques Kallis")
batsmanPerfBoxHist("./ponting.csv","Ricky Ponting")
batsmanPerfBoxHist("./sangakkara.csv","Kumar Sangakkara")
tendulkarsp <- getPlayerDataSp(tend_id,tdir=".",tfile="tendulkarsp.csv",ttype="batting")
kallissp <- getPlayerDataSp(kallis_id,tdir=".",tfile="kallissp.csv",ttype="batting")
pontingsp <- getPlayerDataSp(ponting_id,tdir=".",tfile="pontingsp.csv",ttype="batting")
sangakkarasp <- getPlayerDataSp(sangakkara_id,tdir=".",tfile="sangakkarasp.csv",ttype="batting")
batsmanContributionWonLost("tendulkarsp.csv","Tendulkar")
batsmanContributionWonLost("kallissp.csv","Kallis")
batsmanContributionWonLost("pontingsp.csv","Ponting")
batsmanContributionWonLost("sangakkarasp.csv","Sangakkara")
