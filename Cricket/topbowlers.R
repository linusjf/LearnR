#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : topbowlers
# @created     : Wednesday Sep 20, 2023 22:21:37 IST
#
# @description :
######################################################################
library(cricketr)
library(cricketdata)
murali_id = find_player_id("Muttiah Muralitharan")$ID
print(murali_id)
murali <- getPlayerData(murali_id,dir=".",file="murali.csv",type="bowling",homeOrAway=c(1,2), result=c(1,2,4))
warne_id = find_player_id("Shane Warne")$ID
print(warne_id)
warne <- getPlayerData(warne_id,dir=".",file="warne.csv",type="bowling",homeOrAway=c(1,2), result=c(1,2,4))
kumble_id = find_player_id("Anil Kumble")$ID
print(kumble_id)
kumble <- getPlayerData(kumble_id,dir=".",file="kumble.csv",type="bowling",homeOrAway=c(1,2), result=c(1,2,4))
