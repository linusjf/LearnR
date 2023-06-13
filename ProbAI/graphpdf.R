#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : graphpdf
# @created     : Tuesday Jun 13, 2023 19:57:30 IST
#
# @description : Graphing pdf in R
######################################################################
# Plotting the normal distribution of children's IQ scores
# mean of 100 and sd 15
# What percentage of children have an iq score > 115?
# Define parameters of our distribution
mean = 100
stddev = 15
# Define lower and upper bounds of area of interest
lowerbound = 115
upperbound = Inf
