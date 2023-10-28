#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : lab
# @created     : Saturday Oct 28, 2023 15:08:50 IST
#
# @description :
######################################################################
library(nberwp)
data(paper_authors)
data(authors)
data(papers)

print(str(paper_authors))
print(str(authors))
print(str(papers))

papers <- subset(papers, year >= 2016)
paper_authors <- subset(paper_authors, paper_authors$paper %in% papers$paper)
paper_authors$author <- authors$user_nber[match(paper_authors$author , authors$author)]

head(paper_authors)
