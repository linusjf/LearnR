#!/usr/bin/env Rscript

######################################################################
# @author      : Linus Fernandes (linusfernandes at gmail dot com)
# @file        : bug
# @created     : Wednesday Jul 05, 2023 05:37:10 IST
#
# @description :
######################################################################
#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
suppressMessages(library(tsibble))
suppressMessages(library(tsibbledata))
suppressMessages(library(ggplot2))
suppressMessages(library(feasts))
packageVersion("feasts")
png(file="img.png")
vic_elec |> gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")
dev.off()
