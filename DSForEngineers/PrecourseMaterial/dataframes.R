#!/usr/bin/env Rscript
library(reshape2)

vec1 <- c(1, 2, 3)
vec2 <- c("R", "Scilab", "Java")
vec3 <- c("For prototyping", "For prototyping", "For Scaleup")
df <- data.frame(vec1, vec2, vec3)
print(df)

# accessing first & second row:
print(df[1:2, ])
# accessing first & second column:
print(df[, 1:2])
# accessing 1st & 2nd column – alternate:
print(df[1:2])

# Data frame example 2
pd <- data.frame(Name = c("Senthil", "Senthil", "Sam", "Sam"), Month = c(
  "Jan", "Feb",
  "Jan", "Feb"
), BS = c(141.2, 139.3, 135.2, 160.1), BP = c(90, 78, 80, 81))
pd2 <- subset(pd, Name == "Senthil" | BS > 150)
print("new subset pd2")
print(pd2)

df[[2]][2] <- "R"
print(df)

# continuing from previous example adding extra row and column:
df <- rbind(df, data.frame(vec1 = 4, vec2 = "C", vec3 = "For Scaleup"))
print("adding extra row")
print(df)
df <- cbind(df, vec4 = c(10, 20, 30, 40))
print("adding extra col")
print(df)

# Deleting rows and columns:
df2 <- df[-3, -1]
print(df2)
# conditional deletion:
df3 <- df[, !names(df) %in% c("vec3")]
print(df3)
df4 <- df[!df$vec1 == 3, ]
print(df4)

# Manipulating rows in data frame continued from previous page
df[3, 1] <- 3.1
df[3, 3] <- "Others"
print(df)

vec1 <- c(1, 2, 3)
vec2 <- c("R", "Scilab", "Java")
vec3 <- c("For prototyping", "For prototyping", "For Scaleup")
df <- data.frame(vec1, vec2, vec3, stringsAsFactors = FALSE)
# Now trying the same manipulation
df[3, 3] <- "Others"
print(df)

# Data frame example 2
pd <- data.frame(Name = c("Senthil", "Senthil", "Sam", "Sam"), Month = c(
  "Jan", "Feb",
  "Jan", "Feb"
), BS = c(141.2, 139.3, 135.2, 160.1), BP = c(90, 78, 80, 81))

Df <- melt(pd, id.vars = c("Name", "Month"), measure.vars = c("BS", "BP"))
print(Df)

# cast operation sample code continued from previous code we use dcast as we are
# working on a dataframe
Df2 <- dcast(Df, variable + Month ~ Name, value.var = "value")
print(Df2)
