#!/usr/bin/env Rscript
suppressMessages(library(stringdist))
suppressMessages(library(qdap))

master_recs <- c(
  "Lumiere Technologies",
  "Mendes Metal",
  "Klapisch Aerospace"
)

text_corpus <- c(
  "Equipment ONLY - Lumiere Technologies",
  "Lumiere Technologies",
  "Lumiere Tech, Inc.",
  "Mendes Metal SA - Central Office",
  "*** DO NOT USE *** Mendes Metal",
  "Mendes Metal, SA",
  "Ship to Klapisch Aerospace gmbh",
  "Klapisch Aero, gmbh Munich",
  "Klapisch Aerospace tech (use Klapisch Aero, gmbh Munich acct 84719482-A)"
)

stopwords <- c(
  "only",
  "to",
  "***",
  "do",
  "not",
  "use",
  "-",
  "acct",
  "84719482-A",
  "(",
  ")",
  "a"
)

metrics <- c(
  "osa",
  "lv",
  "dl",
  "hamming",
  "lcs",
  "qgram",
  "cosine",
  "jaccard",
  "jw"
)

text_corpus <- rm_stopwords(text_corpus,
  stopwords = stopwords,
  separate = FALSE, strip = TRUE,
  ignore.case = TRUE,
  apostrophe.remove = TRUE
)

text_corpus
colnames <- c("Master", "Record", metrics)

# Create empty data frame
data <- data.frame(matrix(NA,
  nrow = 0,
  ncol = 2 + length(metrics)
))
names(data) <- colnames
data[, 1] <- as.numeric(data[, 1])
data[, 2] <- as.numeric(data[, 2])

for (rec in master_recs) {
  for (datum in text_corpus) {
    scores <- c()
    for (method in metrics) {
      score <- stringsim(
        tolower(rec),
        datum,
        method = method,
        useBytes = FALSE,
        q = 1
      )
      scores <- append(scores, score)
    }
    data[nrow(data) + 1, ] <- c(rec, datum, scores)
  }
}
df <- data[, 3:11]
df <- as.data.frame(lapply(df, as.numeric))
df <- as.data.frame(df)
means <- rowMeans(df[1:nrow(df), ])
data$MeanScore <- means
result <- subset(data, MeanScore > 0.5, select = c("Master", "Record", "MeanScore"))
result <- as.data.frame(t(as.matrix(result)))
print(format(result))
