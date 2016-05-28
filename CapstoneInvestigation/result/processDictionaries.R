# union dictionaries
# 1. decide what case is appropriate for each word!
# want with max amount (prefer lword), sum amount for remainings
simplifyDictionary = function (d1) {
    d1[, lword := tolower(word)]
    d1[, `:=`(max_amount = max(amount), sum_amount = sum(amount)), by = lword]
    ds1 <- d1[max_amount == amount, list(word = word[1], amount = sum_amount[1]), by = lword]
    ds1 <- ds1[!is.na(lword) & !is.na(word) & nchar(lword) > 0 & nchar(word) > 0]
    setkey(ds1, lword)
    ds1
}

d.twitter <- simplifyDictionary(twitter.trained$d)
d.news<- simplifyDictionary(news.trained$d)
d.blogs <- simplifyDictionary(blogs.trained$d)

dsj <- merge(x = merge(x = d.twitter[, list(lword, word, t.amount = amount)],
                       y = d.blogs[, list(lword, word, b.amount = amount)],
                       by = c("lword", "word"),
                       all = TRUE),
             y = d.news[, list(lword, word, n.amount = amount)],
             by = c("lword", "word"),
             all = TRUE)

rm(d.twitter, d.news, d.blogs)

#dsj <- dsj[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2]
dsj <- dsj[lword != "[number]"]
dsj[is.na(t.amount), t.amount := 0]
dsj[is.na(b.amount), b.amount := 0]
dsj[is.na(n.amount), n.amount := 0]
dsj[, amount := t.amount + b.amount + n.amount]
setkey(dsj, lword)
saveRDS(dsj, "dictionary.all")

simplifyTokens <- function(n21) {
  w1 <- sapply(n21[, token], function(d) {strsplit(d, " ")[[1]][1:5]})
  n21[, token := NULL]
  n21 <- as.data.table(cbind(t(w1), n21))
  setnames(n21, c("word1", "word2", "word3", "word4", "word5", "amount"))

  n21[, level := 5 - (is.na(word1) + is.na(word2) + is.na(word3) + is.na(word4) + is.na(word5))]
  n21
}

tokens.twitter <- simplifyTokens(twitter.trained$t)
saveRDS(tokens.twitter, "tokens.twitter")
rm(twitter.trained)

tokens.news <- simplifyTokens(news.trained$t)
saveRDS(tokens.news, "tokens.news")
rm(news.trained)

tokens.blogs <- simplifyTokens(blogs.trained$t)
saveRDS(tokens.blogs, "tokens.blogs")
rm(blogs.trained)

tokens.5 <- merge(x = tokens.twitter[level == 5, list(word1, word2, word3, word4, word5, t.amount = amount)],
                  y = tokens.blogs[level == 5, list(word1, word2, word3, word4, word5, b.amount = amount)],
                  by = c("word1", "word2", "word3", "word4", "word5"),
                  all = TRUE)
tokens.5 <- merge(x = tokens.5,
                  y = tokens.news[level == 5, list(word1, word2, word3, word4, word5, n.amount = amount)],
                  by = c("word1", "word2", "word3", "word4", "word5"),
                  all = TRUE)
#tokens.5 <- tokens.5[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2]
tokens.5[is.na(t.amount), t.amount := 0]
tokens.5[is.na(b.amount), b.amount := 0]
tokens.5[is.na(n.amount), n.amount := 0]
tokens.5[, amount := t.amount + b.amount + n.amount]

tokens.5.word <- tokens.5[amount > 1]
setkey(tokens.5.word, word1, word2, word3, word4)
saveRDS(tokens.5.word, "tokens.5.word")
rm(tokens.5.word)

# stem
tokens.5[, word1 := wordStem(word1, "english")]
tokens.5[, word2 := wordStem(word2, "english")]
tokens.5[, word3 := wordStem(word3, "english")]
tokens.5[, word4 := wordStem(word4, "english")]
tokens.5 <- tokens.5[, list(t.amount = sum(t.amount), b.amount = sum(b.amount), n.amount = sum(n.amount), amount = sum(amount)), by = list(word1, word2, word3, word4, word5)]
tokens.5 <- tokens.5[amount > 1]
saveRDS(tokens.5, "tokens.5.stem")
rm(tokens.5)

tokens.4 <- merge(x = tokens.twitter[level == 4, list(word1, word2, word3, word4, t.amount = amount)],
                  y = tokens.blogs[level == 4, list(word1, word2, word3, word4, b.amount = amount)],
                  by = c("word1", "word2", "word3", "word4"),
                  all = TRUE)
tokens.4 <- merge(x = tokens.4,
                  y = tokens.news[level == 4, list(word1, word2, word3, word4, n.amount = amount)],
                  by = c("word1", "word2", "word3", "word4"),
                  all = TRUE)
#tokens.4 <- tokens.4[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2]
tokens.4[is.na(t.amount), t.amount := 0]
tokens.4[is.na(b.amount), b.amount := 0]
tokens.4[is.na(n.amount), n.amount := 0]
tokens.4[, amount := t.amount + b.amount + n.amount]

tokens.4.word <- tokens.4[amount > 1]
setkey(tokens.4.word, word1, word2, word3)
saveRDS(tokens.4.word, "tokens.4.word")
rm(tokens.4.word)

# stem
tokens.4[, word1 := wordStem(word1, "english")]
tokens.4[, word2 := wordStem(word2, "english")]
tokens.4[, word3 := wordStem(word3, "english")]
tokens.4 <- tokens.4[, list(t.amount = sum(t.amount), b.amount = sum(b.amount), n.amount = sum(n.amount), amount = sum(amount)), by = list(word1, word2, word3, word4)]
tokens.4 <- tokens.4[amount > 1]
saveRDS(tokens.4, "tokens.4.stem")
rm(tokens.4)


tokens.3 <- merge(x = tokens.twitter[level == 3, list(word1, word2, word3, t.amount = amount)],
                  y = tokens.blogs[level == 3, list(word1, word2, word3, b.amount = amount)],
                  by = c("word1", "word2", "word3"),
                  all = TRUE)
tokens.3 <- merge(x = tokens.3,
                  y = tokens.news[level == 3, list(word1, word2, word3, n.amount = amount)],
                  by = c("word1", "word2", "word3"),
                  all = TRUE)
#tokens.3 <- tokens.3[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2]
tokens.3[is.na(t.amount), t.amount := 0]
tokens.3[is.na(b.amount), b.amount := 0]
tokens.3[is.na(n.amount), n.amount := 0]
tokens.3[, amount := t.amount + b.amount + n.amount]

tokens.3.word <- tokens.3[amount > 1]
setkey(tokens.3.word, word1, word2, word3)
saveRDS(tokens.3.word, "tokens.3.word")
rm(tokens.3.word)

# stem
tokens.3[, word1 := wordStem(word1, "english")]
tokens.3[, word2 := wordStem(word2, "english")]
tokens.3 <- tokens.3[, list(t.amount = sum(t.amount), b.amount = sum(b.amount), n.amount = sum(n.amount), amount = sum(amount)), by = list(word1, word2, word3)]
tokens.3 <- tokens.3[amount > 1]
saveRDS(tokens.3, "tokens.3.stem")
rm(tokens.3)

tokens.2 <- merge(x = tokens.twitter[level == 2, list(word1, word2, t.amount = amount)],
                  y = tokens.blogs[level == 2, list(word1, word2, b.amount = amount)],
                  by = c("word1", "word2"),
                  all = TRUE)
tokens.2 <- merge(x = tokens.2,
                  y = tokens.news[level == 2, list(word1, word2, n.amount = amount)],
                  by = c("word1", "word2"),
                  all = TRUE)
#tokens.2 <- tokens.2[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2]
tokens.2[is.na(t.amount), t.amount := 0]
tokens.2[is.na(b.amount), b.amount := 0]
tokens.2[is.na(n.amount), n.amount := 0]
tokens.2[, amount := t.amount + b.amount + n.amount]

tokens.2.word <- tokens.2[amount > 1]
setkey(tokens.2.word, word1)
saveRDS(tokens.2.word, "tokens.2.word")
rm(tokens.2.word)

# stem
tokens.2[, word1 := wordStem(word1, "english")]
tokens.2 <- tokens.2[, list(t.amount = sum(t.amount), b.amount = sum(b.amount), n.amount = sum(n.amount), amount = sum(amount)), by = list(word1, word2)]
tokens.2 <- tokens.2[amount > 1]
saveRDS(tokens.2, "tokens.2.stem")
rm(tokens.2)
