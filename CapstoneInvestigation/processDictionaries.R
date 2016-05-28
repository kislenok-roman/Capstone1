# union dictionaries
# 1. decide what case is appropriate for each word!
# want with max amount (prefer lword), sum amount for remainings
simplifyDictionary = function (d1) {
    d1[, `:=`(max_amount = max(amount), sum_amount = sum(amount)), by = lword]
    ds1 <- d1[max_amount == amount, list(word = word[1], amount = sum_amount[1]), by = lword]
    ds1 <- ds1[!is.na(lword) & !is.na(word) & nchar(lword) > 0 & nchar(word) > 0]
    setkey(ds1, lword)
    ds1
}

ds1 <- simplifyDictionary(d1)
ds2 <- simplifyDictionary(d2)
ds3 <- simplifyDictionary(d3)

rm(d1, d2, d3)

dsj <- merge(x = merge(x = ds1[, list(lword, word, t.amount = amount)],
                       y = ds2[, list(lword, word, b.amount = amount)],
                       by = c("lword", "word"),
                       all = TRUE),
             y = ds3[, list(lword, word, n.amount = amount)],
             by = c("lword", "word"),
             all = TRUE)

rm(ds1, ds2, ds3)

dsj <- dsj[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2][lword != "[number]"]
dsj[is.na(t.amount), t.amount := 0]
dsj[is.na(b.amount), b.amount := 0]
dsj[is.na(n.amount), n.amount := 0]
dsj[, amount := t.amount + b.amount + n.amount]
setkey(dsj, lword)

simplifyPairs <- function(n21) {
    n21[, word1 := sapply(token, function(d) {strsplit(d, " ")[[1]][1]})]
    n21[, word2 := sapply(token, function(d) {strsplit(d, " ")[[1]][2]})]

    n21 <- n21[, list(word1, word2, amount)]
    n21
}

n21 <- simplifyPairs(n21)
n22 <- simplifyPairs(n22)
n23 <- simplifyPairs(n23)

n2j <- merge(x = merge(x = n21[, list(word1, word2, t.amount = amount)],
                       y = n22[, list(word1, word2, b.amount = amount)],
                       by = c("word1", "word2"),
                       all = TRUE),
             y = n23[, list(word1, word2, n.amount = amount)],
             by = c("word1", "word2"),
             all = TRUE)

rm(n21, n22, n23)
gc()
n2j <- n2j[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2][!is.na(word2)]
n2j[is.na(t.amount), t.amount := 0]
n2j[is.na(b.amount), b.amount := 0]
n2j[is.na(n.amount), n.amount := 0]
n2j[, amount := t.amount + b.amount + n.amount]
setkey(n2j, word1)

simplifyTriplets <- function(n31) {
    n31[, word1 := sapply(token, function(d) {strsplit(d, " ")[[1]][1]})]
    n31[, word2 := sapply(token, function(d) {strsplit(d, " ")[[1]][2]})]
    n31[, word3 := sapply(token, function(d) {strsplit(d, " ")[[1]][3]})]

    n31[, list(word1, word2, word3, amount)][!is.na(word1) & !is.na(word2)]
}

n31 <- simplifyTriplets(n31)
n32 <- simplifyTriplets(n32)
n33 <- simplifyTriplets(n33)

n3j <- merge(x = merge(x = n31[, list(word1, word2, word3, t.amount = amount)],
                       y = n32[, list(word1, word2, word3, b.amount = amount)],
                       by = c("word1", "word2", "word3"),
                       all = TRUE),
             y = n33[, list(word1, word2, word3, n.amount = amount)],
             by = c("word1", "word2", "word3"),
             all = TRUE)

rm(n31, n32, n33)
gc()
n3j <- n3j[is.na(t.amount) + is.na(b.amount) + is.na(n.amount) < 2][!is.na(word2) & !is.na(word3)]
n3j[is.na(t.amount), t.amount := 0]
n3j[is.na(b.amount), b.amount := 0]
n3j[is.na(n.amount), n.amount := 0]
n3j[, amount := t.amount + b.amount + n.amount]
setkey(n3j, word1, word2)

# simplify
#n3j[, max_amount := max(min(amount), amount[order(-amount)][5], na.rm = TRUE), by = list(word1, word2)]
#s3j <- n3j[amount >= max_amount, list(word1, word2, word3, t.amount, b.amount, n.amount, amount)]

