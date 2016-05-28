require(data.table)

options(datatable.auto.index = FALSE)
options(datatable.optimize = 0)

ts3 <- readRDS("result/tokens.3.stem")
ts2 <- readRDS("result/tokens.2.stem")
ts1 <- readRDS("result//dictionary.all")
t3 <- readRDS("result/tokens.3.word")
t2 <- readRDS("result/tokens.2.word")

set.seed(20141127)
test.ds <- readRDS("test.news")

measure <- sample(test.ds, 1000)

measure <- tolower(processTexts(measure))

measure <- measure[nchar(measure) > 2]

require(RWeka)

tokens <- NGramTokenizer(measure, control = Weka_control(min = 3, max = 3, delimiters = " "))

w3 <- sapply(tokens, function(d) {strsplit(d, " ")[[1]][1:3]})
w3 <- unique(as.data.table(t(w3)))
setnames(w3, c("word1", "word2", "word3"))

require(SnowballC)
w3[, sword1 := wordStem(word1, "english")]
w3[, sword2 := wordStem(word2, "english")]

predictWord <- function(word1, word2, ts3, ts2, ts1, guess = TRUE, stem = FALSE) {
    result <- list()

    if (stem) {
        word1 <- wordStem(word1, "english")
        word2 <- wordStem(word2, "english")
    }

    # try guess type
    ds <- data.table(word1 = word1, word2 = word2)
    if (guess) {
        ts2 <- ts2[, list(word1, word2,
                          t.amount = sum(amount) * (t.amount / sum(t.amount)),
                          b.amount = sum(amount) * (b.amount / sum(b.amount)),
                          n.amount = sum(amount) * (n.amount / sum(n.amount)))]
        g1 <- merge(x = ds,
                    y = ts2,
                    by = c("word1", "word2"),
                    allow.cartesian = TRUE)[ , list(sum(t.amount), sum(b.amount), sum(n.amount))]

        mg1 <- which.max(g1)
        if (mg1 == 1) {
            ts3 <- ts3[, list(word1, word2, word3, amount = t.amount)]
            ts2 <- ts2[, list(word1, word2, amount = t.amount)]
            ts1 <- ts1[, list(lword, word, amount = t.amount)]
        } else if (mg1 == 2) {
            ts3 <- ts3[, list(word1, word2, word3, amount = b.amount)]
            ts2 <- ts2[, list(word1, word2, amount = b.amount)]
            ts1 <- ts1[, list(lword, word, amount = b.amount)]
        } else if (mg1 == 3) {
            ts3 <- ts3[, list(word1, word2, word3, amount = n.amount)]
            ts2 <- ts2[, list(word1, word2, amount = n.amount)]
            ts1 <- ts1[, list(lword, word, amount = n.amount)]
        }
        ts3 <- ts3[amount > 0]
        ts2 <- ts2[amount > 0]
        ts1 <- ts1[amount > 0]
    } else {
        ts3 <- ts3[, list(word1, word2, word3, amount)]
        ts2 <- ts2[, list(word1, word2, amount)]
        ts1 <- ts1[, list(lword, word, amount)]
    }

    # try 3-gram
    p3 <- merge(x = unique(ds),
                y = ts3,
                by = c("word1", "word2"),
                all.x = TRUE)

    # try 2-gram
    if (nrow(p3[is.na(word3)]) > 0) {
        p2 <- merge(x = p3[is.na(word3), list(word1, word2)],
                    y = ts2[, list(word2 = word1, word3 = word2, amount)],
                    by = "word2",
                    allow.cartesian = TRUE,
                    all.x = TRUE)

        if (nrow(p2[is.na(word3)]) > 0) {
            # try 1-gram
            result[[1]] <- p2[is.na(word3), list(word1, word2, word3 = ts1[amount == ts1[, max(amount)], lword])]
        } else {
            result[[1]] <- NULL
        }

        # add 2-gram
        result[[2]] <- merge(x = p2[!is.na(word3), .SD[amount == max(amount), list(word3)], by = list(word1, word2)],
                             y = ts1[, list(word3 = lword, amount)],
                             by = "word3",
                             all.x = TRUE)[is.na(amount), amount := 0][, .SD[amount == max(amount), list(word3 = word3[1])], by = list(word1, word2)]
    } else {
        result[[1]] <- result[[2]] <- NULL
    }

    # add 3-gram
    result[[3]] <- merge(x = merge(x = p3[!is.na(word3), .SD[amount == max(amount), list(word3)], by = list(word1, word2)],
                                   y = ts2[, list(word2 = word1, word3 = word2, amount)],
                                   by = c("word2", "word3"),
                                   all.x = TRUE)[is.na(amount), amount := 0][, .SD[amount == max(amount), list(word3 = word3[1])], by = list(word1, word2)],
                         y = ts1[, list(word3 = lword, amount)],
                         by = "word3",
                         all.x = TRUE)[is.na(amount), amount := 0][, .SD[amount == max(amount), list(word3 = word3[1])], by = list(word1, word2)]

    ds[, pos := 1:nrow(ds)]

    merge(x = ds,
          y = rbindlist(result),
          by = c("word1", "word2"))[order(pos), word3]
}

w3[, p1word := predictWord(word1, word2, t3, t2, ts1, guess = TRUE)]
w3[, p2word := predictWord(word1, word2, t3, t2, ts1, guess = FALSE)]
w3[, ps1word := predictWord(sword1, sword2, ts3, ts2, ts1, guess = TRUE)]
w3[, ps2word := predictWord(sword1, sword2, ts3, ts2, ts1, guess = FALSE)]

t3c <- t3[amount > 10]
t2c <- t2[amount > 10]
t1c <- ts1[amount > 10]
w3[, p1cword := predictWord(word1, word2, t3c, t2c, t1c, guess = TRUE)]

t3sc <- t3[amount > 20]
t2sc <- t2[amount > 20]
t1sc <- ts1[amount > 20]
w3[, p1csword := predictWord(word1, word2, t3sc, t2sc, t1sc, guess = TRUE)]

w3[, .N, word3 == p1word][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)] # BEST!
w3[, .N, word3 == p2word][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)]
w3[, .N, word3 == ps1word][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)]
w3[, .N, word3 == ps2word][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)]

w3[, .N, word3 == p1cword][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)] # SMALL!
w3[, .N, word3 == p1csword][, sum(ifelse(word3 == TRUE, N, 0)) / sum(N)] # SMALL!


testPrediction <- function(word1, word2) {
    rbindlist(list(data.table(guess = TRUE, type = "original", word = predictWord(word1, word2, t3, t2, t1, guess = TRUE)),
                   data.table(guess = FALSE, type = "original", word = predictWord(word1, word2, t3, t2, t1, guess = FALSE)),
                   data.table(guess = TRUE, type = "short", word = predictWord(word1, word2, t3c, t2c, t1c, guess = TRUE)),
                   data.table(guess = FALSE, type = "short", word = predictWord(word1, word2, t3c, t2c, t1c, guess = FALSE)),
                   data.table(guess = TRUE, type = "super short", word = predictWord(word1, word2, t3sc, t2sc, t1sc, guess = TRUE)),
                   data.table(guess = FALSE, type = "super short", word = predictWord(word1, word2, t3sc, t2sc, t1sc, guess = FALSE))))
}
