# load data
twitter <- readLines("data/en_US/en_US.twitter.txt")
blogs <- readLines("data/en_US/en_US.blogs.txt")
news <- readLines("data/en_US/en_US.news.txt")

# split
processTexts <- function(s.text) {
    # split by inner bracket
    s.text <- c(gsub("[(].+?[)]", "", s.text),
                regmatches(s.text, regexpr("[(].+?[)]", s.text)))

    # split by sentance
    s.text <- unlist(strsplit(s.text, "[!.?]"))

    # clear punctuation
    s.text <- gsub("[^\u0001-\u007F]", " ", s.text)
    s.text <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", s.text)
    s.text <- gsub("\\]", " ", gsub("\\[", " ", gsub("[…|•“”!\"#&$%\\(\\)*+./:;<=>?@^_`\\{|\\}~,/\\-]", " ", s.text)))
    # potentially: [,] -> [ ][,][ ]
    # potentially: [ ][-][ ] -> []

    # numbers remove
    s.text <- gsub("[0-9]+", " [number] ", s.text)

    # clear whitespaces
    s.text <- gsub("[ ]{2, }", " ", s.text)

    s.text
}

train.portition <- 0.7

twitter <- processTexts(twitter)
blogs <- processTexts(blogs)
news <- processTexts(news)

set.seed(198609)
idx <- sample(1:length(twitter), length(twitter) * train.portition)
train.twitter <- twitter[idx]
test.twitter <- twitter[-idx]
rm(twitter)

idx <- sample(1:length(blogs), length(blogs) * train.portition)
train.blogs <- blogs[idx]
test.blogs <- blogs[-idx]
rm(blogs)

idx <- sample(1:length(news), length(news) * train.portition)
train.news <- news[idx]
test.news <- news[-idx]
rm(news)

saveRDS(test.blogs, "test.blogs")
saveRDS(test.news, "test.news")
saveRDS(test.twitter, "test.twitter")

rm(test.blogs, test.news, test.twitter)

saveRDS(train.blogs, "train.blogs")
saveRDS(train.news, "train.news")
saveRDS(train.twitter, "train.twitter")

rm(train.blogs, train.news)

require(tm)
require(RWeka)
require(data.table)

# 1. Groups

processGroup <- function(texts, ssize = 0, max.token = 3) {
    # sample
    if (ssize == 0) {
        s.text <- texts
        ssize = length(texts)
    } else {
        s.text <- sample(texts, ssize)
    }

    s.text <- processTexts(s.text)

    # lowercase
    l.text <- tolower(s.text)

    # TOKENIZE
    if (max.token < 2) {
        max.token <- 2
    }
    res <- rbindlist(lapply(2:max.token, function(level) {
        data.table(token = NGramTokenizer(l.text, control = Weka_control(min = level, max = level, delimiters = " ")),
                   level = level)
    }))

    # encoding
    Encoding(res$token) <- 'unknown'
    dict <- data.table(word = NGramTokenizer(s.text, control = Weka_control(min = 1, max = 1, delimiters = " ")))[, list(amount = .N), list(word, lword = tolower(word))]
    Encoding(dict$word) <- 'unknown'
    Encoding(dict$lword) <- 'unknown'

    list(ngram = res[, list(amount = .N), by = list(level, token)],
         dict = dict)
}

max.size <- 1000

trainByData <- function(ds, max.size, max.level = 5) {
    max.i <- ceiling(length(ds) / max.size)
    resDictionary <- vector("list", max.i)
    resTokens <- vector("list", max.i * (max.level - 1))
    for (i in 1:max.i) {
        # dictionary
        low <- (i - 1) * max.size + 1
        top <- min(c(i * max.size, length(ds)))

        texts <- ds[low:top]
        resDictionary[[i]] <- data.table(word = NGramTokenizer(texts, control = Weka_control(min = 1, max = 1, delimiters = " ")))[, list(amount = .N), by = word]

        resTokens[[i]] <- data.table(token = NGramTokenizer(tolower(texts), control = Weka_control(min = 2, max = max.level, delimiters = " ")))[, list(amount = .N), by = token]
    }

    list(d = rbindlist(resDictionary)[, list(amount = sum(amount)), by = word],
         t = rbindlist(resTokens)[, list(amount = sum(amount)), by = token])
}
twitter.trained <- trainByData(readRDS("train.twitter"), max.size, 5)
rm(train.twitter)
saveRDS(twitter.trained, "twitter.trained")
rm(twitter.trained)

news.trained <- trainByData(readRDS("train.blogs"), max.size, 5)
saveRDS(news.trained, "news.trained")
rm(news.trained)
blogs.trained <- trainByData(readRDS("train.blogs"), max.size, 5)
saveRDS(blogs.trained, "blogs.trained")
