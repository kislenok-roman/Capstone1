# t4, t3, t2, t1
# optimal c or sc (> 10, > 20)

# news <- readLines("data/en_US/en_US.news.txt")
# t1[, `:=`(t.amount = t.amount / sum(t.amount), b.amount = b.amount / sum(b.amount), n.amount = n.amount / sum(n.amount))]
set.seed(20141129)


# BEGIN
max.avialable.ngram <- 3
t3 <- readRDS("result/tokens.3.stem")
t2 <- readRDS("result/tokens.2.stem")
t1 <- readRDS("result/dictionary.all")
setkey(t1, "lword")
setkey(t2, "word1")
setkey(t3, "word1", "word2")

caseWords <- function(words, case = 0:2) {
    if (case == 0) {
        # as is
        words
    } else if (case == 1) {
        # all case are based on statistics
        t1[J(words), word]
    } else {
        # initial letter should be upper
        gsub("^(.)", "\\U\\1", words, perl = TRUE)
    }
}

predictByDictionary <- function(amount.col = c("amount", "twitter", "news", "blogs"),
                                max.output,
                                case = 0:2,
                                wordPart = NA_character_) {
    words <- setnames(t1[, c("lword", amount.col), with = FALSE], c("lword", "amount"))

    if (is.na(wordPart)) {
        words <- words[amount > 0]
    } else {
        words <- words[amount > 0 & lword %like% paste0("^", wordPart)]
    }

    caseWords(na.omit(words[order(-amount)][1:max.output, lword]), case)
}

predictByNGram <- function(input, current.ngram, is.need.wholeword, max.output) {
    if (current.ngram <= max.avialable.ngram) {
        words <- rev(strsplit(input, " ")[[1]])

        if (is.need.wholeword) {
            help.word <- ""
        } else {
            help.word <- words[1]
            words <- tail(words, -1)
        }

        if (help.word != "[number]") {
            if (length(words) + 1 >= current.ngram) {
                words <- rev(words[1:(current.ngram - 1)])
                words <- setnames(setorderv(`[`(get(paste0("t", current.ngram)), setnames(as.data.table(t(words)), paste0("word", 1:length(words)))), amount.col, -1)[, c(paste0("word", current.ngram), amount.col), with = FALSE], c("word", "amount"))[amount > 0 & word != "[number]", word]

                if (nchar(help.word) > 0) {
                    words[grepl(paste0("^", help.word), words)][1:max.output]
                } else {
                    words[1:max.output]
                }
            } else {
                NA_character_
            }
        } else {
            as.character(1:max.output)
        }
    } else {
        NA_character_
    }
}

predictWord <- function(input,
                        amount.col = c(NA_character_, "amount", "t.amount", "b.amount", "n.amount"),
                        case = TRUE,
                        max.ngram = 3,
                        max.output = 5) {
    amount.col <- match.arg(amount.col)

    # remove clutter
    input <- gsub("^[ ]+$", "", gsub("[ ][ ]+", " ", gsub("^[ ]+", "", gsub("\\]", " ", gsub("\\[", " ", gsub("[+|*\\(\\)/\\\"<=>~_@#$%^&:;,-]", " ", gsub("[^\u0001-\u007F]", " ", input)))))))

    input <- gsub("[0-9]+", " [number] ", input)

    # if we still has something
    if (nchar(input) > 0) {
        is.last.end <- grepl("[!.?][ ]?$", input)
        is.need.wholeword <- grepl("[ ]$", input)

        input <- tolower(strsplit(input, "[!.?]([ ]|$)")[[1]])

        if (is.na(amount.col)) {
            # need to guess amount.col
            input <- input[nchar(input) > 0]

            amount.col <- c("t.amount", "n.amount", "b.amount")[which.max(t(t1[J(unlist(strsplit(input, " "))), list(sum(t.amount, na.rm = TRUE), sum(n.amount, na.rm = TRUE), sum(b.amount, na.rm = TRUE))]))]
        }

        if (is.last.end) {
            # we have new sentence
            list(amount.col = amount.col,
                 words = predictByDictionary(amount.col, max.output, ifelse(case, 2, 0)))
        } else {
            input <- tail(input, 1)

            # decide for max ngram:
            # - depend on user input
            # - but we can't use 4gram if there is only 2 words
            max.ngram <- min(max.ngram, 1 + nchar(input) - nchar(gsub(" ", "", input)))

            result <- c()

            # do while we
            current.ngram <- max.ngram
            while (current.ngram > 1 && length(result) < max.output) {
                result <- unique(c(result, na.omit(predictByNGram(input, current.ngram, is.need.wholeword, max.output))))
                current.ngram <- current.ngram - 1
            }

            if (length(result) > 0) {
                result <- caseWords(result, ifelse(case, 1, 0))
            }

            if (length(result) < max.output) {
                # add from dictionary
                if (is.need.wholeword) {
                    list(amount.col = amount.col,
                         words = as.vector(na.omit(unique(c(result, predictByDictionary(amount.col, 2 * max.output, ifelse(case, 1, 0))))[1:max.output])))
                } else {
                    # plus use the fact that we know part of the word!
                    list(amount.col = amount.col,
                         words = as.vector(na.omit(unique(c(result, predictByDictionary(amount.col, 2 * max.output, ifelse(case, 1, 0), rev(strsplit(input, " ")[[1]])[1])))[1:max.output])))

                }
            } else {
                list(amount.col = amount.col, words = result[1:max.output])
            }
        }
    } else {
        # here we even can't guess, but we know that letter is first!
        if (is.na(amount.col)) {
            amount.col <- "amount"
        }
        list(amount.col = "amount", words = predictByDictionary(amount.col, max.output, ifelse(case, 2, 0)))
    }
}
