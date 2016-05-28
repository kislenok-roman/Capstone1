tokens5.stem <- readRDS("result/tokens.5.stem")
tokens5.word <- readRDS("result/tokens.5.word")
tokens4.stem <- readRDS("result/tokens.4.stem")
tokens4.word <- readRDS("result/tokens.4.word")
tokens3.stem <- readRDS("result/tokens.3.stem")
tokens3.word <- readRDS("result/tokens.3.word")
tokens2.stem <- readRDS("result/tokens.2.stem")
tokens2.stem <- readRDS("result/tokens.2.stem")
dict <- readRDS("result//dictionary.all")

require(data.table)
require(RWeka)
require(SnowballC)

predictWord <- function (text, variants = NULL) {
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

    text <- tolower(processTexts(text))

    n5 <- rev(NGramTokenizer(text, control = Weka_control(min = 4, max = 4, delimiters = " ")))[1]
    n5 <- strsplit(n5, " ")[[1]]
    r <- tokens5.stem[as.list(wordStem(n5, "english"))]

    if (!is.null(variants)) {
        r <- r[word5 %in% variants]
    }

    if (nrow(r) == 0 || all(is.na(r$amount))) {
        n4 <- rev(NGramTokenizer(text, control = Weka_control(min = 3, max = 3)))[1]
        r <- tokens.4[as.list(strsplit(n4, " ")[[1]])]

        if (!is.null(variants)) {
            r <- r[word4 %in% variants]
        }

        if (nrow(r) == 0 || all(is.na(r$amount))) {
            n3 <- rev(NGramTokenizer(text, control = Weka_control(min = 2, max = 2)))[1]
            r <- tokens.3[as.list(strsplit(n3, " ")[[1]])]

            if (!is.null(variants)) {
                r <- r[word3 %in% variants]
            }

            if (nrow(r) == 0 || all(is.na(r$amount))) {
                n2 <- rev(NGramTokenizer(text, control = Weka_control(min = 1, max = 1)))[1]
                r <- tokens.2[J(n2)]

                if (!is.null(variants)) {
                    r <- r[word2 %in% variants]
                }

                if (nrow(r) == 0 || all(is.na(r$amount))) {
                    if (!is.null(variants)) {
                        dict[lword %in% variants][order(-amount), list(lword, amount)]
                    } else {
                        dict[order(-amount), list(lword, amount)]
                    }
                } else {
                    r[order(-amount), list(word2, amount)]
                }

            } else {
                r[order(-amount), list(word3, amount)]
            }

        } else {
            r[order(-amount), list(word4, amount)]
        }

    } else {
        r[order(-amount), list(word5, amount)]
    }
}
