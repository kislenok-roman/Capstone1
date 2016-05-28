predict.text <- function(text, union = TRUE) {
    s.text <- processTexts(text)
    decompose3 <- NGramTokenizer(s.text, control = Weka_control(min = 3, max = 3, delimitets = " "))
    decompose2 <- NGramTokenizer(s.text, control = Weka_control(min = 2, max = 2, delimitets = " "))
    decompose1 <- NGramTokenizer(s.text, control = Weka_control(min = 1, max = 1, delimitets = " "))

    if (union) {
        # union dictionaries
        d <- rbindlist(list(d1, d2, d3))[, list(amount = sum(amount)), by = list(word, lword)][, list(word, lword, p = amount / sum(amount))]
        n2 <- rbindlist(list(n21, n22, n23))[, list(amount = sum(amount)), by = list(token)][, list(token, p = amount / sum(amount))]
        n3 <- rbindlist(list(n31, n32, n33))[, list(amount = sum(amount)), by = list(token)][, list(token, p = amount / sum(amount))]
    } else {
        # find most appropriate dictionary

    }

    # key
    setkey(d, lword)
    setkey(n3, token)
    setkey(n2, token)

    # predict
    p3 <- n3[token %like% paste0("^", tail(decompose2, 1), " ")]
    # p2 <- n2[token %like% paste0("^", tail(decompose1, 1), " ")]

    #
    p3[apply(sapply(variants, function(d) {grepl(paste0(" ", d, "$"), token)}), 1, any)][order(-p)]
    p2[apply(sapply(variants, function(d) {grepl(paste0(" ", d, "$"), token)}), 1, any)][order(-p)]



}
