tokens3.stem <- readRDS("result/tokens.3.stem")

tokens3.stem[, tmax.amount := t.amount[min(c(.N, 3))], by = list(word1, word2)]
tokens3.stem[, bmax.amount := b.amount[min(c(.N, 3))], by = list(word1, word2)]
tokens3.stem[, nmax.amount := n.amount[min(c(.N, 3))], by = list(word1, word2)]
tokens3.stem[, max.amount := amount[min(c(.N, 3))], by = list(word1, word2)]

tokens3.stem[t.amount >= tmax.amount | b.amount >= bmax.amount | n.amount >= nmax.amount]
