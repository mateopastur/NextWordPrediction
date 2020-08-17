library(quanteda)
library(ngram)
library(dplyr)
library(data.table)
library(tokenizers)
blogs <- file("en_US.blogs.txt","rb")
news <- file("en_US.news.txt","rb")
twitter <- file("en_US.twitter.txt","rb")
blogs <- readLines(blogs, skipNul = TRUE)
news <- readLines(news, skipNul = TRUE)
twitter <- readLines(twitter, skipNul = TRUE)
master_vector <- c(twitter[sample(length(twitter), length(twitter) * 0.1)],blogs[sample(length(blogs), length(blogs) * 0.1)],news[sample(length(news), length(news) * 0.1)])
corp <- corpus(master_vector)
master_Tokens <- tokens(
        x = tolower(corp),
        remove_punct = TRUE,
        remove_twitter = TRUE,
        remove_numbers = TRUE,
        remove_hyphens = TRUE,
        remove_symbols = TRUE,
        remove_url = TRUE
)
stemed_words <- tokens_wordstem(master_Tokens, language = "english")
bi_gram <- tokens_ngrams(stemed_words, n = 2)
tri_gram <- tokens_ngrams(stemed_words, n = 3)
four_gram <- tokens_ngrams(stemed_words, n = 4)

uni_DFM <- dfm(stemed_words)
bi_DFM <- dfm(bi_gram)
tri_DFM <- dfm(tri_gram)
four_DFM <- dfm(four_gram)

uni_DFM <- dfm_trim(uni_DFM, 3)
bi_DFM <- dfm_trim(bi_DFM, 3)
tri_DFM <- dfm_trim(tri_DFM, 3)
four_DFM <- dfm_trim(four_DFM, 3)

sums_U <- colSums(uni_DFM)
sums_B <- colSums(bi_DFM)
sums_T <- colSums(tri_DFM)
sums_4 <- colSums(four_DFM)

uni_words <- data.table(word1 = names(sums_U), count = sums_U)

bi_words <- data.table(
        word1 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sums_B), "_", fixed = TRUE), '[[', 2),
        count = sums_B)

tri_words <- data.table(
        word1 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 2),
        word3 = sapply(strsplit(names(sums_T), "_", fixed = TRUE), '[[', 3),
        count = sums_T)

four_words <- data.table(
        word1 = sapply(strsplit(names(sums_4), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sums_4), "_", fixed = TRUE), '[[', 2),
        word3 = sapply(strsplit(names(sums_4), "_", fixed = TRUE), '[[', 3),
        word4 = sapply(strsplit(names(sums_4), "_", fixed = TRUE), '[[', 4),
        count = sums_4)

ngramOne <- uni_words[order(uni_words$count,decreasing = TRUE),]
ngramTwo <- bi_words[order(bi_words$count,decreasing = TRUE),]
ngramThree <- tri_words[order(tri_words$count,decreasing = TRUE),]
ngramFour <- four_words[order(four_words$count,decreasing = TRUE),]

saveRDS(ngramOne,file="ngramOne.rds")
saveRDS(ngramTwo,file="ngramTwo.rds")
saveRDS(ngramThree,file="ngramThree.rds")
saveRDS(ngramFour,file="ngramFour.rds")
