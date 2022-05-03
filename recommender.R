library(yorkr)
allBatsmenIPL <-readRDS("./recom_data/all-batsmen.rds")
allBowlersIPL <-readRDS("./recom_data/all-bowlers.rds")

load("./recom_data/IPL-MatchesDataFrame.RData")
df <- NULL
for(i in 1:length(allBatsmenIPL)){
    for(j in 1:length(allBowlersIPL)){
        a <- batsmanVsBowlerPerf(t20MDF,allBatsmenIPL[i],allBowlersIPL[j])
        df <- rbind(df,a)
    }
}

library(reshape2)
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(tidyr)
library(xtabs)
load("recom_data/batsmenVsBowler.rdata")
df[is.na(df)] <-0

df %>% select(bowler1,timesOut) %>% group_by(timesOut) %>% summarise(cases=n()) %>%
    ggplot(aes(timesOut,cases)) + geom_col()

df3 <- select(df, batsman1,bowler1,timesOut)
df4 <- df3 %>%
    pivot_wider(batsman1, names_from = bowler1, values_from = timesOut) %>%
    bind_rows(list(batsman1 = df$bowler1)) %>%
    filter(!duplicated(batsman1))

df5 <- df3 %>%
    dplyr::group_by(batsman1, bowler1) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1L)

df6 <- xtabs(timesOut ~ ., df3)
df7 <- as.data.frame.matrix(df6)
df8 <- data.matrix(df7)

r <- as(df8,"realRatingMatrix")
getRatingMatrix(r)
r_m <- normalize(r)
getRatingMatrix(r_m)

hist(getRatings(r), breaks=100)
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

r1 <- Recommender(r[1:100], method = "POPULAR")
r1
names(getModel(r1))
getModel(r1)$topN

recom <- predict(r1, r[101:102], n=5)
recom

as(recom, "list")

recom3 <- bestN(recom, n = 3)
recom3

as(recom3, "list")

scheme <- evaluationScheme(r[100:200], method="split", train =0.9,k=1, given=-5, goodRating=5)

df7 <- as.data.frame.matrix(df6)

df2 <- df[df$ballsFaced !=0,]

df2 %>% select(bowler1,timesOut) %>% group_by(timesOut) %>% summarise(cases=n()) %>%
    ggplot(aes(timesOut,cases)) + geom_col()

df2$ballsFaced <- (df2$ballsFaced - mean(df2$ballsFaced))/sd(df2$ballsFaced)
df2$totalRuns <- (df2$totalRuns - mean(df2$totalRuns))/sd(df2$totalRuns)
df2$totalRuns <- (df2$fours - mean(df2$fours))/sd(df2$fours)
df2$totalRuns <- (df2$sixes - mean(df2$sixes))/sd(df2$sixes)
df2$totalRuns <- (df2$SR - mean(df2$SR))/sd(df2$SR)
df2$timesOut <- (df2$timesOut - mean(df2$timesOut))/sd(df2$timesOut)


eval_sets <- evaluationScheme(data = df2,
                              method = "cross-validation",
                              k = 10,
                              given = 5,
                              goodRating = 0)
models_to_evaluate <- list(
    `IBCF Cosinus` = list(name = "IBCF",
                          param = list(method = "cosine")),
    `IBCF Pearson` = list(name = "IBCF",
                          param = list(method = "pearson")),
    `UBCF Cosinus` = list(name = "UBCF",
                          param = list(method = "cosine")),
    `UBCF Pearson` = list(name = "UBCF",
                          param = list(method = "pearson")),
    `Zufälliger Vorschlag` = list(name = "RANDOM", param=NULL)
)
n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)
