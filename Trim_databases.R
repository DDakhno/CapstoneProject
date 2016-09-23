load("DB/dt_trigrams/dt_trigrams.Rdata")

head(db_joined)

dim(db_joined)

length(unique(db_joined$predictor))

prds <- unique(db_joined$predictor)

db_max_top100_each_pred <- data.table()
tmp <- data.table()

for (prd in prds[1:1000]) {
    tmp <- na.omit(db_joined[prd]%>%arrange(desc(freq))%>%slice(1:10))
    db_max_top100_each_pred <- bind_rows(db_max_top100_each_pred,tmp)
}