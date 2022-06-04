### Boruta ###


library(dplyr)
library(Boruta)
library(readr)

## Dataframe ##

df <- read_csv("C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 2 - Marketing Campaign Acceptance Prediction/df_eda.csv")
df <- subset(df, select = -...1)

#df_boruta <- read_csv("df_boruta.csv")
#df <- subset(df_boruta, select = c(-X))


### Database summary ###

attach(df)
dim(df)
str(df)
summary(df)


##################### BORUTA #####################


par(mfrow = c(1,1))
set.seed(222)
df_boruta_2 <- df[, c('Income',
                      'enrolled_months',
                      'education_ordinary_domain', 'marital_status_ordinary_domain',
                      'age',
                      'Kidhome', 'Teenhome', 'children_number',
                      'Recency',
                      'MntWines', 'wines_perc', 'MntFruits', 'fruits_perc', 'MntMeatProducts', 'meat_perc', 'MntFishProducts', 'fish_perc',
                      'MntSweetProducts', 'sweet_perc', 'MntGoldProds', 'total_spent',   
                      'NumDealsPurchases', 'NumWebPurchases', 'web_perc', 'NumCatalogPurchases', 'catalog_perc', 'NumStorePurchases', 'store_perc',
                      'total_purchases',
                      'spent_per_purchase',
                      'NumWebVisitsMonth',
                      'Complain',
                      'AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'total_campaigns',
                      'Response')]
Boruta(Response ~ ., data = df_boruta_2, doTrace=2, maxRuns = 20, pValue = 0.01) -> Boruta_df
#Shadows attributes should be rejected
print(Boruta_df)
plot(Boruta_df)
stats <- attStats(Boruta_df)
stats$features <- row.names(stats)
stats <- stats %>% arrange(desc(meanImp))
stats
plotImpHistory(Boruta_df)

write.csv(stats, 'C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 2 - Marketing Campaign Acceptance Prediction/df_importance.csv')