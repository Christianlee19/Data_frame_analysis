## 1) select columns of interest 
## 2) get counts of unique values 
## 3) boxplot w/ points


# 1)
df = read.csv("TCMA.csv", header = TRUE, stringsAsFactors = F) 
head(df)
df_new = df[ , c('Sample', 'Type')]
head(df_new)


# 2)
datalist = list()
datalist2 = list()
for (i in unique(df_new$Type)){
  iterative_df = df_new[df_new$Type == i, ]                          # matrix contains the info for unique cancer type
  x = data.frame(table(iterative_df$Sample), stringsAsFactors = F)   # number of types specific sample(id) are repeated 
  y = data.frame(mean(x$Freq), median(x$Freq), stringsAsFactors = F) # store mean and median values in 'y'
  x$Type = i                                                         # make a new column for cancer type
  datalist[[i]] = x
  datalist2[[i]] = y
}
combined = do.call(rbind, datalist)
combined2 = do.call(rbind, datalist2)


# 3)
library(ggplot2)
plot = ggplot(combined, aes(x=Type, y=Freq)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_point(size = 1, shape = 21, alpha = 0.35, position=position_jitter())
plot + labs(title = "Mutation counts in patients across cancer types")
