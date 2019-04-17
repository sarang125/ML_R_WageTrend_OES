# Analyze the clustering if any among the 20 occumpations depicting the..
#...trends in average incomes over the 15 years period starting 2001

# Read in the file 

base <- readRDS('C:/Users/SARANG/Downloads/oes.rds')
typeof(base)

# Check for any pre-processing requirements

head(base)
summary(base)

# All in place - No NAs, categorical variables or scaling requirements

dist_oes <- dist(base, method = 'euclidean')
hclust_oes <- hclust(dist_oes, method = 'average')
dend_oes <- as.dendrogram(hclust_oes)
plot(dend_oes)

install.packages('colorspace')
library(colorspace)
install.packages('dendextend')
library(dendextend)

dend_colored <- color_branches(dend_oes)
plot(dend_colored)

# Making the data into tidy dataframe for plotting and handling

library(tibble)
library(tidyr)

df_base <- rownames_to_column(as.data.frame(base), var = 'occupation')
typeof(df_base)

cut_oes <- cutree(hclust_oes, h = 90000)
cut_oes
clust_oes <- mutate(df_base, cluster = cut_oes)
head(clust_oes)

# Create a tidy dataframe with these variables as column

oes <- gather(data = clust_oes, key = year, value = mean_salary, -occupation, -cluster)
head(oes)

library(ggplot2)
ggplot(oes, aes(x = year, y = mean_salary, color = factor(cluster)))+
  geom_line(aes(group = occupation))

# Exploring K-Means

library(purrr)
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = base, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:10, tot_withinss = tot_withinss)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

# Exploring Silhouette average width approach

library(cluster)
sil_width <- map_dbl(2:10, function(k){
  model <- pam(base,k = k)
  model$silinfo$avg.width
})

sil_df <- data.frame(k = 2:10, sil_width = sil_width)

ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)
  