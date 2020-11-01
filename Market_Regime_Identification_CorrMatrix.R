---
title: "market_regime_identification_Using_CorrMatrix"
author: "Ran Cao"
date: "8/21/2020"
output: html_document
---

# import data & data pre-processing
library(readr)
asset_returns <- read.csv("~/Library/asset_returns.csv")
View(asset_returns)
ncol(asset_returns)
factors_returns <- read.csv("~/Library/factors_returns.csv")
ncol(factors_returns)
# delete columns that contain NA 
factors_returns<-subset(factors_returns, select=colMeans(is.na(factors_returns)) == 0) 
asset_returns<-subset(asset_returns, select=colMeans(is.na(asset_returns)) == 0) # change from 61 to 51
#convert data type
factors_returns$fecha = as.POSIXct(strptime(factors_returns$fecha, format = "%Y-%m-%d"))
# combine two datasets together
whole_dataset = cbind(factors_returns,asset_returns) # first 50 are factors, last 51 are asset
ncol(whole_dataset)
View(whole_dataset)

# correlation matrix difference and largest eigenvalue difference visualization & clustering with PAM method
library(lubridate)
window_list = list()
matrix_list = list()
largest_eigenvalue = list()
diff_list = list()
per_row_result = list()
eigenvalue_diff_list = list()
eigenvalue_diff = list()
# 20 year data
starting_date = whole_dataset$fecha[1]
ending_date = tail(whole_dataset$fecha,1)
# initial setup (the first window) for a two year length
window_date_function <- function(c) { 
  window_start_date <- starting_date
  window_end_date <- whole_dataset$fecha[1] %m+% months(c)
  window_list[[1]] = c(window_start_date,window_end_date)
  i = 2
  while (window_end_date <= ending_date)
  {
    window_start_date <- window_start_date %m+% months(1)
    window_end_date <- window_start_date %m+% months(c)
    window_list[[i]] <<- c(window_start_date,window_end_date)
    i <- i+1
  }
  t = length(window_list)
  window_list[[t]][2] <<- ending_date
  # matrix list
  # loop
  for (i in 1:length(window_list)){
    two_year_window = whole_dataset %>% dplyr::filter(fecha>=window_list[[i]][1]& fecha<=window_list[[i]][2]) %>% select(-1) 
    matrix_list[[i]] <<- cor(two_year_window, method = "pearson", use = "complete.obs")
    largest_eigenvalue[[i]] <- eigen(matrix_list[[i]])$values[1]
  }
  # get the differences for correlation matrix diffenrences
  num_variale = ncol(whole_dataset)-1 # since the first column is time, not assets
  # loop
  for (i in 1:length(matrix_list)){
    for(j in 1:length(matrix_list)){
      matrix_diff = matrix_list[[i]]-matrix_list[[j]]
      per_row_result[[j]] <- abs(sum(matrix_diff))/num_variale/num_variale
    }
    diff_list[[i]] <<- per_row_result
  }
  corr_matrix_diff <- data.frame(matrix(unlist(diff_list), nrow = t, byrow = T), stringsAsFactors = F)
  corr_matrix_diff_list <<- as.matrix(corr_matrix_diff)
  # create diff list for the eigenvalue
  for (i in 1:length(largest_eigenvalue)){
    for(j in 1:length(largest_eigenvalue)){
      matrix_diff = largest_eigenvalue[[i]]-largest_eigenvalue[[j]]
      eigenvalue_diff[[j]] <- abs(sum(matrix_diff))
    }
    eigenvalue_diff_list[[i]] <<- eigenvalue_diff
  }
  largesteigenvalue_diff <- data.frame(matrix(unlist(eigenvalue_diff_list), nrow = t, byrow = T), stringsAsFactors = F)
  largesteigenvalue_diff_list <<- as.matrix(largesteigenvalue_diff)
  # change the column/row names
  starting = ymd('2000-06-05') 
  tt = c(starting)
  for (i in 2:t){
    starting = starting %m+% months(1)
    tt[i] = starting
  }
  tt = as.character(tt)
  # visualize corr matrix diff 
  colnames(corr_matrix_diff_list) <<-tt
  rownames(corr_matrix_diff_list) <<-tt
  colnames(largesteigenvalue_diff_list) <<-tt
  rownames(largesteigenvalue_diff_list) <<-tt
}
# main 
window_date_function(c=6) # c represents the months. half year c=6, one year c=12, 1.5 year c=18, 2 year c=24
View(largesteigenvalue_diff_list)
View(corr_matrix_diff_list)
# one year 
window_date_function(c=12)
# one and half year 
window_date_function(c=18)
# two years
window_date_function(c=24)
# visdualization for corr matrix & largest 5 eigenvalues
library(gplots)
my_palette <- colorRampPalette(c("white","orange"))
heatmap.2(corr_matrix_diff_list,
          main = "Corr Matrices Differences", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map                                  
          col=my_palette, 
          Rowv=FALSE,
          Colv=FALSE,
          dendrogram="none") 
heatmap.2(largesteigenvalue_diff_list,
          main = "Largest Eigenvalue Differences", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map                                  
          col=my_palette, 
          Rowv=FALSE,
          Colv=FALSE,
          dendrogram="none") 
###################################### clustering
# correlation matrix diff
library(cluster)
library(ggplot2)
silhouette_width = sapply(2:10,
                          FUN = function(x) pam(x = corr_matrix_diff_list,k = x,diss=TRUE)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
# potential data points: 4 and 7 (2 years window length)
pam.corrmatrix4 <- pam(corr_matrix_diff_list, 4,diss=TRUE)
pam.corrmatrix7 <- pam(corr_matrix_diff_list, 7,diss=TRUE)
print(pam.corrmatrix4)
pam.corrmatrix4$clustering
pam.corrmatrix7$clustering
corrmatrix4 = list()
for (i in 1:4){
  corrmatrix4[[i]] = pam.corrmatrix4$clustering[pam.corrmatrix4$clustering == i]
}
corrmatrix7 = list()
for (i in 1:7){
  corrmatrix7[[i]] = pam.corrmatrix7$clustering[pam.corrmatrix7$clustering == i]
}
# check # optimal points for the 5 largest eigenvalue approach
silhouette_width = sapply(2:10,
                          FUN = function(x) pam(x = largesteigenvalue_diff_list,k = x,diss=TRUE)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
# potential data points: 2 (half year)
pam.eigenvalue2 <- pam(largesteigenvalue_diff_list, 2,diss=TRUE)
eigenvalue2 = list()
for (i in 1:2){
  eigenvalue2[[i]] = pam.eigenvalue2$clustering[pam.eigenvalue2$clustering == i]
}
list1 <- corrmatrix4[1][1]
df1 <- as.data.frame(list1)
vec1 <- row.names(df1)
list2 <- corrmatrix4[2][1]
df2 <- as.data.frame(list2)
vec2 <- row.names(df2)
list3 <- corrmatrix4[3][1]
df3 <- as.data.frame(list3)
vec3 <- row.names(df3)
list4 <- corrmatrix4[4][1]
df4 <- as.data.frame(list4)
vec4 <- row.names(df4)
# save output as csv
write.csv(vec1,"~/Library/regime1",row.names = FALSE)

# optimiziton1: select 5 largest eigenvalues instead of only one largest eigenvalue
# based on two measures (Euclidean Distance & Cosine Similarity)
temp_matrix_list = list()
five_largest_eigenvalue = list()
# loop
for (i in 1:length(window_start_end_list[[1]])){
  two_year_window = whole_dataset %>% dplyr::filter(fecha>=window_start_end_list[[1]][[i]][1]& fecha<=window_start_end_list[[1]][[i]][2]) %>% select(-1) 
  temp_matrix_list[[i]] = cor(two_year_window, method = "pearson", use = "complete.obs")
  five_largest_eigenvalue[[i]] <- eigen(temp_matrix_list[[i]])$values[1:5]
}
# calculate distance between two vectors - two ways to calculate: Euclidean Distance and Cosine Similarity 
library(geometry)
# get the differences for correlation matrix diffenrences
euclidean_distance = list()
euclidean_distance_per_row = list()
cosine_distance = list()
cosine_distance_per_row = list()
# loop
for (y in 1:length(local_matrix_list)){
  for(u in 1:length(local_matrix_list)){
    euclidean_distance_per_row[[u]] = sqrt(sum((five_largest_eigenvalue[[y]]-five_largest_eigenvalue[[u]])^2))
    cosine_distance_per_row[[u]] = dot(five_largest_eigenvalue[[y]],five_largest_eigenvalue[[u]])/sqrt(sum(five_largest_eigenvalue[[y]]^2))/sqrt(sum(five_largest_eigenvalue[[u]]^2)) 
  }
  euclidean_distance[[y]] <- euclidean_distance_per_row
  cosine_distance[[y]] <- cosine_distance_per_row
}
ed_temp <- data.frame(matrix(unlist(euclidean_distance), nrow = length(euclidean_distance), byrow = T), stringsAsFactors = F)
euclidean_distance_output <- as.matrix(ed_temp)
cd_temp <- data.frame(matrix(unlist(cosine_distance), nrow = length(cosine_distance), byrow = T), stringsAsFactors = F)
cosine_distance_output <- as.matrix(cd_temp)
starting = ymd('2000-06-05')
tt = c(starting)
for (i in 2:length(euclidean_distance)){
  starting = starting %m+% months(1)
  tt[i] = starting
}
tt = as.character(tt)
# visualize corr matrix diff 
colnames(euclidean_distance_output) <- tt
rownames(euclidean_distance_output) <- tt
colnames(cosine_distance_output) <- tt
rownames(cosine_distance_output) <- tt
# heatmap in axis order
# for Corr Matrices Differences
library(gplots)
my_palette <- colorRampPalette(c("white","orange"))
heatmap.2(euclidean_distance_output,
          main = "5 Largest Eigenvalues Change Using Euclidean Distance", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map                                  
          col=my_palette, 
          Rowv=FALSE,
          Colv=FALSE,
          dendrogram="none") 
heatmap.2(cosine_distance_output,
          main = "5 Largest Eigenvalues Change Using Cosine Distance", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map                                  
          col=my_palette, 
          Rowv=FALSE,
          Colv=FALSE,
          dendrogram="none") 
# cluster
#euclidean distance
silhouette_width = sapply(2:10,
                          FUN = function(x) pam(x = euclidean_distance_output,k = x,diss=TRUE)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
# euclidean distance @ half year - optimal: 2
pam.euclideandishalf2 <- pam(euclidean_distance_output,2,diss=TRUE)
# euclidean distance @ one and half year - optimal: 2,4
pam.euclideandisonehalf4 <- pam(euclidean_distance_output,4,diss=TRUE)
# euclidean distance @ 2yrs - optimal: 2&5
pam.euclideandis2 <- pam(euclidean_distance_output,2,diss=TRUE)
pam.euclideandis5 <- pam(euclidean_distance_output,5,diss=TRUE)
euclideandis2 = list()
euclideandishalf2 = list()
for (i in 1:2){
  euclideandishalf2[[i]] = pam.euclideandishalf2$clustering[pam.euclideandishalf2$clustering == i]
}
euclideandisonehalf4 = list()
for (i in 1:4){
  euclideandisonehalf4[[i]] = pam.euclideandisonehalf4$clustering[pam.euclideandisonehalf4$clustering == i]
}
euclideandis5 = list()
for (i in 1:5){
  euclideandis5[[i]] = pam.euclideandis5$clustering[pam.euclideandis5$clustering == i]
}
# cosine distance
silhouette_width = sapply(2:10,
                          FUN = function(x) pam(x = cosine_distance_output,k = x,diss=TRUE)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
# cosine distance @ 2 years - optimal: 2
pam.cosinedistance2 <- pam(cosine_distance_output,2,diss=TRUE)
# cosine distance @ one year - optimal: 2
pam.cosinedistance3 <- pam(cosine_distance_output,3,diss=TRUE)
pam.cosinedistance4 <- pam(cosine_distance_output,4,diss=TRUE)
cosinedistance2 = list()
for (i in 1:2){
  cosinedistance2[[i]] = pam.cosinedistance2$clustering[pam.cosinedistance2$clustering == i]
}
cosinedistance3 = list()
for (i in 1:3){
  cosinedistance3[[i]] = pam.cosinedistance3$clustering[pam.cosinedistance3$clustering == i]
}
cosinedistance4 = list()
for (i in 1:4){
  cosinedistance4[[i]] = pam.cosinedistance4$clustering[pam.cosinedistance4$clustering == i]
}


# optimiziton2: apply another clustering method, based on the idea of high-dimensional data
# regarding each window as a variable, and each asset as a component of the variable
# loop this process
new_window_list = list()
# 20 year data
starting_date = whole_dataset$fecha[1]
ending_date = tail(whole_dataset$fecha,1)
# initial setup (the first window) for a two year length
window_start_date <- starting_date
window_end_date <- whole_dataset$fecha[1] %m+% months(6)
new_window_list[[1]] = c(window_start_date,window_end_date)
i = 2
while (window_end_date <= ending_date)
{
  window_start_date <- window_start_date %m+% months(1)
  window_end_date <- window_start_date %m+% months(6)
  new_window_list[[i]] <- c(window_start_date,window_end_date)
  i <- i+1
}
k = length(new_window_list)
new_window_list[[k]][2] <- ending_date
# construct new data
window_return1 <- whole_dataset %>% dplyr::filter(fecha>=new_window_list[[1]][1]&fecha<=new_window_list[[1]][2]) %>% select(-1) %>% colSums()
for (i in 2:length(new_window_list)){
  local_window_return = whole_dataset %>% dplyr::filter(fecha>=new_window_list[[i]][1]&fecha<=new_window_list[[i]][2]) %>% select(-1) %>% colSums()
  window_return1 = cbind(window_return1,local_window_return)
}
colnames(window_return1)
colnames(window_return1) = tt1
# clustering
window_return = t(window_return1)
#find the number of optimal clusters
# Total within sum of squares Plot 
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = window_return,centers = x,iter.max = 1000,nstart = 25)$tot.withinss})
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
# ratio plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = window_return,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
# optimal result
potential_choices = list()
# Silhouette Plot
silhouette_width = sapply(2:10,
                          FUN = function(x) pam(x = window_return,k = x,diss=TRUE)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
# half year 
km_half = kmeans(x = window_return,centers = 3,iter.max=10000,nstart=25)
k_segments_half = km_half$cluster
table(k_segments_half)
potential_choices[[4]] = k_segments_half
# 2 year
km = kmeans(x = window_return,centers = 4,iter.max=10000,nstart=25)
k_segments = km$cluster
table(k_segments)
potential_choices[[2]] = k_segments
km3 = kmeans(x = window_return,centers = 3,iter.max=10000,nstart=25)
k_segments3 = km3$cluster
table(k_segments3)
potential_choices[[1]] = k_segments3
km2 = kmeans(x = window_return,centers = 2,iter.max=10000,nstart=25)
k_segments2 = km2$cluster
table(k_segments2)
library(psych)
temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(window_return,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(window_return,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()
# Hierarchical Cluster
d = dist(x = window_return,method = 'euclidean')
clusters = hclust(d = d,method='ward.D2')
plot(clusters)
library(gridExtra)
library(factoextra)
grid.arrange(fviz_dend(x = clusters,k=2),
             fviz_dend(x = clusters,k=3),
             fviz_dend(x = clusters,k=4)
)
h_segments2 = cutree(tree = clusters,k=2)
table(h_segments2)
h_segments3 = cutree(tree = clusters,k=3)
table(h_segments3)
h_segments4 = cutree(tree = clusters,k=4)
table(h_segments4)
```

```{r}
# calculate the volatility of each market
# new method, half year, k-means, 3 clusters
halfyear_3_regime1 = list()
halfyear_3_regime2 = list()
halfyear_3_regime3 = list()
halfyear_3_regime2[[1]] = c(ymd('2000-06-05'), ymd('2001-03-04'))
halfyear_3_regime3[[1]] = c(ymd('2001-03-05'), ymd('2001-05-04'))
halfyear_3_regime2[[2]] = c(ymd('2001-05-05'), ymd('2001-10-04'))
halfyear_3_regime3[[2]] = c(ymd('2001-10-05'), ymd('2002-01-04'))
halfyear_3_regime1[[1]] = c(ymd('2002-01-05'), ymd('2002-03-04'))
halfyear_3_regime3[[3]] = c(ymd('2002-03-05'), ymd('2002-05-05'))
halfyear_3_regime2[[3]] = c(ymd('2002-05-05'), ymd('2002-11-04'))
halfyear_3_regime3[[4]] = c(ymd('2002-11-05'), ymd('2003-03-04'))
halfyear_3_regime1[[2]] = c(ymd('2003-03-05'), ymd('2004-03-04'))
halfyear_3_regime3[[5]] = c(ymd('2004-03-05'), ymd('2004-09-04'))
halfyear_3_regime1[[3]] = c(ymd('2004-09-05'), ymd('2005-03-04'))
halfyear_3_regime3[[6]] = c(ymd('2005-03-05'), ymd('2005-07-04'))
halfyear_3_regime1[[4]] = c(ymd('2005-07-05'), ymd('2006-04-04'))
halfyear_3_regime3[[7]] = c(ymd('2006-04-05'), ymd('2006-10-04'))
halfyear_3_regime1[[5]] = c(ymd('2006-10-05'), ymd('2007-11-04'))
halfyear_3_regime3[[8]] = c(ymd('2007-11-05'), ymd('2008-04-04'))
halfyear_3_regime2[[4]] = c(ymd('2008-04-05'), ymd('2009-02-04'))
halfyear_3_regime1[[6]] = c(ymd('2009-02-05'), ymd('2010-02-04'))
halfyear_3_regime3[[9]] = c(ymd('2010-02-05'), ymd('2010-08-04'))
halfyear_3_regime1[[7]] = c(ymd('2010-08-05'), ymd('2011-01-04'))
halfyear_3_regime3[[10]] = c(ymd('2011-01-05'), ymd('2011-06-04'))
halfyear_3_regime2[[5]] = c(ymd('2011-06-05'), ymd('2011-11-04'))
halfyear_3_regime3[[11]] = c(ymd('2011-11-05'), ymd('2012-01-04'))
halfyear_3_regime1[[8]] = c(ymd('2012-01-05'), ymd('2012-02-04'))
halfyear_3_regime3[[12]] = c(ymd('2012-02-05'), ymd('2012-10-04'))
halfyear_3_regime1[[9]] = c(ymd('2012-10-05'), ymd('2013-01-04'))
halfyear_3_regime3[[13]] = c(ymd('2013-01-05'), ymd('2014-06-04'))
halfyear_3_regime1[[10]] = c(ymd('2014-06-05'), ymd('2014-07-04'))
halfyear_3_regime3[[14]] = c(ymd('2014-07-05'), ymd('2015-06-04'))
halfyear_3_regime2[[6]] = c(ymd('2015-06-05'), ymd('2015-12-04'))
halfyear_3_regime3[[15]] = c(ymd('2015-12-05'), ymd('2016-05-04'))
halfyear_3_regime1[[11]] = c(ymd('2016-05-05'), ymd('2016-09-04'))
halfyear_3_regime3[[16]] = c(ymd('2016-09-05'), ymd('2017-03-04'))
halfyear_3_regime1[[12]] = c(ymd('2017-03-05'), ymd('2017-12-04'))
halfyear_3_regime3[[17]] = c(ymd('2017-12-05'), ymd('2020-01-04'))
halfyear_3_regime2[[7]] = c(ymd('2020-01-05'), ymd('2020-05-28'))
# bear
ly_bear = list()
ly_bear[[1]] = c(ymd('2000-06-05'), ymd('2002-07-24'))
ly_bear[[2]] = c(ymd('2007-10-18'), ymd('2009-03-16'))
ly_bear[[3]] = c(ymd('2020-02-21'), ymd('2020-05-21'))
regime1.data = list()
for (a in 1:length(ly_bear)){
  regime1.data.copy = factors_returns %>% dplyr::filter(fecha>=ly_bear[[a]][1] & fecha<=ly_bear[[a]][2]) %>% select(-1)
  regime1.data =  rbind(regime1.data,regime1.data.copy)
}
View(regime1.data)
# bull
regime2.data = list()
regime2.data.copy = factors_returns %>% dplyr::filter(fecha>='2002-07-24' & fecha<='2007-10-18') %>% select(-1)
regime2.data =  rbind(regime2.data,regime2.data.copy)
View(regime2.data)
# recovery
regime3.data = list()
regime3.data.copy = factors_returns %>% dplyr::filter(fecha>='2009-03-16' & fecha<='2020-02-21') %>% select(-1)
regime3.data =  rbind(regime3.data,regime3.data.copy)
View(regime3.data)
volatility_list1 = list()
for(i in 1:ncol(regime1.data)){
  volatility_list1[[i]] = sqrt(var(regime1.data[,i]))
}
mean(unlist(volatility_list1)) #0.0101847 
volatility_list2 = list()
for(i in 1:ncol(regime2.data)){
  volatility_list2[[i]] = sqrt(var(regime2.data[,i]))
}
mean(unlist(volatility_list2)) #0.01741643
volatility_list3 = list()
for(i in 1:ncol(regime3.data)){
  volatility_list3[[i]] = sqrt(var(regime3.data[,i]))
}
mean(unlist(volatility_list3)) #0.01045842