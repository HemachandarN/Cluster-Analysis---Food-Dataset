## Read in Data


## Why would I want to cluster food data?
##  One reason may be that I am a health eater and I want to eat a meal
##   that is balanced in calories, high in protein, etc. each day but
##   I want to eat different meals each day. So, I can pick one item
##    in each food cluster.

## Other reasons also exist.
data <- read.csv("imputed_masst.csv")
data

rownames(data)=data[,1]
data=data[,-1]
data=scale(data)  ## should i standardize?  calories, protein, fat, calcium, iron
data

## Explore the data
summary(data)
data <- as.data.frame(data)
rho=cor(data)
rho
data$V48 <- NULL 

require(graphics)

## Single Linkage

## Step 1 - Compute a distance matrix
D=dist(data)

# Step 2 - Run hclust()
hc1<-hclust(D,method = "single")
summary(hc1)

hc2 <- hclust(D, method = "complete")

# Step 3 - Plot dendrogram
plot(hc1)
plot(hc2)

# here is the interpretation part, you can see that foods which are next to each other are most
#    similar.  there is "no cluster" so to speak, but if you look at canned chicken, that is really
#      close to canned tuna (they are very similar!), so you could replace one with the other

## note that this only works with a limited number of foods (not 5000 foods)

# Step 4 - choose number of clusters
memb<-cutree(hc1,k=4)
memb # for single 

memb1<-cutree(hc2, k = 4)
memb1 # for complete 
# for data camp work
class(memb1)
memb1_d <- as.data.frame(memb1)
#generate the segmented dataframe 
library(dplyr)
data1 <-as.data.frame(data) 
segment_food <- mutate(data1, cluster = memb1) 
count(segment_food, cluster)
# colour dendrogram 
library(dendextend)
dend_food <-as.dendrogram(hc2)
dend_colored <- color_branches(dend_food, k=4)
plot(dend_colored)

# calculate the mean for each category 
segment_food %>%
  group_by(cluster)%>%
  summarise_all( list(mean))

# Step 5 - get clustser centers
cent<-NULL
for (k in 1:4){cent<-rbind(cent,colMeans(data[memb1==k,,drop=FALSE]))}
cent  # same as Table 7.21

## Step 6 - Calculate sum of total SS . within SS for each cluster (compare to k-means below)

one=sum((data[memb1==1,,drop=FALSE]-cent[1,])^2)
two=sum((data[memb1==2,,drop=FALSE]-cent[2,])^2)
three=sum((data[memb1==3,,drop=FALSE]-cent[3,])^2)
four=sum((data[memb1==4,,drop=FALSE]-cent[4,])^2)
  
tss_single=one+two+three+four  ## total sum of squares from cluster centroid (mean in this case)
# remember, single linkage has no guarantee for fit, this could be bad.

## kmeans clustering
library(flexclust)

# Step 1 - Run kmeans and analyze clusters
cl=kmeans(data, centers = 5 , nstart = 20)  ## let's keep same number of clusters as before
cl
cl$tot.withinss
names(cl)
c_c<-cl$centers
write.csv(c_c,"cluste.center.csv")

# mutation 
cl$cluster
data2 <-as.data.frame(data)
segment_k <- mutate(data2, cluster = cl$cluster)


# using GGplot
library(ggplot2)
ggplot(segment_k, aes(x= V1, y = V2, color = factor(cluster)))+
  geom_point()

## notice the within sum of squares...should be lower than single linkage  (George Foreman GUARANTEE)

# Step 2 - Plot clusters

plot(data, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)  ## this only does calories and protein
text(data,rownames(data),col=cl$cluster)



# Step 3 - Choose k  (plot total sum of squares)

# my method 
model <- kmeans(x= data , centers = 4)
model$tot.withinss

library(purrr)

tot_withinss <- map_dbl(1:51,  function(k){
  model <- kmeans(x = data, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:51,
  tot_withinss = tot_withinss
)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:51)

ggplot(data=elbow_df, aes(x=k, y=tot_withinss, group=1)) +
  geom_line(color="red")+
  geom_path()+
  scale_x_continuous(breaks = 1:51)
  geom_point(data=elbow_df, aes_string(tot_withinss)
             
        
             
names(data)

tss<-rep(0,51)
for (k in 1:51){tss[k]=kmeans(data,k)$tot.withinss}
plot(1:51,tss)
lines(stats::lowess(1:51))

## you want to see where it no longer decreases much (maybe 4 or 5 clusters), notice single linkage
## fits worse (higher tss) than all except 1 cluster.  it even does worse than 2!

# Step 4 - Interpret clusters

cl$centers

## here, you want to look at the centers...notice cluster 2 has really "high calorie food, high fat, low calclium"
## if we wanna get strong/lean, we want to pick from cluster 1, "high protein, low calorie"
#note that we don't know what the foods are in each cluster yet until we plot them or list them.
cl$cluster

## now we see that cluster 2 has things like hamburger and braised beef
#   cluster 3 has things like broiled chicken (healthy), canned salmon, baked bluefish (yum)

# Step 5  - Plot clusters with pca (this makes your intreptation even better)

## here we are putting food names to the clusters...it helps people understand.
library(pca3d)
?pca3d
library(factoextra)
pca <- prcomp(data, scale.= TRUE )
pca$x
names(pca)
pca_1<- as.data.frame(pca$rotation)
plot(pca_1$PC1, pca_1$PC2)
pc_a<-as.data.frame(pca$x)
plot(pc_a$PC1, pc_a$PC2)

p<-ggplot(pca_1,aes(x=PC1,y=PC2 ))
p<-p+geom_point()
p


fviz_eig(pca)

fviz_pca_var(pca,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)    
fviz_pca_biplot(pca, repel = TRUE,col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), col.ind = "#696969")

pca2d( pca, group=cl$cluster )
pca2d( pca, group=cl$cluster,show.labels=TRUE )
pca3d( pca, group=cl$cluster,show.labels=TRUE )

## make your statements in step 4 combine with step 5
## you may also want to do PCA to rename the axes. you should know how to do this!
# congratulations - very few people can explain PCA and clustering together. this will pay 
#         you dividends down the road in analytics.  Remember to B>0 (be positive!)

