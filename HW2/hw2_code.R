library(ggplot2)
library(stargazer)
library(reshape2)
library(arules)
library(fpc)


setwd("/Users/mymacbook/Desktop/toti/hws/hw2/")

crime <- read.csv("CrimeStats2016.csv", stringsAsFactors = F)
#sapply(crime, class)
str(crime)
crime$State <- as.factor(crime$State)
summary(crime)
stargazer(crime, summary.stat = c("mean","median","min","max"))

#ploting the violant crime rate in a decreasing order
crime2 <- crime[order(crime$Violent.crime.Rate, decreasing = T),]
crime2$State <- factor(crime2$State, levels = crime2$State[order(crime2$Violent.crime.Rate, decreasing = T)])

ggplot(crime2, aes(x=State, y=Violent.crime.Rate)) +
  geom_bar(stat="identity", fill="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("violent_crime_rate.png")


#ploting the property crime rate in a decreasing order
crime3 <- crime[order(crime$Property.crime.Rate, decreasing = T),]
crime3$State <- factor(crime3$State, levels = crime3$State[order(crime3$Property.crime.Rate, decreasing = T)])

ggplot(crime3, aes(x=State, y=Property.crime.Rate)) +
  geom_bar(stat="identity", fill="gray") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("property_crime_rate.png")


#ploting the crime rates distribution next to each other
par(mar=c(7,5,1,1), cex.axis=0.5, srt=45, xpd = TRUE)
boxplot(crime[,c(6, 8, 10, 12, 16, 18, 20)], col = "steelblue", las=2)

#----------------------------Question 2-------------------------------
#lets compute the distance matrix with euclidean method
vars.to.use <- colnames(crime)[c(6, 8, 10, 12, 16, 18, 20)] # variables used
crime_matrix <- scale(crime[,vars.to.use]) #scaling the data
d_matrix <- dist(crime_matrix, method="euclidean") #distance matrix


#----------------------------Question 3-------------------------------
pfit <- hclust(d_matrix, method="ward.D")
plot(pfit, labels=crime$State)
rect.hclust(pfit, k=8)


#----------------------------Question 4-------------------------------
#
#----------total within sum of square-------------
sqr_edist <- function(x, y) {
  sum((x-y)^2)
}


#within sum of squares per cluster
wss.cluster <- function(clustermat) {
  c0 <- apply(clustermat, 2, FUN=mean) #2 means by column, c0 is the centroid of the cluster
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)})) #1 means by row
}

#total within sum of square WSS
wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}

#===============Calinski-Harabasz index==============================
#TSS function
totss <- function(dmatrix) {
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

#A function to calculate the CH index for a number of clusters from 1 to kmax.
ch_criterion <- function(dmatrix, kmax, method="kmeans") {
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1] # number of rows.
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))
  for(k in 2:kmax) {
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else { # hclust
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  
  bss <- totss - wss
  crit.num <- bss/(0:(kmax-1))
  crit.denom <- wss/(npts - 1:kmax)
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)
}

#Calculate both criteria for 1–10 clusters.
clustcrit <- ch_criterion(crime_matrix, 10, method="hclust")
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),
                        wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)
ggsave("ch_index.png")

#----------------------------Question 5-------------------------------
#Use clusterboot() fnc to evaluate the clustering, i.e how stable the clusters are
kbest.p<-8

cboot.hclust <- clusterboot(crime_matrix,clustermethod=hclustCBI,method="ward.D", k=kbest.p, seed=15555) #using hierarchical clustering
summary(cboot.hclust$result) #the results of the clustering

print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(crime[labels==i,c("State","Murder.and..nonnegligent..manslaughter.Rate",
                            "Rape.Rate","Robbery.Rate","Aggravated.assault.Rate", "Burglary.Rate",
                            "Larceny.theft.Rate", "Motor.vehicle.theft.Rate")])
  }
}

groups2<-cboot.hclust$result$partition # returns a vector of cluster labels.
print_clusters(groups2, kbest.p) #The clusters are the same as those produced by a direct call to hclust().

#printing clusters in an outside source
sink("Hierachical_Clusters_Output.txt")
print(print_clusters(groups2, kbest.p))
sink()

cboot.hclust$bootmean #The vector of cluster stabilities.
cboot.hclust$bootbrd #The count of how many times each cluster was dissolved. By default clusterboot() runs 100 bootstrap iterations



#----------------------------Question 6-------------------------------
groups3 <- cutree(pfit, k=3)
print_clusters(groups3, 3)

princ <- prcomp(crime_matrix)
nComp <- 2
#The predict() function will rotate the data into the space described by the principal components. We only want the projection on the first two axes.
project <- predict(princ, newdata=crime_matrix)[,1:nComp] 

#Create a data frame with the transformed data, along with the cluster label and country label of each point.
project.plus <- cbind(as.data.frame(project), 
                      cluster=as.factor(groups3),
                      State=crime$State)
#plotting it
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(color=cluster)) +
  geom_text(aes(label=State),
            hjust=0, vjust=1, size=2)
ggsave("princ_comp.png")



#----------------------------Question 7-------------------------------
#exclude District of Columbia and Puerto Rico for k means since they are outliers
crime_matrix2 <- crime_matrix[-c(9,52),]
kbest.p=3
crime_clust <- kmeans(crime_matrix2, kbest.p, nstart=100, iter.max=100)
summary(crime_clust) #returns all the sum of squares measures.

crime_clust$centers  #may appear in different order, rows are the centroids of the clusters
crime_clust$size #returns the number of points in each cluster
groups_k_means <- crime_clust$cluster #its a group

crime_cleaned <- crime[-c(9,52),] #exclude DC and Puerto Rico

print_clusters2 <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(crime_cleaned[labels==i,c("State","Murder.and..nonnegligent..manslaughter.Rate",
                            "Rape.Rate","Robbery.Rate","Aggravated.assault.Rate", "Burglary.Rate",
                            "Larceny.theft.Rate", "Motor.vehicle.theft.Rate")])
  }
}

print_clusters2(groups_k_means, kbest.p)

#printing clusters in an outside source
sink("Kmeans_Clusters_Output.txt")
print(print_clusters2(groups_k_means, kbest.p))
sink()

#----------------------------Question 8-------------------------------
#use kmeansruns() fnc for picking optimak k
clustering.ch <- kmeansruns(crime_matrix2, krange=1:10, criterion="ch") 
clustering.ch$bestk
summary(clustering.ch)

clustering.asw <- kmeansruns(crime_matrix2, krange=1:10, criterion="asw")
clustering.asw$bestk

critframe <- data.frame(k=1:10, ch=scale(clustering.ch$crit),
                        asw=scale(clustering.asw$crit))

library(reshape2)
critframe <- melt(critframe, id.vars=c("k"), #use melt comand to trasfer the data from wide to long
                  variable.name="measure",
                  value.name="score")

ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)

ggsave("kmeansrun_cluster.png")


#----------------------------Question 9-------------------------------
#create new variables for the levels of each crime rate 
#use equal widh of bins instead of equal frequency binning
crime$Murder.and..nonnegligent..manslaughter.level <- cut(crime$Murder.and..nonnegligent..manslaughter.Rate,
                                                          breaks = c(0, 2.775, 5.250, 6.775, 20.4),
                                                          labels=c("Low", "Medium", "High", "Very High"),
                                                          include.lowest=T)

ggplot(crime, aes(x=Murder.and..nonnegligent..manslaughter.Rate)) + geom_density()
ggsave("murder_rate.png")

table(crime$Murder.and..nonnegligent..manslaughter.level)

crime$Rape.Rate.level <- cut(crime$Rape.Rate,
                             breaks = quantile(crime$Rape.Rate),
                             labels=c("Low", "Medium", "High", "Very High"),
                             include.lowest=T)
table(crime$Rape.Rate.level)

crime$Robbery.Rate.level <- cut(crime$Robbery.Rate,
                             breaks = c(0, 54.48, 80.40, 111.12, 510.90),
                             labels=c("Low", "Medium", "High", "Very High"),
                             include.lowest=T)

table(crime$Robbery.Rate.level)

crime$Aggravated.assault.Rate.level <- cut(crime$Aggravated.assault.Rate,
                                           breaks = quantile(crime$Aggravated.assault.Rate),
                                           labels=c("Low", "Medium", "High", "Very High"),
                                           include.lowest=T)
table(crime$Aggravated.assault.Rate.level)

crime$Burglary.Rate.level <- cut(crime$Burglary.Rate,
                                 breaks = quantile(crime$Burglary.Rate),
                                 labels=c("Low", "Medium", "High", "Very High"),
                                 include.lowest=T)
table(crime$Burglary.Rate.level)

crime$Larceny.theft.Rate.level <- cut(crime$Larceny.theft.Rate,
                                 breaks = quantile(crime$Larceny.theft.Rate),
                                 labels=c("Low", "Medium", "High", "Very High"),
                                 include.lowest=T)
table(crime$Larceny.theft.Rate.level)


crime$Motor.vehicle.theft.Rate.level <- cut(crime$Motor.vehicle.theft.Rate,
                                            breaks = quantile(crime$Motor.vehicle.theft.Rate),
                                            labels=c("Low", "Medium", "High", "Very High"),
                                           include.lowest=T)
table(crime$Motor.vehicle.theft.Rate.level)


#----------------------------Question 10-------------------------------
crime_rules <- crime[,c(21:27)]

#support 5%, confidence 75%
rules_1 <- apriori(crime_rules,
                 parameter =list(support = 0.05, confidence=0.75))
summary(rules_1)

#support 5%, confidence 90%
rules_2 <- apriori(crime_rules,
                   parameter =list(support = 0.05, confidence=0.9))
summary(rules_2)


#support 10%, confidence 75%
rules_3 <- apriori(crime_rules,
                   parameter =list(support = 0.1, confidence=0.75))
summary(rules_3)

#support 10%, confidence 90%
rules_4 <- apriori(crime_rules,
                   parameter =list(support = 0.1, confidence=0.9))
summary(rules_4)

#support 20%, confidence 75%
rules_5 <- apriori(crime_rules,
                   parameter =list(support = 0.2, confidence=0.75))
summary(rules_5)

#----------------------------Question 11-------------------------------

inspect(head((sort(rules_3, by="confidence")), n=5))


#----------------------------Question 12-------------------------------
murder_rules <- apriori(crime_rules,
                  parameter =list(support = 0.1,
                                  confidence=0.75),
                  appearance=list(rhs=c("Murder.and..nonnegligent..manslaughter.level=Very High"),
                                  default="lhs"))
summary(murder_rules)

murder_rulesConf <- sort(murder_rules, by="confidence")
inspect(head(lhs(murder_rulesConf), n=5))









