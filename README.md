readLines("orange_small_train.data",n=10)

data <- read.delim("orange_small_train.data", sep="\t") 

###  Summary ####
###  http://www.statmethods.net/stats/descriptives.html
SalesJan2009 <- read.csv("~/Data/SalesJan2009.csv")
install.packages("Hmisc")
library(Hmisc)
describe(SalesJan2009)

#### Format time ####
a<-SalesJan2009$Transaction_date[3]
strptime(a, "%d/%m/%y %H:%M")
SalesJan2009$Transaction_date_New<-strptime(SalesJan2009$Transaction_date, "%d/%m/%y %H:%M")

#### Aggragation ####
library(doBy)
summaryBy(Price ~ Payment_Type + Product, data=SalesJan2009, FUN=c(length,mean,sd))

length2 <- function (x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}
summaryBy(Price ~ Payment_Type + Product, data=SalesJan2009, FUN=c(length2,mean,sd))


#### SQL ####
library(sqldf)

## Multiple Linear Regression Example 
## http://www.statmethods.net/stats/regression.html
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results
# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics


# Logistic Regression
## http://www.statmethods.net/advstats/glm.html
# where F is a binary factor and 
# x1-x3 are continuous predictors 
fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())
summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals


#################################################################
#                                                               #
#                02 Principal Component Analysis                #
#                                                               #
#################################################################

#Load the data
data <- read.csv(file="kmeans_data.csv", header=T, sep=",", row.names=1)

# Scaling data
data2 <- data.frame(scale(data))

# Verify variance is uniform
plot(sapply(data2, var))

# Proceed with principal components
pc <- princomp(data2)
plot(pc) #Variances plot for PCs 
screeplot(pc, type="lines") #Variances plot for PCs
summary(pc) # 7 components is both 'elbow' and explains >70% variance


#################################################################
#                                                               #
#                03 Clustering on loadings of PCs               #
#                                                               #
#################################################################

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(data2)
pc$rotation[,1] #loadings for the first principal component 
pc$rotation[,2] #loadings for the second principal component 

# Get loadings from all 15 principal components
comp <- data.frame(pc$x[,1:15])


# Determine optimal number of clusters 
library(clValid)

#Internal measures (Connectivity, Silhouette Width and Dunn Index) 
intern <- clValid(comp, 3:8, clMethods=c("kmeans"),
                  validation="internal")
summary(intern)
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F,
     xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#stability measures (Average Proportion of Non-overlap, Average Distance and Average Distance between Means)
stab <- clValid(comp, 3:8, clMethods=c("kmeans"),
                validation="stability")
optimalScores(stab)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(stab, measure=c("APN","AD","ADM"),legend=FALSE)
plot(nClusters(stab),measures(stab,"APN")[,,1],type="n",axes=F,
     xlab="",ylab="")
legend("center", clusterMethods(stab), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#Rank Aggregation to determine the overall winner
result <- clValid(comp, 3:8, clMethods=c("kmeans"),
                  validation=c("internal","stability"))
res <- getRanksWeights(result)

if(require("RankAggreg")) {
  CEWS <- RankAggreg(x=res$ranks, k=5, weights=res$weights, seed=123, verbose=FALSE)
  CEWS}

# Apply k-means with k=4
k <- kmeans(comp, 4, nstart=100, iter.max=1000)
library(ggplot2)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))

# Check cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))

#################################################################
#                                                               #
#                03 Clustering on loadings of PCs               #
#                                                               #
#################################################################

#Plot clusters on PC space
windows()
one.df<-cbind(comp,k$cluster)
colnames(one.df)[ncol(one.df)] <- "Cluster"
dd.col <- rainbow(length(unique(one.df$Cluster)))
one.df$Cluster<-as.factor(one.df$Cluster)
p1<- ggplot(aes(x = one.df$PC2, y =  one.df$PC1,col=Cluster), 
            data = one.df)
p2<- p1 + geom_point(alpha = 1/2,size=4) +
  scale_color_manual(values = dd.col) +
  theme(legend.position=c(1,1),legend.justification=c(1,1)) +
  scale_size_area() + 
  labs(
    x = "Principle Component 2",
    y = "Principle Component 1") 
#marginal density of x - plot on top
plot_top <- ggplot(one.df, aes(one.df$PC2, col=Cluster,fill=Cluster)) + 
  geom_density(alpha=.4) + 
  scale_fill_manual(values = dd.col) + 
  theme(legend.position = "none") +
  labs(x = "") 
#marginal density of y - plot on the right
plot_right <- ggplot(one.df, aes(one.df$PC1, col=Cluster,fill=Cluster)) + 
  geom_density(alpha=.4) + 
  scale_fill_manual(values = dd.col) + 
  coord_flip() + 
  theme(legend.position = "none")+
  labs(x = "") 
#arrange the plots together, with appropriate height and width for each row and column
library(gridExtra)
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
grid.arrange(plot_top, empty, p2, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))


#BoxPlot the clusters' stats
library(cluster)
library(fpc)
library(ggplot2)
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
box.only <- function(x) {
  r <- quantile(x, probs = c(0.25, 0.25, 0.5, 0.75, 0.75))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
min.max <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1.00))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
library(reshape)
one.df<-cbind(data,k$cluster)
colnames(one.df)[ncol(one.df)] <- "Cluster"
one.df$Cluster<-as.factor(one.df$Cluster)
myvars <- c("Cluster")
melted.df<-melt(one.df,id.vars=myvars)

#Export the mean value of all features with cluster id
c<-aggregate(one.df[,1:15], by=list(one.df$Cluster), FUN=mean)
write.csv(c,"mean.csv")

a<-subset(melted.df,variable %in% unique(melted.df$variable)[1:5])
b<-subset(melted.df,variable %in% unique(melted.df$variable)[6:10])
c<-subset(melted.df,variable %in% unique(melted.df$variable)[11:15])

windows()
p2<- ggplot(aes(y = value, x =  Cluster,color=Cluster), 
            data = a) + facet_grid(variable ~ .,scales="free")
p2 + stat_summary(fun.data = box.only, geom = "boxplot") + 
  geom_point(alpha = 1/5,size=4) + 
  theme(legend.position = "none") +
  scale_fill_manual("Cluster",values=dd.col)

windows()
p3<- ggplot(aes(y = value, x =  Cluster,color=Cluster), 
            data = b) + facet_grid(variable ~ .,scales="free")
p3 + stat_summary(fun.data = box.only, geom = "boxplot") + 
  geom_point(alpha = 1/5,size=4)+ 
  theme(legend.position = "none") +
  scale_fill_manual("Cluster",values=dd.col)

windows()
p4<- ggplot(aes(y = value, x =  Cluster,color=Cluster), 
            data = c) + facet_grid(variable ~ .,scales="free")
p4 + stat_summary(fun.data = box.only, geom = "boxplot") + 
  geom_point(alpha = 1/5,size=4)+ 
  theme(legend.position = "bottom") +
  scale_fill_manual("Cluster",values=dd.col)
#Save the data with Clusters
sqlSave(mssql, one.df,tablename="dbo.ZZZ_OUTPUT_ROUTE_CLUSTERS")


