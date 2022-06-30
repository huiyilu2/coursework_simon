rm(list=ls())

load('C:/Users/odyss/Downloads/GBA424 - Toy Horse Case Data.Rdata')


# A:Using Regression to estimate the conjoint model at the individual level

# To convert data frame in conjoint Data into numeric
desmat = data.matrix(conjointData[4:7])
atts = c("Low Price","Tall Size","Rocking","Glamour")
colnames(desmat) = atts
head(desmat)

sampleSize = 200
ratings = conjointData$ratings
ID= conjointData$ID
desmatf = cbind(rep(1,nrow(desmat)),desmat);
partworths = matrix(nrow=sampleSize,ncol=ncol(desmatf))
for(i in 1:sampleSize){ 
    partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = c("Intercept",atts)


partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings)
head(finalratings)


# B:Here, Conducting Benefit Segmentation via Cluster Analysis of Conjoint Part-Utilities

install.packages("cluster")
library(cluster) #methods for cluster analysis in R
install.packages("fpc")
library(fpc) #abbreviation for flexible procedures for clustering
install.packages("factoextra")
library(factoextra) #helps to xtract and visualize output of exploatory multivariate data analysis
install.packages("gridExtra")
library(gridExtra) 
library(data.table) #for data manipulation

##To Evaluate number of clusters to use on data with visualizations

clustTest <- function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
    if(scale){ toClust <- scale(toClust);} #scale each element by susbtracting the mean and dividing by standard deviation for columns of a numeric matrix
    set.seed(seed);   #setting a random number of seed before cluster analysis
    wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var)) #to calculate first wss
    for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
    gpw <- fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
    pm1 <- pamk(toClust,scaling=TRUE)
    gps <- fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
    if(print){
        grid.arrange(gpw,gps, nrow = 1)
    }
    list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}

# test how many clusters we need to use
set.seed(123456)
toClust = partworths[,2:5]
tmp = clustTest(toClust)

runClusts <- function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
    if(length(nClusts)>4){
        warning #(¡±Using only first 4 elements of n Clusts¡±)
    }
    kms=list(); ps=list();
    for(i in 1:length(nClusts)){
        kms[[i]] <- kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
        ps[[i]] <- fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
        
    }
    library(gridExtra)
    if(print){
        tmp <- marrangeGrob(ps, nrow = 2,ncol=2)
        print(tmp)
    }
    list(kms=kms,ps=ps)
}

clusts = runClusts(partworths,2:3)

#To plot Kmeans cluster as three plot eport
plotClust <- function(km,toClust,
                      discPlot=FALSE,standardize=TRUE,margins = c(7,4,4,2)){
    nc <- length(km$size) #number of clusters
    
    percsize <- paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="") #generating the lables for graph
    pie(km$size,labels=percsize,col=1:nc) #col indicates the color palette
    
    gg <- fviz_cluster(km, geom = "point", data = toClust) + ggtitle(paste("k =",nc))
    print(gg)
    
    if(discPlot){
        plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
    }
    if(standardize){
        kmc <- (km$centers-rep(colMeans(toClust),each=nc))/rep(apply(toClust,2,sd),each=nc)
        rng <- range(kmc)
        dist <- rng[2]-rng[1]
        locs <- kmc+.05*dist*ifelse(kmc>0,1,-1)
        par(mar=margins)
        bm <- barplot(kmc,col=1:nc,beside=TRUE,las=2,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
        text(bm,locs,formatC(kmc,format="f",digits=1))
    } else {
        rng <- range(km$centers)
        dist <- rng[2]-rng[1]
        locs <- km$centers+.05*dist*ifelse(km$centers>0,1,-1)
        bm <- barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
        text(bm,locs,formatC(km$centers,format="f",digits=1))
    }
    vs <- data.table(Segment = 1:nrow(km$centers),km$centers,Size = km$size/sum(km$size))
    vs[order(-Size),]
}


plotClust(clusts$kms[[1]],toClust)
plotClust(clusts$kms[[2]],toClust)


# C: To Conduct a priori segmentation
ageD = respondentData$age
ageD = rep(ageD,each=16)
genderD = rep(respondentData$gender,each=16)

#First, consider the aggregate regression
summary(lm(ratings~desmat))

#run the regression with interactions for segment dummies
summary(lm(ratings~desmat*ageD))

#We are doing the same for gender dummy
summary(lm(ratings~desmat*genderD))

#Since age is significant, we subset our data for two age groups:
summary(lm(ratings~desmat,subset=ageD==1)) #older kids
summary(lm(ratings~desmat,subset=ageD==0)) #younger kids


###################################################11111111111111
ratingData <- as.data.frame(matrix(nrow = 200, ncol = 16))
colnames(ratingData) <- c(1:16)
for(a in 1:200){
    ratingData[a, ] <- conjointData$ratings[conjointData$ID == a]
}
ratingData[is.na(ratingData)] <- 0


###set up scenarios###
scens = list()
# EarlyRiders maintain its two current products.
scens[[1]]= c(7,5,13)
scens[[2]]= c(7,4,13)
# EarlyRiders change one of its current products to bouncing motion.
scens[[3]]= c(7,5,16)
scens[[4]]= c(7,4,16)
scens[[5]]= c(8,5,13)
scens[[6]]= c(8,4,13)
scens[[7]]= c(8,5,16)
scens[[8]]= c(8,4,16)



simFCShares <- function(scens,data){ 
    inmkt <- data[scens,]
    bestOpts <- apply(inmkt, 2, max)
    decs <- (inmkt>rep(bestOpts,each=length(scens))-.00000000000001)
    shs <- decs/rep(colSums(decs),each=length(scens))
    shs
    rowMeans(shs)
}

###test market shares###
sim1<- simFCShares(scens[[1]], ratingData)
sim2<- simFCShares(scens[[2]], ratingData)
sim3<- simFCShares(scens[[3]], ratingData)
sim4<- simFCShares(scens[[4]], ratingData)
sim5<- simFCShares(scens[[5]], ratingData)
sim6<- simFCShares(scens[[6]], ratingData)
sim7<- simFCShares(scens[[7]], ratingData)
sim8<- simFCShares(scens[[8]], ratingData)


simProfit = function(data,scen, myProds, prices, vcosts,fcosts,mktsize=1){
    mktshr = simFCShares(scen,data);
    vprofit = mktshr * (prices-vcosts)*mktsize;
    sum(vprofit[myProds])-fcosts
}


simProf1 = simProfit(ratingData,scens[[1]],c(2,3),c(111.99,111.99,111.99),c(41,33,33),40000,4000)
simProf2 = simProfit(ratingData,scens[[2]],c(2,3),c(111.99,95.99,111.99),c(41,46,33),47000,4000)
simProf3 = simProfit(ratingData,scens[[3]],c(2,3),c(111.99,111.99,95.99),c(41,33,41),47000,4000)
simProf4 = simProfit(ratingData,scens[[4]],c(2,3),c(111.99,95.99,95.99),c(41,46,41),54000,4000)
simProf5 = simProfit(ratingData,scens[[5]],c(2,3),c(95.99,111.99,111.99),c(41,33,33),40000,4000)
simProf6 = simProfit(ratingData,scens[[6]],c(2,3),c(95.99,95.99,111.99),c(41,46,33),47000,4000)
simProf7 = simProfit(ratingData,scens[[7]],c(2,3),c(95.99,119.99,95.99),c(41,33,41),47000,4000)
simProf8 = simProfit(ratingData,scens[[8]],c(2,3),c(95.99,95.99,95.99),c(41,46,41),54000,4000)


simcom1 = simProfit(ratingData,scens[[1]],c(1),c(111.99),c(41),20000,4000)
simcom2 = simProfit(ratingData,scens[[2]],c(1),c(111.99),c(41),20000,4000)
simcom3 = simProfit(ratingData,scens[[3]],c(1),c(111.99),c(41),20000,4000)
simcom4 = simProfit(ratingData,scens[[4]],c(1),c(111.99),c(41),20000,4000)
simcom5 = simProfit(ratingData,scens[[5]],c(1),c(95.99),c(41),27000,4000)
simcom6 = simProfit(ratingData,scens[[6]],c(1),c(95.99),c(41),27000,4000)
simcom7 = simProfit(ratingData,scens[[7]],c(1),c(95.99),c(41),27000,4000)
simcom8 = simProfit(ratingData,scens[[8]],c(1),c(95.99),c(41),27000,4000)

