shot<-read.csv('shot_logs.csv')
library(magrittr)
library(ggplot2)
library(dplyr)
shot$SHOT_CLOCK[is.na(shot$SHOT_CLOCK)] <- 0
summary(shot$SHOT_CLOCK)
shot$SHOT_CLOCK_NA=ifelse(is.na(shot$SHOT_CLOCK),1,0)
summary(shot$SHOT_CLOCK_NA)
ggplot(data = shot)+
  geom_histogram(aes(x=PTS,y = ..density..),bins=18)+
  facet_wrap(~SHOT_CLOCK_NA)+
  labs(title = 'histogram of points made',
       x = "Points")
ggplot(data = shot)+
  geom_histogram(aes(x=SHOT_DIST,y = ..density..),bins=18)+
  facet_wrap(~SHOT_CLOCK_NA)+
  labs(title = 'histogram of shot distance',
       x = "SHOT_DIST")

shot$SHOT_CLOCK_24=ifelse(shot$SHOT_CLOCK==24.00,"24 sec","not 24 sec")
summary(shot$SHOT_CLOCK_24)
length(which(shot$SHOT_CLOCK_24=="0 sec"))
summary(shot$SHOT_CLOCK)
shot$SHOT_CLOCK_24[is.na(shot$SHOT_CLOCK_24)] <- "0 sec" 
ggplot(data = shot)+
  geom_histogram(aes(x=PTS,y = ..density..),bins=18)+
  facet_wrap(~SHOT_CLOCK_24)+
  labs(title = 'histogram of points made',
       x = "Points")
ggplot(data = shot)+
  geom_histogram(aes(x=SHOT_DIST,y = ..density..),bins=18)+
  facet_wrap(~SHOT_CLOCK_24)+
  labs(title = 'histogram of shot distance',
       x = "SHOT_DIST")
summary(shot)
length(which(shot$TOUCH_TIME==0))
length(which(shot$TOUCH_TIME==-163.600))
shot<-subset(shot,shot$TOUCH_TIME>=0)
length(which(shot$TOUCH_TIME<0))
length(which(shot1$TOUCH_TIME<0))
#
summary(shot$LOCATION)
shot$LOCATION=ifelse(shot$LOCATION=="A",0,1)
View(shot)
library(lubridate)
shot$GAME_CLOCK=period_to_seconds(ms(shot$GAME_CLOCK))


##Pritisha's cleaning data part
#Checking wins and points related columns to remove remove redundancy

result_check1=unique(paste(shot$SHOT_RESULT,shot$FGM))
#SHOT_RESULT=FGM 
#--- we can drop SHOT_RESULT
#result_check1
# "made 1"   "missed 0"

result_check2=unique(paste(shot$PTS_TYPE,shot$PTS,shot$FGM))
#result_check2
#"2 2 1" "3 0 0" "2 0 0" "3 3 1"
#PTS=PTS_TYPE when FGM=1 ELSE 0 (i.e. PTS_TYPE*FGM) 
#---Consistent:Keep PTS for later use

#Outliers in defender distance

#Distribution of closest defender distance
hist(shot$CLOSE_DEF_DIST,
     main = "Distribution of Closest Defender Distance",
     xlab="Closest Defender Distance (ft)")
#majority within 10 feet --- how many records b/w 10-20ft and >20ft
sub_10=subset(shot,shot$CLOSE_DE4F_DIST>10)
#4236 records
sub_10to20=subset(subset(shot,shot$CLOSE_DEF_DIST>10),CLOSE_DEF_DIST<=20)
#4152 records
sub_20=subset(shot,shot$CLOSE_DEF_DIST>20)
#174 records

#Distribution of closest defender distance >10feet away (possible outliers)
hist(sub_10$CLOSE_DEF_DIST,
     main = "Distribution of Closest Defender Distance",
     xlab="Closest Defender Distance (ft)")
hist(sub_10to20$CLOSE_DEF_DIST,
     main = "Distribution of Closest Defender Distance",
     xlab="Closest Defender Distance (ft)")
hist(sub_20$CLOSE_DEF_DIST,
     main = "Distribution of Closest Defender Distance",
     xlab="Closest Defender Distance (ft)")
#most seem to be covered within 25 feet
sub_25=subset(shot,shot$CLOSE_DEF_DIST>25)
#64 records --- maybe use 25 as threshold if not 20

View(sub_25)

#Outliers based on sht distance & Creating additional Shot related columns
#1.Open vs. Guarded (1,0)
shot$open_shots=ifelse(shot$CLOSE_DEF_DIST>6, 1, 0)

#2.Pass vs. Dribble Shot (1,0)
shot$pass_shots=ifelse(shot$DRIBBLES==0, 1, 0)

#3.Checking point outliers based on shot distance

#What is the min distance for a 3pt: NBA=22--need to cross verify
shots_check=subset(shot,PTS==3)
shots_check1=subset(shots_check,SHOT_DIST<21)
summary(shots_check1)
hist(shots_check1$SHOT_DIST,
     main = "Distribution of Outlier 3 Pointers(<21ft) based on Shot Distance",
     xlab="Shot Distance (ft)")
#Even with a leeway of 1 foot there are 45 rows with misrecorded SHOT_DIST
#for 3 pointers --- small percentage of dataset,drop rows

#What is the min distance for a 2pt: NBA=25ft max--need to cross verify
shots_check2=subset(shot,PTS==2)
shots_check3=subset(shots_check,SHOT_DIST>27)
summary(shots_check3)
hist(shots_check3$SHOT_DIST,
     main = "Distribution of Outlier 2 Pointers(>27ft) based on Shot Distance",
     xlab="Shot Distance (ft)")

#Corner3 Shot Column (1)
shot$corner_shots=ifelse(shot$PTS==3, 
                         ifelse(shot$SHOT_DIST<=23, 1, 0), 0)
shots_chk=subset(shot,corner_shots==1)

###############################################################
#joining datasets
players <- read.csv('players.csv')

players$Pos <- ifelse(players$Pos=='PG', 1, ifelse(players$Pos=='SG', 2, ifelse(players$Pos=='SF', 3, ifelse(players$Pos=='PF', 4, 5))))

#only data for DEFENDER
defender <- select(players, Player_2, Pos, Age, Weight, Experience, Salary, Height_cm)
#change column names
colnames(defender)[colnames(defender)=="Pos"] <- "pos_def"
colnames(defender)[colnames(defender)=="Age"] <- "age_def"
colnames(defender)[colnames(defender)=="Weight"] <- "weight_def"
colnames(defender)[colnames(defender)=="Experience"] <- "experience_def"
colnames(defender)[colnames(defender)=="Salary"] <- "salary_def"
colnames(defender)[colnames(defender)=="Height_cm"] <- "height_def"


join1 <- left_join(shot, defender, by = c("CLOSEST_DEFENDER"="Player_2"))         	 

#only data for SHOT TAKER
shot_taker <- select(players, Player, Pos, Age, Weight, Experience, Salary, Height_cm, Country)
#change column names
colnames(shot_taker)[colnames(shot_taker)=="Pos"] <- "pos"
colnames(shot_taker)[colnames(shot_taker)=="Age"] <- "age"
colnames(shot_taker)[colnames(shot_taker)=="Weight"] <- "weight"
colnames(shot_taker)[colnames(shot_taker)=="Experience"] <- "experience"
colnames(shot_taker)[colnames(shot_taker)=="Salary"] <- "salary"
colnames(shot_taker)[colnames(shot_taker)=="Height_cm"] <- "height"
colnames(shot_taker)[colnames(shot_taker)=="Country"] <- "country"

join2 <- left_join(join1, shot_taker, by = c("player_name"="Player"))        

View(join2)
summary(join2$pos)
join2$W <- ifelse(join2$W == 'W',1,0)

######################################################
installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)
names(join2)
join2$age<-as.numeric(levels(join2$age))[join2$age]
join2$height<-as.numeric(levels(join2$height))[join2$height]
join2$age_def<-as.numeric(levels(join2$age_def))[join2$age_def]
join2$height_def<-as.numeric(levels(join2$height_def))[join2$height_def]
join2 <- na.omit(join2) 
xjoin2 <- scale(join2[,c(3:12,17,27:38)])
summary(xjoin2)
apply(xjoin2,2,sd) # sd=1
apply(xjoin2,2,mean) # mean=0
View(join2)
## First we fit k-Means with k=2 (i.e. two clusters)
two <- kmeans(xjoin2,2,nstart=10)
kfit <- lapply(1:100, function(k) kmeans(xjoin2,k))
kaic <- sapply(kfit,kIC)
kbic  <- sapply(kfit,kIC,"B")
kHDic  <- sapply(kfit,kIC,"C")
## Now we plot them, first we plot AIC
par(mar=c(1,1,1,1))
par(mai=c(1,1,1,1))
plot(kaic, xlab="k (# of clusters)", ylab="IC (Deviance + Penalty)", 
     ylim=range(c(kaic,kbic,kHDic)), # get them on same page
     type="l", lwd=2)
abline(v=which.min(kaic))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
lines(kHDic, col=3, lwd=2)
abline(v=which.min(kHDic),col=3)
text(c(which.min(kaic),which.min(kbic),which.min(kHDic)),c(mean(kaic),mean(kbic),mean(kHDic)),c("AIC","BIC","HDIC"))
which.min(kaic)
which.min(kbic)
which.min(kHDic)
k=100#AIC
k=100#BIC
k=17#HDIC
k=5
# BIC: 56%; HDIC: 40%; k=5 26% (for explaining deviance of x)
1 - sum(kfit[[k]]$tot.withinss)/kfit[[k]]$totss


Ssimple_kmeans <- kmeans(xjoin2,10,nstart=10)
colorcluster <- 1+Ssimple_kmeans$cluster
### Summarize a variable on each cluster
a0=tapply(join2[,9],Ssimple_kmeans$cluster,mean)
a1=tapply(join2[,10],Ssimple_kmeans$cluster,mean)
a2=tapply(join2[,11],Ssimple_kmeans$cluster,mean)
a3=tapply(join2[,12],Ssimple_kmeans$cluster,mean)
a4=tapply(join2[,13],Ssimple_kmeans$cluster,mean)
a5=tapply(join2[,17],Ssimple_kmeans$cluster,mean)
a6=tapply(join2[,24],Ssimple_kmeans$cluster,mean)
a7=tapply(join2[,25],Ssimple_kmeans$cluster,mean)
a8=tapply(join2[,26],Ssimple_kmeans$cluster,mean)
a9=tapply(join2[,33],Ssimple_kmeans$cluster,mean)
a10=tapply(join2[,34],Ssimple_kmeans$cluster,mean)
a11=tapply(join2[,35],Ssimple_kmeans$cluster,mean)
a12=tapply(join2[,36],Ssimple_kmeans$cluster,mean)
a13=tapply(join2[,38],Ssimple_kmeans$cluster,mean)
df=data.frame(cbind(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13))
View(df)
getwd()
### We can compute the wins on each cluster
### and contrast with the overall FGM
tapply(join2[,18],Ssimple_kmeans$cluster,mean,na.rm=TRUE)
mean(join2[,18],na.rm=TRUE)


###############################
#plot cluster plot
names(join2)
simple <- join2[,c(9,12)]
par(mar = c(5, 5, 3, 1))
Ssimple <- scale(simple)
apply(Ssimple,2,sd) 
apply(Ssimple,2,mean) 
Ssimple_kmeans1 <- kmeans(Ssimple,10,nstart=10)
colorcluster <- 1+Ssimple_kmeans1$cluster

### simple_kmeans$cluster is a vector
### telling the cluster each observation belongs to.
### for example simple_kmeans$cluster[5]=2 
### means that observation 5 belongs to cluster 2.
### we can associate a color for each cluster 
### (and therefore for each observartion)
### in the vector colorcluster
plot(Ssimple, col = 1, xlab="SHOT_CLOCK", 
     ylab="SHOT_DIST",ylim=c(-1.5,2), cex = 0.5,
     main="k Means for shot distance and shot clock")
plot(Ssimple, col = colorcluster, xlab="SHOT_CLOCK",
     ylab="SHOT_DIST",ylim=c(-1.5,2), cex = 0.5,
     main="k Means for shot distance and shot clock")
points(Ssimple_kmeans1$centers, col = 1, pch = 24,
       cex = 1.5, lwd=1, bg = 2:5,
       main="k Means for shot distance and shot clock")

###########################
simple1 <- join2[,c(10,34)]
par(mar = c(5, 5, 3, 1))
Ssimple1 <- scale(simple1)
apply(Ssimple1,2,sd) 
apply(Ssimple1,2,mean) 
Ssimple_kmeans11 <- kmeans(Ssimple1,10,nstart=10)
colorcluster <- 1+Ssimple_kmeans11$cluster
plot(Ssimple1, col = 1, xlab="DRIBBLES", 
     ylab="age",ylim=c(-1.5,2), cex = 1.5)
plot(Ssimple1, col = colorcluster, xlab="DRIBBLES",
     ylab="age",ylim=c(-1.5,2), cex = 1.5)
points(Ssimple_kmeans11$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)


### The command
Ssimple_kmeans$centers
## displays the k centers (good to interpret)
Ssimple_kmeans$size
## displays the size of each cluster
b4=as.vector(Ssimple_kmeans$cluster)
names(join2)
join3=cbind(join2[,1],join2[,6],join2[,20],b4)
View(join3)
getwd()
write.csv(join3,'C:/Users/Cara You/Desktop/duke/data science for business/project/Cara.csv', row.names = FALSE)
b1=Ssimple_kmeans$size
b2=Ssimple_kmeans$size/127745
b3=tapply(join2[,18],Ssimple_kmeans$cluster,mean,na.rm=TRUE)

mean(join2[,18],na.rm=TRUE)
df=data.frame(cbind(b1,b2,b3))
View(df)


###########################################
### PCA
installpkg("plfm")
library(plfm)
pca.xjoin2 <- prcomp(xjoin2, scale=TRUE)
plot(pca.xjoin2,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
xjoin2pc <- predict(pca.xjoin2) # scale(jpin)%*%pca.join$rotation
### This will give you the PC score of each shot on each Principal Component
xjoin2pc
plot(xjoin2pc[,1:2], pch=21,  main="")

plot(xjoin2pc[,3:4], pch=21,  main="")

loadings <- pca.xjoin2$rotation[,1:4]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:23],1]
loadingfit <- lapply(1:23, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:23],2]
loadingfit <- lapply(1:23, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:23],3]
loadingfit <- lapply(1:23, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:23],4]
loadingfit <- lapply(1:23, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]

