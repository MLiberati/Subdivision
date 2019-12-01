# Creating maps based on GA results
# COMBO demo area

remove(list=ls())

## R Libraries ##
library(plotly)
library(ggplot2)
library(Rmisc)
library(cowplot)
library(scales)

CFA_short <- c("Riparian", "Priority", "Represent")
CFA_long_spaced <- c("Riparian corridor", "Priority habitat", "Representativeness")

StabilizationPRE <- c(50)
StabilizationPOST <- c(50)
datePOST <- "2018.07.12"
datePRE <- "2018.07.12"
todaydate <- gsub("-",".",Sys.Date()) # today's date (YYYY.MM.DD)

objectives <- c("Cost","Charact","Connect","Riparian","Priority","Represent")
direction <-c(rep("min",6))
Y_labels <- c("Cost (mill $US)", "Town character conflict", "Fragmentation (km)", "Missing riparian", 
              "Missing priority", "Non-representative")
X_labels <- c("Cost (mill $US)", "Town character conflict", "Fragmentation (km)", "Missing riparian", 
              "Missing priority", "Non-representative")

###################################################
###### FUNCTION TO GENERATE FRONTIER POINTS #######
###################################################

frontPnts <- function(binsize, xVar, yVar, Yminmax, fitness){
  rangeX <- range(fitness[,xVar]) 
  binwidth <- (rangeX[2]-rangeX[1])/binsize
  
  numb <- binsize + 2
  front <- as.data.frame(matrix(nrow=numb,ncol=2))
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=min(fitness[,xVar]),to=max(fitness[,xVar]),by=(binwidth)) 
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    if(i==1){ 
      x <-cutoffs[i]
      y <- fitness[,yVar][fitness[,xVar]%in%x] 
      front[i,1:2]<-c(x,y)
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
    }
    
    else if(i>1&i<binsize){
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y) 
    }
    
    else if(i==binsize){ 
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
      x <- max(fitness[,xVar])
      y <- do.call(Yminmax,as.list(fitness[,yVar][fitness[,xVar]%in%x]))
      front[i+2,1:2]<-c(x,y)
    }
  }
  return(front)
}

frontPnts_single <- function(binsize, xVar, yVar, Yminmax, fitness){
  rangeX <- range(fitness[,xVar]) 
  binwidth <- (rangeX[2]-rangeX[1])/binsize
  
  numb <- binsize + 2
  front <- as.data.frame(matrix(nrow=numb,ncol=2))
  names(front) <- c("x","y")
  
  #establish the cutoffs
  cutoffs<-seq(from=min(fitness[,xVar]),to=max(fitness[,xVar]),by=(binwidth)) 
  #identify min/max solutions within each bin
  for(i in 1:binsize){
    if(i==1){ 
      x <-cutoffs[i]
      y <- fitness[,yVar][fitness[,xVar]%in%x] 
      front[i,1:2]<-c(x,y)
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
    }
    
    else if(i>1&i<binsize){
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y) 
    }
    
    else if(i==binsize){ 
      low <- cutoffs[i]
      upper <- cutoffs[i+1]
      binbest <- subset(fitness, fitness[,xVar]>low & fitness[,xVar]<upper) 
      y <- do.call(Yminmax, as.list(binbest[,yVar])) 
      x <- min(binbest[,xVar][binbest[,yVar]%in%y])
      front[i+1,1:2]<-c(x,y)
      x <- max(fitness[,xVar])
      y <- do.call(Yminmax,as.list(fitness[,yVar][fitness[,xVar]%in%x]))
      front[i+2,1:2]<-c(x,y)
    }
  }
  return(front)
}

###################################################
###################################################
###################################################

#### Identify solutions of interest

# PRE-subdivision
setwd("P:\\Projects-Current\\Subdivision\\Sub_Results\\PRE_COMBO_forR")
fitnessPRE <- read.csv(paste0(StabilizationPRE,"gens_PRE_forR_",datePRE,".csv"))
fitnessPRE <- fitnessPRE[,c("Solution","Cost","Charact","Connect","Riparian","Priority","Represent")]
fitnessPRE$Cost <- fitnessPRE$Cost / 1000000
fitnessPRE$Connect <- fitnessPRE$Connect / 1000

#----------------------#

# POST-subdivision
setwd("P:\\Projects-Current\\Subdivision\\Sub_Results\\POST_COMBO_forR")
fitnessPOST <- read.csv(paste0(StabilizationPOST,"gens_POST_forR_",datePOST,".csv"))
fitnessPOST <-fitnessPOST[,c("Solution","Cost","Charact","Connect","Riparian","Priority","Represent")]
fitnessPOST$Cost <- fitnessPOST$Cost / 1000000
fitnessPOST$Connect <- fitnessPOST$Connect / 1000

#----------------------#

budget <- 30

PREbestSolns <- c()
# Solutions that acheive the best scenario outcomes for a budget
for(i in 4:length(objectives)){
  front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = objectives[i], Yminmax = min, fitness=fitnessPRE)
  
  if(i<6){
    a <- min(front$y[front$x <= budget & front$x != 0], na.rm = T)
    best <- fitnessPRE$Solution[fitnessPRE[,objectives[i]]%in%a]
  }
  
  #get best representative outcome for the budget
  if(i == 6){
    mini_df <- front[front$x <= budget & front$x != 0,]
    a <- mini_df[nrow(mini_df),2]
    best <- fitnessPRE$Solution[fitnessPRE[,objectives[i]]%in%a]
  }
  
  if(length(best) == 1){
    PREbestSolns[i-3] <- best
  }
  
  if(length(best) > 1){
    mini_df <- data.frame(matrix(nrow=length(best), ncol=length(objectives)+1))
    for(m in 1:length(best)){
      mini_df[m,] <- fitnessPRE[best[m],c(1:7)]
    }
    vals = range(mini_df[,2])
    PREbestSolns[i-3] <- fitnessPRE$Solution[match(vals[1],fitnessPRE$Cost)]
  }
}

POSTbestSolns <- c()
# Solutions that acheive the best scenario outcomes for a budget
for(i in 4:length(objectives)){
  front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = objectives[i], Yminmax = min, fitness=fitnessPOST)

  if(i<6){
    a <- min(front$y[front$x <= budget & front$x != 0], na.rm = T)
    best <- fitnessPOST$Solution[fitnessPOST[,objectives[i]]%in%a]
  }
  
  #get best representative outcome for the budget
  if(i == 6){
    mini_df <- front[front$x <= budget & front$x != 0,]
    a <- mini_df[nrow(mini_df),2]
    best <- fitnessPOST$Solution[fitnessPOST[,objectives[i]]%in%a]
  }
  
  if(length(best) == 1){
    POSTbestSolns[i-3] <- best
  }
  
  if(length(best) > 1){
    mini_df <- data.frame(matrix(nrow=length(best), ncol=length(objectives)+1))
    for(m in 1:length(best)){
      mini_df[m,] <- fitnessPOST[best[m],c(1:7)]
    }
    vals = range(mini_df[,2])
    POSTbestSolns[i-3] <- fitnessPOST$Solution[match(vals[1],fitnessPOST$Cost)]
  }
}

#----------------------#

# Achieve near-perfect representativeness for lowest cost

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Represent", Yminmax = min, fitness=fitnessPRE)
a <- min(front$x[front$y <= 0.5], na.rm = T)
ALLbestPRE <- fitnessPRE$Solution[fitnessPRE$Cost%in%a]
PREbestSolns <- c(PREbestSolns, ALLbestPRE)

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Represent", Yminmax = min, fitness=fitnessPOST)
a <- min(front$x[front$y <= 0.5], na.rm = T)
ALLbestPOST <- fitnessPOST$Solution[fitnessPOST$Cost%in%a]
POSTbestSolns <- c(POSTbestSolns, ALLbestPOST)

###################################################
###################################################
###################################################

#### Identify binary parcel strings for best COMPACT soln on budget

# Pre-subdivision

setwd("P:\\Projects-Current\\Subdivision\\Sub_Results\\PRE_Combo_Originals")
data <- read.csv(paste0(StabilizationPRE,"gens_PRE_",datePRE,".csv"),header=FALSE)

binary <- data[1,1]
NumParcelsPRE <- length(unlist(strsplit(as.character(binary), split="[,]")))
dfPRE <- data.frame(matrix(nrow=NumParcelsPRE, ncol=length(PREbestSolns)), row.names=seq(0,NumParcelsPRE-1,1))

colnames(dfPRE) <- c("BestRip", "BestHab", "BestRep", "SCP")

for(i in 1:length(PREbestSolns)){
  
  binary <- data[PREbestSolns[i],1]
  # unconcatinate objective values for each solution (row)
  commas <- unlist(strsplit(as.character(binary), split="[,]"))
  intermediates <- as.numeric(unlist(strsplit(as.character(binary), split="[,]"))[3:NumParcelsPRE-1])
  first <- as.numeric(unlist(strsplit(commas[1], split="[[]"))[2])
  last <- as.numeric(unlist(strsplit(commas[NumParcelsPRE], split="[]]"))[1])
  # asign objective values to the solution object
  dfPRE[1,i] <- first
  dfPRE[NumParcelsPRE,i] <- last
  dfPRE[2:(NumParcelsPRE-1),i] <- intermediates
  
}

#-------------------#

# Post-subdivision

setwd("P:\\Projects-Current\\Subdivision\\Sub_Results\\POST_Combo_Originals")
data <- read.csv(paste0(StabilizationPOST,"gens_POST_",datePOST,".csv"),header=FALSE)

binary <- data[1,1]
NumParcelsPOST <- length(unlist(strsplit(as.character(binary), split="[,]")))
dfPOST <- data.frame(matrix(nrow=NumParcelsPOST, ncol=length(POSTbestSolns)), row.names=seq(0,NumParcelsPOST-1,1))

colnames(dfPOST) <- c("BestRip", "BestHab", "BestRep", "SCP")

for(i in 1:length(POSTbestSolns)){
  
  binary <- data[POSTbestSolns[i],1]
  # unconcatinate objective values for each solution (row)
  commas <- unlist(strsplit(as.character(binary), split="[,]"))
  intermediates <- as.numeric(unlist(strsplit(as.character(binary), split="[,]"))[3:NumParcelsPOST-1])
  first <- as.numeric(unlist(strsplit(commas[1], split="[[]"))[2])
  last <- as.numeric(unlist(strsplit(commas[NumParcelsPOST], split="[]]"))[1])
  # asign objective values to the solution object
  dfPOST[1,i] <- first
  dfPOST[NumParcelsPOST,i] <- last
  dfPOST[2:(NumParcelsPOST-1),i] <- intermediates
  
}

###################################################
###################################################
###################################################


#### Add solutions of interest to shapefiles

setwd(paste("P:\\Projects-Current\\Subdivision\\Sub_DerivedFiles\\DemoArea_Combo"))

library(shapefiles)
library(raster)
library(rgdal)

shpPRE <- shapefile("demoArea_PRE_combo")

shpPRE$BestRip <- dfPRE[,1]
shpPRE$BestHab <- dfPRE[,2]
shpPRE$BestRep <- dfPRE[,3]
shpPRE$SCP <- dfPRE[,4]

shapefile(shpPRE, "demoArea_PRE_combo_SOLNS", overwrite=TRUE)

#--------------#

setwd(paste("P:\\Projects-Current\\Subdivision\\Sub_DerivedFiles\\DemoArea_Combo"))

shpPOST <- shapefile("demoArea_POST_combo_7500m2")

shpPOST$BestRip <- dfPOST[,1]
shpPOST$BestHab <- dfPOST[,2]
shpPOST$BestRep <- dfPOST[,3]
shpPOST$SCP <- dfPOST[,4]

shapefile(shpPOST, "demoArea_POST_combo_7200m2_SOLNS", overwrite=TRUE)


