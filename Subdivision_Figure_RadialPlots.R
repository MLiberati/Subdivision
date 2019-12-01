# Create radial maps for best solutions
# COMBO demo area

remove(list=ls())

## R Libraries ##
library(plotly)
library(ggplot2)
library(Rmisc)
library(cowplot)
library(scales)

library(plotrix)
library(dplyr)

CFA_short <- c("Riparian", "Priority", "Represent")
CFA_long_spaced <- c("Riparian corridor", "Priority habitat", "Representativeness")

StabilizationPRE <- c(50)
StabilizationPOST <- c(50)
datePOST <- "2018.07.12"
datePRE <- "2018.07.12"
todaydate <- gsub("-",".",Sys.Date()) # today's date (YYYY.MM.DD)

objectives <- c("Cost","Charact","Connect","Riparian","Priority","Represent")
direction <-c(rep("min",6))
labels <- c("Cost", "Town\nconflict", "Network\ndistance", "Missing\nriparian", "Missing\npriority", "Representative\nscore")

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
############## GGPLOT TEMPLATES ###################
###################################################

textsize <- 20

mytheme_both <- theme(text=element_text(family="serif", size=textsize), 
                      legend.position="none", 
                      legend.background = element_rect(fill = "grey95"),
                      axis.ticks.length=unit(0.3,"cm"),
                      axis.line = element_line(colour = 'black', size = 1))

mypoints <- geom_point(size=1, shape=20, color="grey80")

myscale <- scale_fill_gradient(low='white', high='red', space="Lab", guide="colourbar")

myplot <- theme_classic(base_family = "serif")

size1 <- 4; size2 <- 5; size3 <- 3

shape1 <- 0; shape2 <- 1; shape3 <- 2
shape4 <- 5; shape5 <- 6; shape6 <- 4

stroke <- 4
color2 <- "black"
color <- "grey"

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

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Riparian", Yminmax = min, fitness=fitnessPRE)
a <- min(front$x[front$y <= 0.01], na.rm = T)
bestPRE <- fitnessPRE$Solution[fitnessPRE$Cost%in%a]

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Riparian", Yminmax = min, fitness=fitnessPOST)
a <- min(front$x[front$y <= 0.01], na.rm = T)
bestPOST <- fitnessPOST$Solution[fitnessPOST$Cost%in%a]

#----------------------#

df <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
colnames(df) <- objectives
for(i in 1:length(PREbestSolns)){
  df[i,] <- fitnessPRE[PREbestSolns[i],2:ncol(fitnessPRE)]
  df[i+3,] <- fitnessPOST[POSTbestSolns[i],2:ncol(fitnessPOST)]
}

#----------------------#

# Rescale PRE & POST fitnesses
for(l in 2:(length(objectives)+1)){
  fitnessPRE[,l] <- rescale(fitnessPRE[,l], c(0,1))
  fitnessPOST[,l] <- rescale(fitnessPOST[,l], c(0,1))
}

##################################################################
##################################################################
##################################################################

### MCP solution - Riparian - with budget

# matrix for the polygons
df <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
colnames(df) <- objectives
for(i in 1:length(PREbestSolns)){
  df[i,] <- fitnessPRE[PREbestSolns[i],2:ncol(fitnessPRE)]
  df[i+3,] <- fitnessPOST[POSTbestSolns[i],2:ncol(fitnessPOST)]
}

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
pnt[1,4] <- fitnessPRE[PREbestSolns[1],5]
pnt[4,4] <- fitnessPOST[POSTbestSolns[1],5]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[1,]) #PRE
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[4,]) #POST
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## MCP - Riparian 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('MCP_riparian_30budget.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(1,4),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common cost
radial.plot(df[1,1], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark objective being optimized for the budget
radial.plot(pnt[c(1,4),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.455
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Riparian Scenario MCSs")), cex=0.55)

dev.off()

#-----------------------------------------------------------#

### SCP solution - Get all riparian for least cost

# matrix for the polygons
#PRE
row1 <- c(84.58, 103.56, 0.068, 0, 49.28, 2.026)
scale1 <- c(283.85, 182.39, 2.728, 258.03, 117.80, 13.5965)
row1 <- row1 / scale1
#POST
row2 <- c(80.55, 61.71, 0.068, 0, 68.07, 4.877)
scale2 <- c(284.21, 182.36, 2.6343, 258.03, 117.801, 19.178)
row2 <- row2 / scale2

df <- data.frame(rbind(row1,row2))

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=2, ncol=length(objectives)))
pnt[1,1] <- df[1,1]
pnt[2,1] <- df[2,1]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[1,]) #PRE
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[2,]) #POST
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## SCP - Riparian 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('SCP_allriparian_leastcost.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(1,2),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common riparian
radial.plot(df[1,4], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark cost (which we are trying to minimize)
radial.plot(pnt[c(1,2),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.455
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Riparian Scenario SCSs")), cex=0.55)

dev.off()

##################################################################
##################################################################
##################################################################

### MCP solution - Priority habitat - with budget

# matrix for the polygons
df <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
colnames(df) <- objectives
for(i in 1:length(PREbestSolns)){
  df[i,] <- fitnessPRE[PREbestSolns[i],2:ncol(fitnessPRE)]
  df[i+3,] <- fitnessPOST[POSTbestSolns[i],2:ncol(fitnessPOST)]
}

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
pnt[2,5] <- fitnessPRE[PREbestSolns[2],6]
pnt[5,5] <- fitnessPOST[POSTbestSolns[2],6]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[2,]) #PRE
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[4,]) #POST
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## MCP - Priority 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('MCP_priority_30budget.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(2,5),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common cost
radial.plot(df[1,1], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark objective being optimized for the budget
radial.plot(pnt[c(2,5),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.475
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Priority Habitat Scenario MCSs")), cex=0.55)

dev.off()

#-----------------------------------------------------------#

### SCP solution - Get all priority for least cost

# matrix for the polygons
#PRE
row1 <- c(59.18, 99.78, 0.0498, 88.77, 0, 9.38)
scale1 <- c(283.85, 182.39, 2.728, 258.03, 117.80, 13.5965)
row1 <- row1 / scale1
#POST
row2 <- c(53.65, 55.86, 0.170, 134.11, 0, 19.25)
scale2 <- c(284.21, 182.36, 2.6343, 258.03, 117.801, 19.178)
row2 <- row2 / scale2

df <- data.frame(rbind(row1,row2))

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=2, ncol=length(objectives)))
pnt[1,1] <- df[1,1]
pnt[2,1] <- df[2,1]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[1,]) #PRE
tot <- data.frame(cbind(V1,V2))
areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)

areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[2,]) #POST
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## SCP - Priority 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('SCP_allpriority_leastcost.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(1,2),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common priority
radial.plot(df[1,5], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark cost (which we are trying to minimize)
radial.plot(pnt[c(1,2),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.475
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Priority Habitat Scenario SCSs")), cex=0.55)

dev.off()

##################################################################
##################################################################
##################################################################

### MCP solution - Represent - with budget

# matrix for the polygons
df <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
colnames(df) <- objectives
for(i in 1:length(PREbestSolns)){
  df[i,] <- fitnessPRE[PREbestSolns[i],2:ncol(fitnessPRE)]
  df[i+3,] <- fitnessPOST[POSTbestSolns[i],2:ncol(fitnessPOST)]
}

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=length(PREbestSolns)*2, ncol=length(objectives)))
pnt[3,6] <- fitnessPRE[PREbestSolns[3],7]
pnt[6,6] <- fitnessPOST[POSTbestSolns[3],7]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[3,]) #PRE
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[6,]) #POST
tot <- data.frame(cbind(V1,V2))

areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## MCP - Represent 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('MCP_represent_30budget.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(3,6),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common cost
radial.plot(df[3,1], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark objective being optimized for the budget
radial.plot(pnt[c(3,6),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.52
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Representativeness Scenario MCSs")), cex=0.55)

dev.off()

#-----------------------------------------------------------#

### SCP solution - Get all represent for least cost

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Represent", Yminmax = min, fitness=fitnessPRE)
a <- min(front$x[front$y <= 0.005], na.rm = T)
ALLbestPRE <- fitnessPRE$Solution[fitnessPRE$Cost%in%a]

front <- frontPnts_single(binsize = 100, xVar = "Cost", yVar = "Represent", Yminmax = min, fitness=fitnessPOST)
a <- min(front$x[front$y <= 0.005], na.rm = T)
ALLbestPOST <- fitnessPOST$Solution[fitnessPOST$Cost%in%a]

#PRE
row1 <- fitnessPRE[ALLbestPRE,c(2:7)]
#POST
row2 <- fitnessPOST[ALLbestPOST,c(2:7)]

df <- data.frame(rbind(row1,row2))

# matrix for the points to be highlighted
pnt <- data.frame(matrix(nrow=2, ncol=length(objectives)))
pnt[1,1] <- df[1,1]
pnt[2,1] <- df[2,1]

# calculate polygon areas for radial plots
#PRE
V1 <- c(1:6)
V2 <- as.numeric(df[1,]) #PRE
tot <- data.frame(cbind(V1,V2))
areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))
#POST
V1 <- c(1:6)
V2 <- as.numeric(df[2,]) #POST
tot <- data.frame(cbind(V1,V2))
areas <- tot %>% 
  setNames(c("variable", "value")) %>% 
  mutate(nextval = lead(value, default = value[1]),
         angle   = (1/6) * (2*pi),
         # change 1/n to number of variables
         area    = value*nextval*sin(angle)/2)
areas %>% summarise(total = sum(area))

########## SCP - Represent 

setwd("P:\\Projects-Current\\Subdivision\\Sub_Figures")
jpeg('SCP_allrepresent_leastcost.jpg', width = 3.5, height = 3.5, units = 'in', res=300)

# plot pre and post polygons
par(cex.axis=0.6, cex.lab=0.5, family='serif')
radial.plot(df[c(1,2),], rp.type="p", labels=labels, line.col=c("grey","black"), lwd=2, 
            radial.lim=c(0,1),mar=c(0,1,3,1), show.grid.labels=3)
# mark common represent
radial.plot(df[1,6], rp.type="s", radial.lim=c(0,1), point.symbols=18, point.col="black", cex=1.2, add=TRUE) 
# mark cost (which we are trying to minimize)
radial.plot(pnt[c(1,2),], rp.type="s", radial.lim=c(0,1), point.symbols=16, point.col=c("grey","black"), cex=1, add=TRUE) 

# Add legend
x = -0.52
y = 1.5
legend(x=x, y=y, c("Original delineation","Subdivided delineation"), col=c("grey","black"), lty=1, lwd=2, 
       title=expression(bold("Representativeness Scenario SCSs")), cex=0.55)

dev.off()
