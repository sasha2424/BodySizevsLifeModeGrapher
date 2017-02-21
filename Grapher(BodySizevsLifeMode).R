setwd("c:/Users/Alex/Desktop/R/")
data <- read.delim(file = 'genera_range_through.txt')
data <- subset(data,!is.na(data$ecospace))
data$sizeBin <- NA



###################################################

#Graph Settings
# A 0
# B 1
# C 2
# M 3
# T 4
# Spec 5
# Combined 6

GRAPH <- 4



#Spec graph
#Set start and end age in MYA

StartAge <- 650 
EndAge <- 500 


###################################################


NoB <- 100 #Number of Size Bins
SIG <- 1 # for rounding the bin size



###################################################

# Custom Max Values
# set to NA to use standard

AM <- NA
BM <- NA
CM <- NA
MM <- NA
TM <- NA


###################################################







data$logVol <- log10(data$max_vol)

A <- subset(data,data$phylum == 'Arthropoda')
B <- subset(data,data$phylum == 'Brachiopoda')
C <- subset(data,data$phylum == 'Chordata')
M <- subset(data,data$phylum == 'Mollusca')
T <- data
Spec <- subset(data,data$lad_age > EndAge & data$lad_age < StartAge)


Au <- unique(A$ecospace)
Bu <- unique(B$ecospace)
Cu <- unique(C$ecospace)
Mu <- unique(M$ecospace)
Tu <- unique(T$ecospace)

if(is.na(AM)){
Amax <- signif(quantile(A$max_vol,.75,na.rm = TRUE)+1.5*IQR(A$max_vol,na.rm = TRUE),SIG)
} else {
Amax <- AM
}

if(is.na(BM)){
Bmax <- signif(quantile(B$max_vol,.75,na.rm = TRUE)+1.5*IQR(B$max_vol,na.rm = TRUE),SIG)
} else {
Bmax <- BM
}

if(is.na(CM)){
Cmax <- signif(quantile(C$max_vol,.75,na.rm = TRUE)+1.5*IQR(C$max_vol,na.rm = TRUE),SIG)
} else {
Cmax <- CM
}

if(is.na(MM)){
Mmax <- signif(quantile(M$max_vol,.75,na.rm = TRUE)+1.5*IQR(M$max_vol,na.rm = TRUE),SIG)
} else {
Mmax <- MM
}

if(is.na(TM)){
Tmax <- signif(quantile(data$max_vol,.75,na.rm = TRUE)+1.5*IQR(data$max_vol,na.rm = TRUE),SIG)
} else {
Tmax <- TM
}



# creating size bins for all data ( A , B , C , M , T)

for(i in 0:NoB){
A$sizeBin[A$max_vol > i*(Amax/NoB) & A$max_vol <= (i+1)*(Amax/NoB)] <- i
}
for(i in 0:NoB){
B$sizeBin[B$max_vol > i*(Bmax/NoB) & B$max_vol <= (i+1)*(Bmax/NoB)] <- i
}
for(i in 0:NoB){
C$sizeBin[C$max_vol > i*(Cmax/NoB) & C$max_vol <= (i+1)*(Cmax/NoB)] <- i
}
for(i in 0:NoB){
M$sizeBin[M$max_vol > i*(Mmax/NoB) & M$max_vol <= (i+1)*(Mmax/NoB)] <- i
}
for(i in 0:NoB){
T$sizeBin[T$max_vol > i*(Tmax/NoB) & T$max_vol <= (i+1)*(Tmax/NoB)] <- i
}





Ad <- length(unique(A[A$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Ad <- cbind(Ad,length(unique(A[A$sizeBin == i,]$ecospace)))
}
Bd <- length(unique(B[B$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Bd <- cbind(Bd,length(unique(B[B$sizeBin == i,]$ecospace)))
}
Cd <- length(unique(C[C$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Cd <- cbind(Cd,length(unique(C[C$sizeBin == i,]$ecospace)))
}
Md <- length(unique(M[M$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Md <- cbind(Md,length(unique(M[M$sizeBin == i,]$ecospace)))
}

Td <-(length(unique(T[T$sizeBin == i,]$ecospace)))/(length(T[T$sizeBin == i,]$ecospace))
for(i in 1:NoB){
Td <- cbind(Td,(length(unique(T[T$sizeBin == i,]$ecospace)))/(length(T[T$sizeBin == i,]$ecospace)))
}

Specd <- length(unique(Spec[Spec$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Specd <- cbind(Specd,length(unique(Spec[Spec$sizeBin == i,]$ecospace)))
}





#Graphing
#
#individual and combined
#settings above



#par(mfrow=c(2,2))
thickness <- 1

#Individual

if(GRAPH == 0){
LBLA <- paste(c('Biovolume (',toString(Amax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Ad,xlab=LBLA,ylab='Unique Life Modes',pch = 16,col='Red',main='Unique Life Modes per Body Size Division Among Arthropods',lwd = thickness)
lines(smooth.spline(0:NoB, Ad, spar=0.35),lwd = thickness)
dev.print(pdf, 'arthropoda.pdf')
}

if(GRAPH == 1){
LBLB <- paste(c('Biovolume (',toString(Bmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Bd,xlab=LBLB,ylab='Unique Life Modes',pch = 16,col='olivedrab4',main='Unique Life Modes per Body Size Division Among Brachiopods',lwd = thickness)
lines(smooth.spline(0:NoB, Bd, spar=0.35),lwd = thickness)
dev.print(pdf, 'brachiopoda.pdf')
}

if(GRAPH == 2){
LBLC <- paste(c('Biovolume (',toString(Cmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Cd,xlab=LBLC,ylab='Unique Life Modes',pch = 16,col='royalblue',main='Unique Life Modes per Body Size Division Among Chordates',lwd = thickness)
lines(smooth.spline(0:NoB, Cd, spar=0.35),lwd = thickness)
dev.print(pdf, 'chordata.pdf')
}

if(GRAPH == 3){
LBLM <- paste(c('Biovolume (',toString(Mmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Md,xlab=LBLM,ylab='Unique Life Modes',pch = 16,col='Purple',main='Unique Life Modes per Body Size Division Among Molluscs',lwd = thickness)
lines(smooth.spline(0:NoB, Md, spar=0.35),lwd = thickness)
dev.print(pdf, 'mullusca.pdf')
}

if(GRAPH == 4){
LBLT <- paste(c('Biovolume (',toString(Tmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Td,xlab=LBLT,ylab='Unique Life Modes Scaled by Number of Organisms',pch = 16,col='orange',main='Unique Life Modes Scaled by Number of Organisms per Body Size Division Among All',lwd = thickness)
lines(smooth.spline(0:NoB, Td, spar=0.35),lwd = thickness)
dev.print(pdf, 'CombinedScaled.pdf')
}

if(GRAPH == 5){
LBLSpec <- paste(c('Biovolume (',toString(Tmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Specd,xlab=LBLSpec,ylab='Unique Life Modes',pch = 16,col='Purple',main='Unique Life Modes per Body Size Division Among $#@#$$#@#$ Marine Organism',lwd = thickness)
lines(smooth.spline(0:NoB, Specd, spar=0.80),lwd = thickness)
dev.print(pdf, 'Special.pdf')
}




#Combined

if(GRAPH == 6){
LBLCom <- paste(c('Biovolume (',toString(Tmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Ad,xlab=LBLCom,ylab='Unique Life Modes',pch = 18,col='Red',main='Unique Life Modes Per Body Size Division',lwd = thickness)
lines(smooth.spline(0:NoB, Ad, spar=0.35),col='Red',lwd = thickness)

points(0:NoB,Cd,col='orange',pch = 18,lwd = thickness)
lines(smooth.spline(0:NoB, Cd,spar=0.35),col='orange',lwd = thickness)

points(0:NoB,Md,col='Purple',pch=18,lwd = thickness)
lines(smooth.spline(0:NoB, Md,spar=0.35),col='Purple',lwd = thickness)

points(0:NoB,Bd,col='Cyan',pch="18",lwd = thickness)
lines(smooth.spline(0:NoB, Bd,spar=0.35),col='Cyan',lwd = thickness)

dev.print(pdf, 'combinedv2.pdf')
}






