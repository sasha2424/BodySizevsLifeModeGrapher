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
# Custom 6

GRAPH <- 5

######

#For SPECIAL GRAPH
#Set start and end age in MYA

StartAge <- 150
EndAge <- 100

######

# FOR CUSTOM GRAPH

#should data be scaled
SCALED <- FALSE

#phyla to be used
#either phyla name or NA for all Phyla
#multiple names may be used with c(a,b,c) funtion
#use print(unique(data$phylum)) to see all available Phyla


CustomPhyla <- NA


CustomStartAge <- 600
CustomEndAge <- 0


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
SpecM <- NA
CustM <- NA


###################################################


#Graphical Settings

#line thickness
thickness <- 5

#line smoothing
smooth <-.5

#plot parameters
AllScale <- 1
AxisScale <- 1.5
LabelScale <- 1.5
TitleScale <- 1.5


#Line colors and dot colors

ACol <- rgb(1,0,0)
AColLine <- rgb(.8,0,0)

BCol <- rgb(0,1,0)
BColLine <- rgb(0,.8,0)

CCol <- rgb(1,0,1)
CColLine <- rgb(.8,0,.8)

MCol <- rgb(0,0,1)
MColLine <- rgb(0,0,.8)

TCol <- rgb(.5,.5,1)
TColLine <- rgb(.4,.4,.8)

SpecCol <- rgb(.5,.5,1)
SpecColLine <- rgb(.4,.4,.8)

CustCol <- rgb(0,1,1)
CustColLine <- rgb(0,.8,.8)

par(pch = 16,lwd = thickness,cex = AllScale,cex.axis = AxisScale,cex.lab = LabelScale,cex.main = TitleScale)

###################################################





data$logVol <- log10(data$max_vol)

A <- subset(data,data$phylum == 'Arthropoda')
B <- subset(data,data$phylum == 'Brachiopoda')
C <- subset(data,data$phylum == 'Chordata')
M <- subset(data,data$phylum == 'Mollusca')
T <- data
Spec <- subset(data,data$lad_age > EndAge & data$lad_age < StartAge)

Cust <- subset(data,data$lad_age > CustomEndAge & data$lad_age < CustomStartAge)
if(!(is.na(CustomPhyla))){
Cust <- subset(data,data$phylum %in% CustomPhyla)
}


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

if(is.na(SpecM)){
Specmax <- signif(quantile(Spec$max_vol,.75,na.rm = TRUE)+1.5*IQR(Spec$max_vol,na.rm = TRUE),SIG)
} else {
Specmax <- SpecM
}

if(is.na(CustM)){
Custmax <- signif(quantile(Cust$max_vol,.75,na.rm = TRUE)+1.5*IQR(Cust$max_vol,na.rm = TRUE),SIG)
} else {
Custmax <- CustM
}



# creating size bins for all data ( A , B , C , M , T, Spec, Cust)
if(GRAPH == 0 || GRAPH == 6){
for(i in 0:NoB){
A$sizeBin[A$max_vol > i*(Amax/NoB) & A$max_vol <= (i+1)*(Amax/NoB)] <- i
}
}

if(GRAPH == 1 || GRAPH == 6){
for(i in 0:NoB){
B$sizeBin[B$max_vol > i*(Bmax/NoB) & B$max_vol <= (i+1)*(Bmax/NoB)] <- i
}
}

if(GRAPH == 2 || GRAPH == 6){
for(i in 0:NoB){
C$sizeBin[C$max_vol > i*(Cmax/NoB) & C$max_vol <= (i+1)*(Cmax/NoB)] <- i
}
}

if(GRAPH == 3 || GRAPH == 6){
for(i in 0:NoB){
M$sizeBin[M$max_vol > i*(Mmax/NoB) & M$max_vol <= (i+1)*(Mmax/NoB)] <- i
}
}

if(GRAPH == 4){
for(i in 0:NoB){
T$sizeBin[T$max_vol > i*(Tmax/NoB) & T$max_vol <= (i+1)*(Tmax/NoB)] <- i
}
}

if(GRAPH == 5){
for(i in 0:NoB){
Spec$sizeBin[Spec$max_vol > i*(Specmax/NoB) & Spec$max_vol <= (i+1)*(Specmax/NoB)] <- i
}
}

if(GRAPH == 6){
for(i in 0:NoB){
Cust$sizeBin[Cust$max_vol > i*(Custmax/NoB) & Cust$max_vol <= (i+1)*(Custmax/NoB)] <- i
}
}




if(GRAPH == 0){
Ad <- length(unique(A[A$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Ad <- cbind(Ad,length(unique(A[A$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 1){
Bd <- length(unique(B[B$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Bd <- cbind(Bd,length(unique(B[B$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 2){
Cd <- length(unique(C[C$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Cd <- cbind(Cd,length(unique(C[C$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 3){
Md <- length(unique(M[M$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Md <- cbind(Md,length(unique(M[M$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 4){
Td <-(length(unique(T[T$sizeBin == i,]$ecospace)))/(length(T[T$sizeBin == i,]$ecospace))
for(i in 1:NoB){
Td <- cbind(Td,(length(unique(T[T$sizeBin == i,]$ecospace)))/(length(T[T$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 5){
Specd <- length(unique(Spec[Spec$sizeBin == 0,]$ecospace))
for(i in 1:NoB){
Specd <- cbind(Specd,length(unique(Spec[Spec$sizeBin == i,]$ecospace)))
}
}

if(GRAPH == 6){
if(SCALED){
Custd <-(length(unique(Cust[Cust$sizeBin == i,]$ecospace)))/(length(Cust[Cust$sizeBin == i,]$ecospace))
} else {
Custd <- length(unique(Cust[Cust$sizeBin == 0,]$ecospace))
}
for(i in 1:NoB){
if(SCALED){
Custd <- cbind(Custd,(length(unique(Cust[Cust$sizeBin == i,]$ecospace)))/(length(Cust[Cust$sizeBin == i,]$ecospace)))
} else {
Custd <- cbind(Custd,length(unique(Cust[Cust$sizeBin == i,]$ecospace)))
}
}
}





#Graphing


#par(mfrow=c(2,2))


#Individual


if(GRAPH == 0){
LBLA <- paste(c('Biovolume (',toString(Amax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Ad,xlab=LBLA,ylab='Unique Life Modes',col=ACol,main='Unique Life Modes per Body Size Division Among Arthropods')
lines(smooth.spline(0:NoB, Ad,spar = smooth),col = AColLine)
dev.print(pdf, 'arthropoda.pdf')
}

if(GRAPH == 1){
LBLB <- paste(c('Biovolume (',toString(Bmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Bd,xlab=LBLB,ylab='Unique Life Modes',col = BCol,main='Unique Life Modes per Body Size Division Among Brachiopods')
lines(smooth.spline(0:NoB, Bd,spar = smooth),col = BColLine)
dev.print(pdf, 'brachiopoda.pdf')
}

if(GRAPH == 2){
LBLC <- paste(c('Biovolume (',toString(Cmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Cd,xlab=LBLC,ylab='Unique Life Modes',col = CCol,main='Unique Life Modes per Body Size Division Among Chordates')
lines(smooth.spline(0:NoB, Cd,spar = smooth),col = CColLine)
dev.print(pdf, 'chordata.pdf')
}

if(GRAPH == 3){
LBLM <- paste(c('Biovolume (',toString(Mmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Md,xlab=LBLM,ylab='Unique Life Modes',col = MCol,main='Unique Life Modes per Body Size Division Among Molluscs')
lines(smooth.spline(0:NoB, Md,spar = smooth),col = MColLine)
dev.print(pdf, 'mullusca.pdf')
}

if(GRAPH == 4){
LBLT <- paste(c('Biovolume (',toString(Tmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
plot(0:NoB,Td,xlab=LBLT,ylab='Unique Life Modes Scaled by Number of Organisms',col = TCol,main='Unique Life Modes Scaled by Number of Organisms per Body Size Division Among All')
lines(smooth.spline(0:NoB, Td,spar = smooth),col = TColLine)
dev.print(pdf, 'CombinedScaled.pdf')
}

if(GRAPH == 5){
LBLSpec <- paste(c('Biovolume (',toString(Specmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
TITLE <- paste(c('Unique Life Modes per Body Size Division Among All Marine Organism (',StartAge,',',EndAge,') MYA'),collapse = ' ')
plot(0:NoB,Specd,xlab=LBLSpec,ylab='Unique Life Modes',col = SpecCol,main=TITLE)
lines(smooth.spline(0:NoB, Specd,spar = smooth),col = SpecColLine)
dev.print(pdf, 'Special.pdf')
}



if(GRAPH == 6){
LBLCust <- paste(c('Biovolume (',toString(Custmax/NoB),' mm^3 multiplied by X value)'),collapse = ' ')
if(is.na(CustomPhyla)){
TITLE <- paste(c('Unique Life Modes per Body Size Division Among All Marine Organism (',CustomStartAge,',',CustomEndAge,') MYA'),collapse = ' ')
} else {
TITLE <- paste(c('Unique Life Modes per Body Size Division Among', CustomPhyla ,'(',CustomStartAge,',',CustomEndAge,') MYA'),collapse = ' ')
}
plot(0:NoB,Custd,xlab=LBLCust,ylab='Unique Life Modes',col = CustCol,main=TITLE)
lines(smooth.spline(0:NoB, Custd,spar = smooth),col = CustColLine)
dev.print(pdf, 'Custom.pdf')
}








