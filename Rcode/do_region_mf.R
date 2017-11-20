##
##set working directory where the data files are stored
##
rm(list=ls())
#setwd("D:/VID/paper/flowmap/circlize")

##
##install packages if not already done so (uncomment)
##
# install.packages("circlize")
# install.packages("plyr")

##
##read in table and define matrix (m) and reference data.frame (df1)
##
#m<-read.table("region_custom.txt", skip=2, stringsAsFactors=FALSE)
m<-read.table("data/region_custom.txt", skip=2, stringsAsFactors=FALSE)

#data.frame for details on each region
df1<-m[,1:3]
names(df1)<-c("order","rgb","region")
df1$region<-gsub("_", " ", df1$region)
df1
#flow matrix
m<-m[,-(1:3)]/1e06
m<-as.matrix(m)
dimnames(m)<-list(orig=df1$region,dest=df1$region)
df1
grid.col = df1$rcol
m
#c(palCom, palCom2, palCom3, palCom4, palAdd, palEnd)
#length(grid.col)
#grid.col
col_mat <- rep(df1$lcol,each=length(grid.col))
#col_mat[m<0.5] <-"#00000000"
m2<-m
m2[m2<0.2] <- 0
m2
# Make a much simpler version of the graph: 
circos.clear()
chordDiagram(as.matrix(m2),grid.col=grid.col, col= col_mat, transparency = 0.5, link.border= 0, directional = 1, direction.type = c("arrows"),
             link.arr.type = "big.arrow")
circos.clear()

##
##sort order of data.frame and matrix for plotting in circos
##
library("plyr")
df1<-arrange(df1, order)
df1$region <- factor(df1$region, levels=df1$region)
m<-m[levels(df1$region),levels(df1$region)]

##
##define ranges of circos sectors and their colors (both of the sectors and the links)
##
df1$xmin <- 0
df1$xmax <- rowSums(m)+colSums(m)
n<-nrow(df1)
df1 <- cbind(df1, matrix(as.numeric(unlist(strsplit(df1$rgb,","))),nrow=n, byrow=TRUE) )
names(df1)[ncol(df1)-2:0]<-c("r","g","b")
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)

##
##plot sectors
##
library("circlize")
par(mar=rep(0,4))
circos.clear()

#basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.1), start.degree = 90, gap.degree =4)

#sector details
circos.initialize(factors = df1$region, xlim = cbind(df1$xmin, df1$xmax))

#plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$region, track.height=0.1, 
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                       
                         #plot country labels
                         circos.text(x=mean(xlim), y=2.2, labels=name, direction = "arc")
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.8, direction = "outside", major.at=seq(0,floor(df1$xmax)[i]), minor.ticks=1,
                                     labels.away.percentage = 0.15)
                       })


##
##plot links
##
#add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
df1$sum1 <- colSums(m)
df1$sum2 <- numeric(n)

#create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
df2<-cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2<-reshape(df2, idvar="orig", varying=list(1:n), direction="long", timevar="dest", time=rownames(m),  v.names = "m")
df2<-arrange(df2,desc(m))

#keep only the largest flows to avoid clutter
df2<-subset(df2, m>quantile(m,0.65))


for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$region)
  j<-match(df2$dest[k],df1$region)
  
  #plot link
  circos.link(sector.index1=df1$region[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$region[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[i], top.ratio=0.66, top.ratio.low=0.67)
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}

#save current plot to pdf
dev.copy2pdf(file = "rregion.pdf", height=10, width=10)


# #out first
# blank in part of main sector
# circos.rect(min(xlim)+rowSums(m)[i], min(ylim), max(xlim), 0.3, col = "white", border = "white")

# #out first
# df1$sum1 <- numeric(n)
# df1$sum2 <- colSums(m)