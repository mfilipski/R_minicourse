# Clean the environment, clean the graphics parameters
rm(list=ls())
dev.off()

# Load packages (install them if needed)
list.of.packages <- c("UsingR", "datasets", "lattice", "circlize", "maps",
                      "gridExtra", "tm", "wordcloud", "RColorBrewer", "SnowballC", "xlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)


# Part 1: R makes graphs easy (Wow!) 
####################################

# First, some practice: load data, see what's in it, plot the points.
data(cars)
head(cars)
plot(cars)

# Multiple graphs: 
data(iris)
head(iris)
pairs(iris[1:4], pch= 16, col=iris$Species, main="Correlations between flower characteristics")


# Lots of graphs at once:
data("Medicare")
head(Medicare)
densityplot(~log(Average.Total.Payments) | Provider.State, data=Medicare )

# Even more graphs at once: 
data(snacks)
head(snacks)
pairs(snacks[,-1])

# Works with 3d too: 
cloud(Sepal.Length~Petal.Length*Petal.Width | Species, data=iris )


# You can easily superimpose different functions in the plot: 
data("diamonds")
head(diamonds)
g <- ggplot(data=diamonds, aes(y=price, x=carat, color=cut))
g <- g + facet_wrap(~cut, scales = "free_x") 
g <- g + geom_point(color="steelblue", shape=16, alpha=0.2)
g <- g + geom_smooth(method="lm",color="darkgrey", size=0.5, se=FALSE)
g <- g + geom_smooth(level=.99)
g




# Part 2: R has packages for so many things (More "Wow!")
####################################################################

### CIRCLIZE: A package to make Chord diagrams:
#---------------------------------------------------------------------

# Go see the graph online: http://www.global-migration.info/  (made with D3.js)
# We can make a basic version of that with R. 
# Read in Migration data and take a look
library(circlize)
rm(list=ls())
regData<-read.table("data/region_custom.txt", skip=2, stringsAsFactors=FALSE)
regData

# Extract flow matrix:
mFlows<-regData[,-(1:3)]/1e06
mFlows<-as.matrix(mFlows)
dimnames(mFlows)<-list(orig=regData[,3],dest=regData[,3])
# Exclude the smallest flows:
m<-mFlows
m[m<0.2] <- 0


# Extract colors:
col <- regData[,2] 
n <- length(col)
col <- cbind(col, matrix(as.numeric(unlist(strsplit(col,","))),nrow=n, byrow=TRUE))
cols<-rgb(col[,2], col[,3], col[,4], max = 255)
grid.col = cols
col_mat <- rep(cols,each=n)

# Call circos to make a (much simpler) version of the chord diagram: 
circos.clear()
circos.par(cell.padding=c(0,0,0,0), start.degree = 90, gap.degree =4)
chordDiagram(as.matrix(m),grid.col=grid.col, col= col_mat, transparency = 0.5, link.border= 0, directional = 1, direction.type = c("arrows"),
             link.arr.type = "big.arrow")
circos.clear()


### R can draw MAPS!  
#---------------------------------------------------------------------
dev.off()
library(maps)
states <- map_data("state")
head(states)
map("state")
map("state", boundary=FALSE, col="gray", add=TRUE)


# Even better: 
ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
    coord_fixed(1.3) +
    guides(fill=FALSE)

# Better still: 
head(state.x77)
usdata = data.frame(region=tolower(rownames(state.x77)), state.x77, stringsAsFactors=F)
mapIncome <- ggplot(usdata, aes(map_id = region)) + 
    geom_map(aes(fill = Income), map = states) +
    scale_fill_gradientn(colours=c("lightblue","darkblue")) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    geom_map(aes(fill = Income), map = states)+
    coord_fixed(1.3)
mapIncome

# Ok, last one:
mapIncome2 <- ggplot(usdata, aes(map_id = region)) + 
    geom_map(aes(fill = Income), map = states) +
    scale_fill_gradientn(colours=c("lightblue","darkblue")) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_fixed(1.3) + theme_void()
mapMurder <- ggplot(usdata, aes(map_id = region)) + 
    geom_map(aes(fill = Murder), map = states) +
    scale_fill_gradientn(colours=c("darkgreen","red")) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_fixed(1.3)  + theme_void()         
mapPop <- ggplot(usdata, aes(map_id = region)) + 
    geom_map(aes(fill = Population), map = states) +
    scale_fill_gradientn(colours=c("bisque","coral4")) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_fixed(1.3) + theme_void()
mapSchool <- ggplot(usdata, aes(map_id = region)) + 
    geom_map(aes(fill = HS.Grad), map = states) +
    scale_fill_gradientn(colours=c("brown4","blue")) + 
    expand_limits(x = states_map$long, y = states_map$lat) +
    coord_fixed(1.3) + theme_void()
grid.arrange(mapPop, mapIncome2, mapSchool, mapMurder)


### R can do Wordclouds: 
#---------------------------------------------------------------------
library(tm); library(wordcloud); library(SnowballC); library(RColorBrewer)
jbl <- readLines("data/jblyrics.txt")
justin <- Corpus(VectorSource(jbl))
inspect(justin)

# clean up 
j <- tm_map(justin, tolower)
j <- tm_map(j, removePunctuation)
j <- tm_map(j, removeWords, stopwords("English"))
j <- tm_map(j, removeWords, c("one", "yeah")) 
j <- tm_map(j, stripWhitespace)
#j <- tm_map(j, stemDocument)
inspect(j)
wordcloud(j, scale=c(3,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(6, "Dark2"))



# Part 3: R can get data online 
####################################################################

# Let's get infant mortality from the Unicef page: 
# https://data.unicef.org/topic/child-survival/under-five-mortality/
# Give R the URL, download the file: 
url <- "https://data.unicef.org/wp-content/uploads/2015/12/IMR_mortality_rate_2017.xls"
filepath <- "data/infmort.xls"
download.file(url, destfile="data/infmort.xls", mode = "wb")

# Read the file, and start working with it. 
infant <- read.xlsx2(filepath, sheetIndex = 1, check.names=TRUE, col.names=TRUE, startRow = 12)
head(infant)






# Part 2: Actually using R
####################################

## 2a: R thinks differently than Stata

## 2b: Some things are easier, but others are harder

## 2c: List of some advantages 

####Editor:             autocomplete; long variable names; etc. 
####Functionalities:    anything you dream of, and more




# Part 3: Where to learn more about R? 
#########################################

# take the JHU sequence
# esp. reproducible research

