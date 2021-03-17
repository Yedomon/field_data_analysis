## take a cross object from r/qtl and produce linkage map
## on chr 1,4,6,15
getwd()
library(qtl)
data(hyper)
outfile = file.path("C:/Users/user/Documents", "hyper.pdf")
lmv.linkage.plot(hyper,outfile,mapthese=c(1,4,6,15))

## color some of the markers for emphasis

library(qtl)
data(hyper)

# make a list to pass label options
flist <- list()
locus <- c("D1Mit123","D1Mit105","D6Mit273","D15Mit56","D15Mit156")
col   <- c("red")
flist[[1]] <- list(locus=locus,col=col)

outfile = file.path("C:/Users/user/Documents", "hyperred.pdf")
lmv.linkage.plot(hyper,outfile,mapthese=c(1,4,6,15),markerformatlist=flist)

## change some of the pdf options and chromosome color
## changing linkage group title color (col.lgtitle) to same as
## foreground pdf color

library(qtl)
data(hyper)

outfile = file.path("C:/Users/user/Documents", "hyperlg.pdf")
lmv.linkage.plot(hyper,outfile,
                 mapthese=c(1,4,6,15),
                 pdf.bg="black",pdf.fg="white",col.lgtitle="white",
                 pdf.height=8,pdf.title="myhyper",lg.col="tan")

## change all label colors and fonts

library(qtl)
data(hyper)

outfile = file.path("C:/Users/user/Documents", "hypercol.pdf")
lmv.linkage.plot(hyper,outfile,mapthese=c(1,4,6,15),
                 lcol="blue",lfont=2,lcex=1.2,rcol="red",rfont=3,rcex=2)

## make a dataframe to pass sections of chr to col
## use a ruler instead of printing positions as labels
## only allow one column for duplicate markers at same position
## (default is 3)

library(qtl)
data(hyper)

chr = c(1, 4, 6, 15)
s = c(82,35,9.8,7.7)
e = c(94,47,21.9,13.1)
col = c("pink","blue","blue","green")
sectcoldf <-  data.frame(chr, s, e, col,stringsAsFactors = FALSE)

outfile = file.path("C:/Users/user/Documents", "hyperruler.pdf")
lmv.linkage.plot(hyper,outfile,mapthese=c(1,4,6,15),
                 ruler=TRUE,maxnbrcolsfordups = 1, sectcoldf=sectcoldf)

## plot qtls also out of a r/qtl scanone object
## plot marker names on left (instead of right) of chr 4 and 7

library(qtl)
data(hyper)

# create scanone df for testing
hyper <-
  calc.genoprob(hyper,
                step = 2.0,
                map.function = "haldane",
                stepwidth = "fixed")
hyper.scanone <- scanone(hyper)

outfile = file.path("C:/Users/user/Documents", "testrqtlhyper2.pdf")
lmv.linkage.plot(hyper,
                 outfile, mapthese=c(1,4,6,7,15),
                 qtlscanone = hyper.scanone,
                 posonleft = c(TRUE,FALSE,TRUE,FALSE,TRUE))

## Not run: 
## plot a carrot comparative linkage map
## kindly provided by Massimo Iorizzo:
## Cavagnaro et al. BMC Genomics 2014, 15:1118

# make a df to pass qtl info
qtldf <- data.frame(
  chr = character(),
  qtl = character(),
  so = numeric(),
  si = numeric(),
  ei = numeric(),
  eo = numeric(),
  col = character(),
  stringsAsFactors = FALSE
)
qtldf <- rbind(qtldf,
               data.frame(
                 chr = "70349LG3",
                 qtl = "RTPE-Q1",
                 so = 36.6,
                 si = 37,
                 ei = 37,
                 eo = 38,
                 col="red"
               ))
# make a list to pass label options
flist <- list()
locus <- c("BSSR-094", "K0149", "K0627", "K2161", "ESSR-087", "ESSR-057")
font  <- c(2)   #bold
flist[[1]] <- list(locus = locus, font = font)
locus <- c("F3H", "FLS1")
font  <- c(4)   #bold italic
flist[[2]] <- list(locus = locus, font = font)
locus <- c("P3", "P1", "Raa1")
font  <- c(3)   #italic
col <- c("red")
flist[[3]] <- list(locus = locus, font = font, col = col)
filename <- system.file("extdata", "Carrot.csv", package="LinkageMapView")
outfile = file.path("C:/Users/user/Documents", "carrot.pdf")
lmv.linkage.plot(
  mapthis = filename,
  outfile = outfile,
  ruler = TRUE,
  lgtitle = c("2170", "70349", "10117"),
  maxnbrcolsfordups = 1,
  markerformatlist = flist,
  lg.col = "lightblue1",
  pdf.width =10,
  revthese = c("70349LG3"),
  qtldf=qtldf
)

## End(Not run)

## do a density map with default colors
data(oat)

outfile = file.path("C:/Users/user/Documents", "oat_Mrg01.pdf")
lmv.linkage.plot(oat,outfile,mapthese=c("Mrg01","Mrg02"),denmap=TRUE)

## Not run: 
## do a density map and provide your own colors with lmvdencolor helper
data(oat)
##
outfile = file.path("C:/Users/user/Documents", "oat_Mrg01_YlGn.pdf")

sectcoldf <- lmvdencolor(oat,colorin =
                           colorRampPalette(RColorBrewer::brewer.pal(8, "YlGn"))(5))

lmv.linkage.plot(oat,outfile,denmap=TRUE,sectcoldf=sectcoldf)

## End(Not run)

View(oat)
