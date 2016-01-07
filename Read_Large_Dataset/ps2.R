#read the table into R without unzipping
con = file("/Users/Xinyue_star/src/stat243/ps2/ss13hus.csv.bz2.1", "r")
close(con)
##first select targeted columns
target = c("ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP","FPARC", "HHL", "NOC", "MV", "VEH", "YBL")
blockSize = 72190
nLines = 7219001
numSam = 10000
blockSize = 10
nLines = 30
numSam = 10
#####Helper function 1, select columns
selColTF = function(target,con){
  firstline = read.csv(con, nrows=1)
  judge = is.element(colnames(firstline), target)
  return(judge)
}
#judge = selColTF(target,con)

selCol = function(target,con){
  judge = selColTF(target,con)
  filterit = character(length(judge))
  for (i in 1:length(judge))
  {
    if (judge[i]) filterit[i] = "numeric"
    else filterit[i] = "NULL" 
  }
  return (filterit)
}
##Helper Func 1.1


#####function 2, sample, get logical val of sampled index
sampleInd = function(pop, numSam){
  set.seed(0)
  mysample=sample(1:pop, numSam)
  sampleindex=is.element(c(1:pop), mysample) 
  return (sampleindex)
}
#



##########funtion 3, read chunks
buildChunks = function(blockSize, pop, numSam,target){
  sampleindex = sampleInd(pop, numSam)
  filterit = selCol(target, con)
  mydata = data.frame(matrix(NA, nrow = numSam, ncol=length(target)))
  lines=1
  sta = 1
  for(i in 1:ceiling(pop/blockSize)){
    print(i)
    end =sta + sum(sampleindex[lines:min(pop, (lines+blockSize-1))])-1 
    if (end>sta){
      mydata[sta:end,] = 
        read.csv(con, nrow = blockSize, colClasses = filterit)[sampleindex[lines:min(pop, (lines+blockSize-1))],]
    }
    sta = end+1
    lines = lines + blockSize
  }
  colnames(mydata) = target
  return (mydata)
}
mydataCSV = buildChunks(blockSize,nLines, numSam,target)
close(con)

##b
#SELECT COL
#judge = selColTF(target,con)
#SELECT ROW
#sampleindex = sampleInd(nLines, numSam)
#BUILD THE CHUNKS
buildChunksRL= function(blockSize, pop, numSam,target){
  judge = selColTF(target,con)
  sampleindex = sampleInd(pop, numSam)
  mydataRL = data.frame(matrix(NA, nrow = numSam, ncol=length(target)))
  STA = 1
  lines = 1
  for(i in 1:ceiling(pop / blockSize)){
    print(i)
    END  = STA + sum(sampleindex[lines:min(pop, (lines+blockSize-1))])-1 
    if (END>=STA){
      mydata2 = readLines(con, n = blockSize)[sampleindex[lines:min(pop,lines+blockSize-1)]]
      mydata2.1 = str_split(mydata2,",")
      mydata2.2 = matrix(unlist(mydata2.1), byrow = TRUE, ncol=length(judge))[,judge]
      mydataRL[STA:END,] = mydata2.2
    }
    STA = END+1
    lines = lines +blockSize
  }
  mydataRL = data.frame(data.matrix(mydataRL))
  return(mydataRL)
}

mydataRL = buildChunksRL(blockSize, nLines, numSam,target)
##compare the time of read.csv and read.Lines. Just for several Chunks
blockSize1 =10000
nLines1 =100000
numSam1 = 1000
close(con)
con = file("/Users/Xinyue_star/src/stat243/ps2/ss13hus.csv.bz2.1", "r")
system.time(mydataRL.test <- buildChunksRL(blockSize1, nLines1, numSam1,target))
system.time(mydataCSV.text <- buildChunks(blockSize1,nLines1, numSam1,target))
##the performance of readLines() is better in this way.

#c
##############
con = file("/Users/Xinyue_star/src/stat243/ps2/ss13hus.csv.bz2.1", "r")
judge = selColTF(target,con)
indx = which(judge =="TRUE")
write(indx,file = "preprocessing.txt",ncolumns=length(target), sep=",")
close(con)
##############

#then preprocess data in bash
#read csv in chunks
con = file("pre_data.txt", "r")
desert = read.csv(con, nrow = 1,sep=",")

buildChunksPre = function(blockSize, pop, numSam,target){
  sampleindex = sampleInd(pop,numSam)
  mydataPre = data.frame(matrix(NA, nrow = numSam, ncol=length(target)))
  lines=1
  sta = 1
  #######
  for(i in 1:ceiling(pop/blockSize)){
    print(i)
    end =sta + sum(sampleindex[lines:min(pop, (lines+blockSize-1))])-1 
    if (end>sta){
      mydataPre[sta:end,] = 
        read.csv(con, nrow = blockSize,sep=",")[sampleindex[lines:min(pop, (lines+blockSize-1))],]
    }
    sta = end+1
    lines = lines + blockSize
  }
  colnames(mydataPre) = target
  return(mydataPre)
}
system.time(mydataPre <- buildChunksPre(blockSize, nLines, numSam,target))

#d
cor = cor(mydataPre,use="complete.obs")
#draw pic
library(lattice)
#Build the horizontal and vertical axis information
hor <- target
ver <- target
#Build the fake correlation matrix
nrowcol <- length(ver)
cor = cor(mydataPre,use="complete.obs")
#Build the plot
rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
levelplot(cor, main="stage 12-14 array correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(-1,1,0.05))




