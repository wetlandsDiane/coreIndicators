setwd("U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\wildlifeChecklist\\csv")

#combine all files into one
fileList=list.files()
dat1=read.csv(fileList[1], header=TRUE, skip=2)
for (i in 2:length(fileList)){
  datNew=read.csv(fileList[i], header=TRUE, skip=2)
  dat1=rbind(dat1, datNew[,1:22])
}
finalDat=dat1



#make a dataset with all of the columns combined together
cols=c("assess", "wildlife", "mollusks", "amphib", "waterfowl", "shorebirds", "wading", "piscivorous", "beaver", "sagegrouse", "salmonids", "rapots", "songbirds", "mammals", "muskrats")
cols=c("wildlife", "mollusks", "amphib",  "sagegrouse", "salmonids", "rapots", "songbirds")

resultsFrame=as.data.frame.matrix(table(finalDat$UniqueID, finalDat[,cols[1]]))
resultsFrame$dataType="test"
resultsFrame$UniqueID=row.names(resultsFrame)
outResultsFrame=resultsFrame

for (i in 2:length(cols)){
  resultsFrameTemp=as.data.frame.matrix(table(finalDat$UniqueID, finalDat[,cols[i]]))
  resultsFrameTemp$dataType=cols[i]
  resultsFrameTemp$UniqueID=row.names(resultsFrameTemp)
  outResultsFrame=rbind(resultsFrameTemp, outResultsFrame)
}

mergeDat=read.csv(fileList[1], header=TRUE, skip=2)
outResults=merge(outResultsFrame, mergeDat[,c("UniqueID", "category", "indicator")], by="UniqueID")
head(outResults)
dim(outResults)
setwd("U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\summarizedChecklistOutput\\")

write.csv(outResults, "wildlifeSummaries.csv")


assess1=as.data.frame.matrix(table(finalDat$UniqueID, finalDat$assess))
assess1$UniqueID=row.names(assess1)
assess2=merge(assess1, mergeDat[,c("UniqueID", "category", "indicator")], by="UniqueID")
head(assess2)
dim(assess2)
write.csv(assess2, "canYouAssess.csv")


