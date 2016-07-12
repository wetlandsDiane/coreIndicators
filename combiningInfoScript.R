#############################
#Code for combining separate files from wildlife biologist into two final files
#one for taxon-specific info and one for whether or not user can assess the trait
#############################

combineFiles=function(wd, cols, outwd){
  setwd(wd)
  #combine all files into one
  fileList=list.files()
  dat1=read.csv(fileList[1], header=TRUE, skip=2)
  for (i in 2:length(fileList)){
    datNew=read.csv(fileList[i], header=TRUE, skip=2)
    dat1=rbind(dat1, datNew[,1:22])
  }
  finalDat=dat1
  resultsFrame=as.data.frame.matrix(table(finalDat$UniqueID, finalDat[,cols[1]]))
  resultsFrame$dataType="wildlife"
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
  outResults=outResults[,-which(colnames(outResults)=="V1")] #this column is meaningless
  head(outResults)
  dim(outResults)
  finalResults=outResults[,c("UniqueID", "dataType", "category", "Necessary", "Good", "Not Important", "Not Eval.", "indicator")]
  setwd(outwd)
  write.csv(finalResults,row.names=FALSE, "wildlifeSummaries.csv")
  assess1=as.data.frame.matrix(table(finalDat$UniqueID, finalDat$assess))
  assess1$UniqueID=row.names(assess1)
  assess2=merge(assess1, mergeDat[,c("UniqueID", "category", "indicator")], by="UniqueID")
  assess2=assess2[,-which(colnames(assess2)=="V1")]
  write.csv(assess2, row.names=FALSE,"canYouAssess.csv")
}
  
  
  