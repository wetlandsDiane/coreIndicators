##################
#analysis to look at species-specific results for core indicator checklist for wildlife species
##################
source("C:\\Users\\dmenuz\\Desktop\\gitRepo\\coreIndicators\\combiningInfoScript.R")
wd="U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\wildlifeChecklist\\csv"
cols=c("wildlife", "mollusks", "amphib",  "sagegrouse", "salmonids", "rapots", "songbirds")
outwd="U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\summarizedChecklistOutput\\"
combineFiles(wd, cols, outwd)

###Read in data
setwd("U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\summarizedChecklistOutput")
wildlife=read.csv("wildlifeSummaries.csv")
assess=read.csv("canYouAssess.csv")

###Figure out number of unique responses for each taxa group
sumChecked=wildlife$Necessary+wildlife$Good+wildlife$Not.Important
numRecords=tapply(sumChecked, wildlife$dataType, max)

###look at what variables folks think they can assess
#if more than 75% of respondents (removing Not. Eval.) say yes, then yes
#if >=50 to 75%, possibly, otherwise say no
#as of 7/13/2016, all indicators were assessed by at least 6 of 7 individuals
sumChecked=assess$No+assess$Too.time.consuming+assess$Yes
numAssess=max(sumChecked)
assess$negative=with(assess, Too.time.consuming+No)
assess$evalStatus=with(assess, ifelse(Yes>(numAssess-Not.Eval.)*.75, "Yes", ifelse(Yes>=(numAssess-Not.Eval.)*.5, "Possibly", 
    ifelse(negative>(numAssess-Not.Eval.)*.5, "No", "No data"))))

###Indicators are considered Not important if no one evaluated them or less than or equal to 25% of respondents did not say Necessary or Good
###or only one individual evaluated the data and it was not deemed good/necessary (might have been not eval.)
###for the ones below, respondents are all those that filled in an answer
###Indicators are considered necessary if over 75% of respondents said necessary and no one said not important
###indicators are considered good if over 50% of respondents said good/necessary (positive)
###if >25% to <50% of respondents said positive, then Uncertain; also uncertain if less than half of folks responded but some said something positive

wildlifeCalcs=function(taxa){
  num=numRecords[taxa]
  taxaSub=wildlife[wildlife$dataType==taxa & wildlife$UniqueID!=9,] #the 9 is to get rid of duplicated indicator
  positive=taxaSub$Necessary+taxaSub$Good
  maxValues=positive+taxaSub$Not.Important
  taxaMerge=merge(taxaSub, assess[c(1,9)], by="UniqueID")
  taxaMerge$habitatTrait="Unknown"
  taxaMerge$habitatTrait=ifelse(positive==0, "Not important",  #all without necessary/good checked will be assumed not important (even if skipped)
    ifelse(maxValues==0, "Not important", #not important if no data recorded by anyone for the indicator
    #next two lines look specifically at situations where indicator was rated by less than 50% of respondents
    ifelse(maxValues<0.5*num & taxaMerge$Not.Important/positive<=1, "Uncertain", #
    ifelse(maxValues<0.5*num & taxaMerge$Not.Important/positive>1, "Not important",#
    #remaining lines will look at situations where indicated rated by >=50% respondents and there are at least some positive ratings
    ifelse(taxaMerge$Necessary>(maxValues*.75) , "Necessary", #if over 75% of people who rated the indicator considered it necessary-> necessary
    ifelse(positive>(maxValues*0.50), "Good", #if more than or equal to half the respondents said something positive==Good
    ifelse(positive>(maxValues*0.25), "Uncertain", "Not important")))))))
  out=taxaMerge[, c("habitatTrait", "evalStatus", "category", "indicator")]
  out=out[out$category!="",]
  sortTrait=ifelse(out$habitatTrait=="Necessary", 1, ifelse(out$habitatTrait=="Good", 2, 
    ifelse(out$habitatTrait=="Uncertain", 3, 4)))
  sortEval=ifelse(out$evalStatus=="Yes", 1, ifelse(out$evalStatus=="Maybe", 2, 
    ifelse(out$evalStatus=="No", 3, 4)))
  out2=out[order(sortTrait, sortEval),]
  out2$group=rep(taxa, nrow(out2))
  return(out2)
}
amphib=wildlifeCalcs("amphib")
raptors=wildlifeCalcs("rapots")
salmonid=wildlifeCalcs("salmonids")
songbirds=wildlifeCalcs("songbirds")
sagegrouse=wildlifeCalcs("sagegrouse")
mollusks=wildlifeCalcs("mollusks")

###Look at wildlife in general, incl. both ratings for general wildlife and number of times each trait shows up for each taxa group
generalWildlife=wildlifeCalcs("wildlife")
write.csv(generalWildlife[generalWildlife$habitatTrait!="Not important",], "generalWildlife.csv", row.names=FALSE)

comboWildlife=rbind(amphib, raptors, salmonid, songbirds, sagegrouse, mollusks)
cw2=comboWildlife[comboWildlife$habitatTrait %in% c("Necessary", "Good"),] #extract out only those that are necessary or good
cw3=data.frame(table(cw2$indicator))
colnames(cw3)=c("indicator", "TaxaFreq")
cw4=merge(generalWildlife, cw3, by="indicator")
sortTrait=ifelse(cw4$habitatTrait=="Necessary", 4, ifelse(cw4$habitatTrait=="Good", 3, 
  ifelse(cw4$habitatTrait=="Uncertain", 2, 1)))
sortEval=ifelse(cw4$evalStatus=="Yes", 4, ifelse(cw4$evalStatus=="Maybe", 3, 
  ifelse(cw4$evalStatus=="No", 2, 1)))
cw5=cw4
colnames(cw5)[2]="genWildlife"

#combining all of the files into a final output file
m1=merge(cw5, amphib[,c("habitatTrait", "indicator")], by="indicator")
colnames(m1)[ncol(m1)]="amphib"
m2=merge(m1, mollusks[,c("habitatTrait", "indicator")], by="indicator")
colnames(m2)[ncol(m2)]="mollusks"
m3=merge(m2, raptors[,c("habitatTrait", "indicator")], by="indicator")
colnames(m3)[ncol(m3)]="raptors"
m4=merge(m3, sagegrouse[,c("habitatTrait", "indicator")], by="indicator")
colnames(m4)[ncol(m4)]="sagegrouse"
m5=merge(m4, salmonid[,c("habitatTrait", "indicator")], by="indicator")
colnames(m5)[ncol(m5)]="salmonids"
m6=merge(m5, songbirds[,c("habitatTrait", "indicator")], by="indicator")
colnames(m6)[ncol(m6)]="songbirds"
wildlifeUnique=unique(wildlife[,c("UniqueID", "indicator")])
m7=merge(m6, wildlifeUnique, by="indicator")
m7$UniqueID=as.numeric(m7$UniqueID)
orderM7=m7[order(m7$UniqueID),c("UniqueID","category", "evalStatus", "indicator", "genWildlife", "TaxaFreq", 
    "amphib", "mollusks", "raptors", "sagegrouse", "salmonids", "songbirds")]



write.csv(orderM7, "finalWildlifeAnalysis.csv", row.names=FALSE)

