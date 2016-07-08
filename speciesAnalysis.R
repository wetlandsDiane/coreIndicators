##################
#analysis to look at species-specific results for core indicator checklist for wildlife species
##################

###Read in data
setwd("U:\\GWP\\Wetland\\CoreIndicatorsGrant\\wildlifeIndicatorChecklists\\summarizedChecklistOutput")
wildlife=read.csv("wildlifeSummaries.csv")
assess=read.csv("canYouAssess.csv")

###Figure out number of unique responses for each taxa group
sumChecked=wildlife$Necessary+wildlife$Good+wildlife$Not.Important
numRecords=tapply(sumChecked, wildlife$dataType, max)

###look first at amphibian data
taxa="amphib"
num=numRecords[taxa]

taxaSub=wildlife[wildlife$dataType==taxa,]
topIndicators=which(taxaSub$Necessary+taxaSub$Good==num)
modIndicators=which(taxaSub$Necessary+taxaSub$Good>num/2 & taxaSub$Necessary+taxaSub$Good!=num)
taxaSub$rating=0
taxaSub$rating[topIndicators]=1
taxaSub$rating[modIndicators]=2

mergeAssess=merge(taxaSub, assess[c(1,2,4,5)], by="UniqueID")
out=mergeAssess[mergeAssess$rating>0,c(9, 3,8, 10, 11, 12)]
write.csv(out, "amphib.csv", row.names=FALSE)
