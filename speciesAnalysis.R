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
###maybe need to do something so I don't assess attributes if less than 50% of that attribute was evaluated- or those go only to uncertain category??
sumChecked=assess$No+assess$Too.time.consuming+assess$Yes
numAssess=max(sumChecked)
assess$negative=with(assess, Too.time.consuming+No)
assess$evalStatus=with(assess, ifelse(Yes>(numAssess-Not.Eval.)*.75, "Yes", ifelse(Yes>=(numAssess-Not.Eval.)*.5, "Possibly", 
    ifelse(negative>(numAssess-Not.Eval.)*.5, "No", "No data"))))

###if greater than 75% of respondents call something necessary and none say not important, than necessary
###if greater than 50% of respondents call something good or necessary, than make it good
###if greater than 25% say something, call it uncertain (unless only one respondent, than call it no)
wildlifeCalcs=function(taxa){
  num=numRecords[taxa]
  taxaSub=wildlife[wildlife$dataType==taxa & wildlife$UniqueID!=9,] #the 9 is to get rid of duplicated indicator
  positive=taxaSub$Necessary+taxaSub$Good
  total=positive+taxaSub$Not.Important
  missing=num-total
  maxValues=num-missing
  taxaMerge=merge(taxaSub, assess[c(1,9)], by="UniqueID")
  taxaMerge$habitatTrait=ifelse(maxValues==0, "Not important",
    ifelse(taxaMerge$Necessary>(maxValues*.75) & taxaMerge$Not.Important==0, "Necessary", 
    ifelse(positive>(maxValues*.5), "Good", ifelse(num==1 & positive==0, "Not important",
    ifelse(positive>(maxValues*.25), "Uncertain", "Not important")))))
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
write.csv(amphib[amphib$habitatTrait!="Not important",], "amphib.csv", row.names=FALSE)
raptors=wildlifeCalcs("rapots")
write.csv(raptors[raptors$habitatTrait!="Not important",], "raptors.csv", row.names=FALSE)
salmonid=wildlifeCalcs("salmonids")
write.csv(salmonid[salmonid$habitatTrait!="Not important",], "salmonids.csv", row.names=FALSE)
songbirds=wildlifeCalcs("songbirds")
write.csv(songbirds[songbirds$habitatTrait!="Not important",], "songbirds.csv", row.names=FALSE)
sagegrouse=wildlifeCalcs("sagegrouse")
write.csv(sagegrouse[sagegrouse$habitatTrait!="Not important",], "sagegrouse.csv", row.names=FALSE)
mollusks=wildlifeCalcs("mollusks")
write.csv(mollusks[mollusks$habitatTrait!="Not important",], "mollusks.csv", row.names=FALSE)

###Look at wildlife in general traits
generalWildlife=wildlifeCalcs("wildlife")
comboWildlife=rbind(amphib, raptors, salmonid, songbirds, sagegrouse, mollusks)
cw2=comboWildlife[comboWildlife$habitatTrait %in% c("Necessary", "Good"),]
cw3=data.frame(table(cw2$indicator))
cw4=merge(generalWildlife, cw3, by.x="indicator", by.y="Var1")
sortTrait=ifelse(cw4$habitatTrait=="Necessary", 1, ifelse(cw4$habitatTrait=="Good", 2, 
  ifelse(cw4$habitatTrait=="Uncertain", 3, 4)))
sortEval=ifelse(cw4$evalStatus=="Yes", 1, ifelse(cw4$evalStatus=="Maybe", 2, 
  ifelse(cw4$evalStatus=="No", 3, 4)))
cw5=cw4[order(sortTrait, sortEval),]

write.csv(cw5, "wildlife.csv", row.names=FALSE)
