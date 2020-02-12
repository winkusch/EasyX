setClass("MULTINDEP",
	representation = representation(
						strEqcCommand		=	"character",
						arcdCriterion		=	"character",
						acolMIndep			=	"character",
						astrMIndepTag		=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						arcdCriterion		=	"",
						acolMIndep			=	"",
						astrMIndepTag		=	""
						),
	contains = c("INDEP")
)

setGeneric("setMULTINDEP", function(object) standardGeneric("setMULTINDEP"))
setMethod("setMULTINDEP", signature = (object = "MULTINDEP"), function(object) {
	
	aEqcSlotNamesIn = c("arcdCriterion","acolMIndep","astrMIndepTag","blnIndepMin","colInChr","colInPos","numPosLim","blnAddIndepInfo","strTag","blnStepDown","filePrio","colPrioMarker","colInMarker","blnRecombRate","fileRecombRate","numRecombRateLim","colRecombRateChr","colRecombRatePos","colRecombRateRate","blnRecombRateMaximize","blnRecombRatePosLim")
	
	
	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	
	return(object)
})

#############################################################################################################################
validMULTINDEP <- function(objMULTINDEP) {
	
	### Valid with GWADATA?
	
	if(objMULTINDEP@arcdCriterion[1] == "") 
		cat(paste(" EASY WARNING:MULTINDEP\n No criterion arcdCriterion defined. All data will be used for independentisation.", sep=""))
	if(objMULTINDEP@acolMIndep[1] == "") 
		stop(paste(" EASY ERROR:MULTINDEP\n No column acolMIndep defined. Please set acolMIndep.", sep=""))
	
	
	## NO UNDERSCORES IN TAGS!
	
	return(TRUE)
}
MULTINDEP.GWADATA.valid <- function(objMULTINDEP, objGWA) {
	
	aisMatch = objMULTINDEP@acolMIndep %in% objGWA@aHeader
	if(any(!aisMatch))
		stop(paste("EASY ERROR:MULTINDEP\n Column \n",objMULTINDEP@acolMIndep[which(!aisMatch)[1]]," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
}

#############################################################################################################################
MULTINDEP.run <- function(objMULTINDEP, objGWA, objREPORT, isValidScript) {
		
	acolMIndep		<- objMULTINDEP@acolMIndep
	astrMIndepTag	<- objMULTINDEP@astrMIndepTag
	strTagMultiIndep <- objMULTINDEP@strTag  ## Tag for multi locus clumping
	blnAddIndepInfo <- objMULTINDEP@blnAddIndepInfo
	numPosLim <- objMULTINDEP@numPosLim
	colInChr <- objMULTINDEP@colInChr
	colInPos <- objMULTINDEP@colInPos
	blnRecombRate <- objMULTINDEP@blnRecombRate
	
	# BMI.aLociTag, BMI.aTopHit, BMI.aNumLocusSNPs
	# WHR.aLociTag, WHR.aTopHit, WHR.aNumLocusSNPs
	# WHRADJ.aLociTag, WHRADJ.aTopHit, WHRADJ.aNumLocusSNPs
	
	## compile multiLociTag
	## compile multiTopHit
	
	### reduce objGWA
	aisUse = FALSE
	for(strTagTmp in astrMIndepTag) aisUse = aisUse | !is.na(GWADATA.getcol(objGWA, paste(strTagTmp,"aLociTag",sep=".")))
		
	if(isValidScript & any(aisUse)) {
	
		objGWA.indep <- GWADATA.getrows(objGWA, which(aisUse))
		
		if(blnRecombRate) {
			aCrossTraitRRCoordinates <- rep(NA, nrow(objGWA.indep@tblGWA))
		}
		
		## sort table by minimum P
		
		df = as.data.frame(objGWA.indep@tblGWA)
		aMinPval = apply(df[,acolMIndep],1,min,na.rm=T)
		aidxMin = apply(df[,acolMIndep],1,which.min)
		
		df <- df[order(aMinPval),]
		aidxMin <- aidxMin[order(aMinPval)]
		
		achr = df[,colInChr]
		apos = df[,colInPos]
		
		alc <- arc <- rep(NA,length(aidxMin))
		for(k in 1:length(aidxMin)) {
			curt = astrMIndepTag[aidxMin[k]]
			alc[k] = df[k,paste(curt,".aLocusCoordinates",sep="")]
			arc[k] = df[k,paste(curt,".aRRCoordinates",sep="")]
		}
		### these are the coordinates of the min Pval across traits
		alpos = unlist(lapply(strsplit(alc,":",fixed=T),function(x) x[2]))
		alpos1 = unlist(lapply(strsplit(alpos,"_",fixed=T),function(x) x[1]))
		alpos1 = as.integer(alpos1)
		alpos2 = unlist(lapply(strsplit(alpos,"_",fixed=T),function(x) x[2]))
		alpos2 = gsub("EoCHR","Inf",alpos2)
		alpos2 = as.numeric(alpos2)
		
		arpos = unlist(lapply(strsplit(arc,":",fixed=T),function(x) x[2]))
		arpos1 = unlist(lapply(strsplit(arpos,"_",fixed=T),function(x) x[1]))
		arpos1 = as.integer(arpos1)
		arpos2 = unlist(lapply(strsplit(arpos,"_",fixed=T),function(x) x[2]))
		arpos2 = gsub("EoCHR","Inf",arpos2)
		arpos2 = as.numeric(arpos2)
		
		### some aRRcoordinates may overlap!; note they arre sorted by p
		### eg 
		### 1:100_200
		### 1:200_300
		### -> increase arpos1 == 200 by 1; to add the maxvar to the stronger signal
		
		for(i in 1:length(arpos1)) {
			chri = achr[i]
			rpos1i = arpos1[i]
			isIncrease = achr == chri & arpos2 == rpos1i
			if(any(isIncrease)) {
				arpos2[isIncrease] = arpos2[isIncrease] - 1
			}

			rpos2i = arpos2[i]
			isIncrease = achr == chri & arpos1 == rpos2i
			if(any(isIncrease)) {
				arpos1[isIncrease] = arpos1[isIncrease] + 1
			}
		}
				
		acurposmin = pmax(alpos1-numPosLim, arpos1)
		acurposmax = pmin(alpos2+numPosLim, arpos2)
		
		aCrossTraitLocusId <- aTraitTopHit <- rep(NA, nrow(objGWA.indep@tblGWA))
		
		for(i in 1:nrow(df)) {
			
			if(i/1000==round(i/1000)) print(paste(i,nrow(df),sep=" of "))
			
			if(is.na(aCrossTraitLocusId[i])) {
				
				if(blnRecombRate) {
					
					# curt = astrMIndepTag[aidxMin[i]]
										
					# ###################### EXTEND DEF TO OTHER TRAITS !!!!!
					# # based cys.aLocusCoordinates !!!!! sjould work because overlaps will be overwrittem !
					# lc = df[i,paste(curt,".aLocusCoordinates",sep="")]
					# # lchr = strsplit(lc,":",fixed=T)[[1]][1]
					# lpos = strsplit(lc,":",fixed=T)[[1]][2]
					# lpos1 = strsplit(lpos,"_",fixed=T)[[1]][1]
					# lpos2 = strsplit(lpos,"_",fixed=T)[[1]][2]
					# lpos2 = gsub("EoCHR","Inf",lpos2)
					# lpos1 = as.integer(lpos1)
					# lpos2 = as.numeric(lpos2)

					# rc = df[i,paste(curt,".aRRCoordinates",sep="")]
					# ## rchr = strsplit(rc,":",fixed=T)[[1]][1]
					# rpos = strsplit(rc,":",fixed=T)[[1]][2]
					# rpos1 = strsplit(rpos,"_",fixed=T)[[1]][1]
					# rpos2 = strsplit(rpos,"_",fixed=T)[[1]][2]
					# rpos2 = gsub("EoCHR","Inf",rpos2)
					# rpos1 = as.integer(rpos1)
					# rpos2 = as.numeric(rpos2)
					
					### Viewed from the variant: 
					# curposmin = max(df[i,colInPos]-numPosLim, rpos1)
					# curposmax = min(df[i,colInPos]+numPosLim, rpos2)
					
					### Viewed from the locus: 
					# curposmin = max(alpos1[i]-numPosLim, arpos1[i])
					# curposmax = min(alpos2[i]+numPosLim, arpos2[i])
					
					isLocus = df[,colInChr] == achr[i] & df[,colInPos] >= acurposmin[i] & df[,colInPos] <= acurposmax[i]
					# isLocus = df[,paste(curt,".aLociTag",sep="")] == curtid
					
				} else {
					isLocus = df[,colInChr]==achr[i] & abs(df[,colInPos]-apos[i]) <= numPosLim
				}
				
				isCrossTraitListed = isLocus & !is.na(aCrossTraitLocusId)
				if(any(isCrossTraitListed)) {
					# get cross trait ID
					anumcrosstraitid = unique(aCrossTraitLocusId[isCrossTraitListed])
					if(length(anumcrosstraitid)==1) {
						newCtId = anumcrosstraitid
						aCrossTraitLocusId[i] = newCtId
					} else if(length(anumcrosstraitid)==2) {
						newCtId = max(aCrossTraitLocusId,na.rm=T) + 1
						# combine locus IDs
						aCrossTraitLocusId[i] = newCtId
						# reset previous 1
						isCt1 = aCrossTraitLocusId == anumcrosstraitid[1]
						aCrossTraitLocusId[isCt1] = newCtId
						# reset previous 2
						isCt2 = aCrossTraitLocusId == anumcrosstraitid[2]
						aCrossTraitLocusId[isCt2] = newCtId
					} else {
						stop("MULTINDEP: Combination problem.")
					}
				} else {
					# set new cross trait id
					if(i==1) {
						newCtId = 1
					} else {
						newCtId = max(aCrossTraitLocusId,na.rm=T) + 1
					}
					aCrossTraitLocusId[i] = newCtId
				}
			}
			
			### set trait top hit column
			strTopHitTag = NA
			for(strMIndepTag in astrMIndepTag) {
				isTopHit = !is.na(df[i,paste(strMIndepTag,".aTopHit",sep="")])
				if(isTopHit) {
					if(is.na(strTopHitTag)) strTopHitTag <- strMIndepTag
					else strTopHitTag <- paste(strTopHitTag,strMIndepTag,sep=";")
				}
			}
			aTraitTopHit[i] = strTopHitTag
			
		}
		
		## set cross trait locus tag and cross trait top hit
		ctids = unique(aCrossTraitLocusId)
		aMinPvalOrdered = apply(df[,acolMIndep],1,min,na.rm=T)
		
		##############
		
		aCrossTraitLocusTag <- aCrossTraitTopHit <- aCrossTraitLocusCoordinates <-  rep(NA, nrow(objGWA.indep@tblGWA))
			
		for(curct in ctids) {
			
			isCurCtLocus = aCrossTraitLocusId == curct
			strLocusTag = NA
			chrj <- pos1j <- pos2j <- NA
			arc = c()
			
			for(strMIndepTag in astrMIndepTag) {
				isHit = any(!is.na(df[isCurCtLocus,paste(strMIndepTag,".aLociTag",sep="")]))
				if(isHit) {
					if(is.na(strLocusTag)) strLocusTag <- strMIndepTag
					else strLocusTag <- paste(strLocusTag,strMIndepTag,sep=";")
					
					alc = df[isCurCtLocus,paste(strMIndepTag,".aLocusCoordinates",sep="")]
					alchr = unlist(lapply(strsplit(alc,":",fixed=T),function(x) x[1]))
					alpos = unlist(lapply(strsplit(alc,":",fixed=T),function(x) x[2]))
					alpos1 = unlist(lapply(strsplit(alpos,"_",fixed=T),function(x) x[1]))
					alpos1 = as.integer(alpos1)
					alpos2 = unlist(lapply(strsplit(alpos,"_",fixed=T),function(x) x[2]))
					alpos2 = gsub("EoCHR","Inf",alpos2)
					alpos2 = as.numeric(alpos2)
					
					chrj = as.character(na.omit(unique(alchr)))
					pos1j = min(pos1j,min(alpos1,na.rm=T),na.rm=T)
					pos2j = max(pos2j,max(alpos2,na.rm=T),na.rm=T)
					
					if(blnRecombRate) {
						arc = c(arc, df[isCurCtLocus,paste(strMIndepTag,".aRRCoordinates",sep="")])
						arcuni = unique(df[isCurCtLocus,paste(strMIndepTag,".aRRCoordinates",sep="")])
						arc = c(arc, arcuni)
						arc = unique(arc)
					}
				}
			}
			aCrossTraitLocusTag[isCurCtLocus] = strLocusTag
			
			aCrossTraitLocusCoordinates[isCurCtLocus] = paste(chrj,":",pos1j,"_",pos2j,sep="")
			
			if(blnRecombRate) {
				# arcuni = unique(arc)
				# if(length(arcuni)>1) stop("RRcoord mismatch.")
				# aCrossTraitRRCoordinates[isCurCtLocus] = arcuni[1]
				arc = arc[!is.na(arc)]
				
				aCrossTraitRRCoordinates[isCurCtLocus] = paste(arc,collapse=";")
			}
			
			idxCurLocus = which(isCurCtLocus)
			idxCurLocusMinP = idxCurLocus[which.min(aMinPvalOrdered[idxCurLocus])]
			aCrossTraitTopHit[idxCurLocusMinP] = strLocusTag
			
		}
		
		######
			
		objGWA.indep@tblGWA <- as.data.table(df)
		
		## add compiled vals to objGWA
		strTagMultiIndep <- ifelse(strTagMultiIndep != "", paste(strTagMultiIndep,".",sep=""), strTagMultiIndep)
		
		if(paste(strTagMultiIndep, "aCrossTraitLocusId",sep="") %in% objGWA@aHeader)
			stop("EASY ERROR:MULTINDEP\nAdding column aCrossTraitLocusId to large data-set failed because column\n",paste(strTagMultiIndep,"aCrossTraitLocusId",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTagMultiIndep, "aCrossTraitTopHit",sep="") %in% objGWA@aHeader)
			stop("EASY ERROR:MULTINDEP\nAdding column aCrossTraitTopHit to large data-set failed because column\n",paste(strTagMultiIndep,"aCrossTraitTopHit",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTagMultiIndep, "aCrossTraitLocusTag",sep="") %in% objGWA@aHeader)
			stop("EASY ERROR:MULTINDEP\nAdding column aCrossTraitLocusTag to large data-set failed because column\n",paste(strTagMultiIndep,"aCrossTraitLocusTag",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTagMultiIndep, "aTraitTopHit",sep="") %in% objGWA@aHeader)
			stop("EASY ERROR:MULTINDEP\nAdding column aTraitTopHit to large data-set failed because column\n",paste(strTagMultiIndep,"aTraitTopHit",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTagMultiIndep, "aCrossTraitLocusCoordinates",sep="") %in% objGWA@aHeader)
			stop("EASY ERROR:MULTINDEP\nAdding column aCrossTraitLocusCoordinates to large data-set failed because column\n",paste(strTagMultiIndep,"aCrossTraitLocusCoordinates",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		
		if(blnRecombRate) {
			if(paste(strTagMultiIndep, "aCrossTraitRRCoordinates",sep="") %in% objGWA@aHeader)
				stop("EASY ERROR:MULTINDEP\nAdding column aCrossTraitRRCoordinates to large data-set failed because column\n",paste(strTagMultiIndep,"aCrossTraitRRCoordinates",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		}
		
		
		objGWA.indep <- GWADATA.cbind(objGWA.indep, aCrossTraitLocusId, paste(strTagMultiIndep, "aCrossTraitLocusId",sep=""))
		objGWA.indep <- GWADATA.cbind(objGWA.indep, aCrossTraitTopHit, paste(strTagMultiIndep, "aCrossTraitTopHit",sep=""))
		objGWA.indep <- GWADATA.cbind(objGWA.indep, aCrossTraitLocusTag, paste(strTagMultiIndep, "aCrossTraitLocusTag",sep=""))
		objGWA.indep <- GWADATA.cbind(objGWA.indep, aTraitTopHit,paste(strTagMultiIndep, "aTraitTopHit",sep=""))
		objGWA.indep <- GWADATA.cbind(objGWA.indep, aCrossTraitLocusCoordinates,paste(strTagMultiIndep, "aCrossTraitLocusCoordinates",sep=""))
		if(blnRecombRate) objGWA.indep <- GWADATA.cbind(objGWA.indep, aCrossTraitRRCoordinates,paste(strTagMultiIndep, "aCrossTraitRRCoordinates",sep=""))
		
		# objGWA.indep <- GWADATA.cbind(objGWA.indep, aTraitLocusTag,paste(strTagMultiIndep, "aTraitLocusTag",sep=""))
		
		if(blnAddIndepInfo) {
			### merges by intersect(names)		
			objGWA <- GWADATA.merge(objGWA,objGWA.indep, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
		}	
		
		objGWA.indep.x.traittophits <- GWADATA.getrows(objGWA.indep, which(!is.na(aTraitTopHit)))
		objGWA.indep.x.crosstraittophits <- GWADATA.getrows(objGWA.indep, which(!is.na(aCrossTraitTopHit)))
		
				
		objREPORT <- REPORT.addval(objREPORT,paste(strTagMultiIndep,"numCrossTraitLoci",sep=""),max(aCrossTraitLocusId,na.rm=TRUE))

	} else {
		
		objGWA.indep <- GWADATA.copy(objGWA)
		objGWA.indep.x.traittophits <- GWADATA.copy(objGWA)
		objGWA.indep.x.crosstraittophits <- GWADATA.copy(objGWA)
		
		strTagMultiIndep <- ifelse(strTagMultiIndep != "", paste(strTagMultiIndep,".",sep=""), strTagMultiIndep)
		if(blnAddIndepInfo) {
			objGWA.added <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTagMultiIndep, "aCrossTraitLocusId",sep=""))
			objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)), paste(strTagMultiIndep, "aCrossTraitTopHit",sep=""))
			objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)), paste(strTagMultiIndep, "aCrossTraitLocusTag",sep=""))
			objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTagMultiIndep, "aTraitTopHit",sep=""))
			
			objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTagMultiIndep, "aCrossTraitLocusCoordinates",sep=""))
			if(blnRecombRate) objGWA.added <- GWADATA.cbind(objGWA.added, rep(NA,nrow(objGWA@tblGWA)),paste(strTagMultiIndep, "aCrossTraitRRCoordinates",sep=""))
			
			objGWA <- objGWA.added
		}
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTagMultiIndep,"numCrossTraitLoci",sep=""),0)
	
	}
	
	return(list(objGWA,objGWA.indep,objGWA.indep.x.traittophits,objGWA.indep.x.crosstraittophits,objREPORT))
	
}


MULTINDEP <- function(strEqcCommand){ 
	## Wrapper for class definition
	MULTINDEPout <- setMULTINDEP(new("MULTINDEP", strEqcCommand = strEqcCommand))
	validMULTINDEP(MULTINDEPout)
	#MULTINDEPout.valid <- validMULTINDEP(MULTINDEPout)
	return(MULTINDEPout)
}

