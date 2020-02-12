setClass("INDEPX",
	representation = representation(
						strEqcCommand		=	"character",
						rcdCriterion		=	"character",
						acolPval			=	"character",
						astrPvalTag			=	"character",
						anumPvalLim			=	"numeric",
						colInChr			=	"character",
						colInPos			=	"character",
						numPosLim			=	"numeric",
						fileRecombRate		=	"character",
						colRecombRateChr	=	"character",
						colRecombRatePos	=	"character",
						colRecombRate		=	"character",
						numRecombRateLim 	= 	"numeric",
						fileGene			=   "character",
						colGeneChr			=	"character",
						colGenePosStart		=	"character",
						colGenePosStop		=	"character",
						colGeneName			=	"character",
						fileAnnot			=	"character",
						strAnnotTag			=	"character",
						colAnnotTag			=	"character",
						colAnnotChr			=	"character",
						colAnnotPos			=	"character",
						numAnnotPosLim		=	"numeric",
						blnAddIndepInfo 	= 	"logical",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						rcdCriterion		=	"",
						acolPval			=	"",
						astrPvalTag			=	"",
						anumPvalLim			=	5e-8,
						colInChr			=	"",
						colInPos			=	"",
						numPosLim			=	500000,
						fileRecombRate		=	"",
						colRecombRateChr	=	"chr",
						colRecombRatePos	=	"position",
						colRecombRate		=	"COMBINED_rate.cM.Mb.",
						numRecombRateLim 	= 	20,
						fileGene			= 	"",
						colGeneChr			=	"Chr",
						colGenePosStart		=	"Pos1",
						colGenePosStop		=	"Pos2",
						colGeneName			=	"Gene",
						fileAnnot			=	"",
						strAnnotTag			=	"Annot",
						colAnnotTag			=	"",
						colAnnotChr			=	"Chr",
						colAnnotPos			=	"Pos",
						numAnnotPosLim		=	-1,
						blnAddIndepInfo 	= 	FALSE,
						strTag				=	"INDEPX"
						)
	#contains = c("EcfReader")
)

setGeneric("setINDEPX", function(object) standardGeneric("setINDEPX"))
setMethod("setINDEPX", signature = (object = "INDEPX"), function(object) {
	
	aEqcSlotNamesIn = c("rcdCriterion","acolPval","astrPvalTag","anumPvalLim","colInChr","colInPos","numPosLim","fileRecombRate","colRecombRateChr","colRecombRatePos","colRecombRate","numRecombRateLim",
						"fileGene","colGeneChr","colGenePosStart","colGenePosStop","colGeneName",
						"fileAnnot","strAnnotTag","colAnnotTag","colAnnotChr","colAnnotPos","numAnnotPosLim",
						"blnAddIndepInfo","strTag")
	
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
validINDEPX <- function(objINDEPX) {
	
	### Valid with GWADATA?
	
	# if(objINDEPX@rcdCriterion == "") 
		# cat(paste(" EASY WARNING:INDEPX\n No criterion rcdCriterion defined. All data will be used for independentisation.", sep=""))
		
	if(objINDEPX@acolPval[1] == "") 
		stop(paste(" EASY ERROR:INDEPX\n No column acolPval defined. Please set acolPval.", sep=""))
	if(objINDEPX@colInChr == "") 
		stop(paste(" EASY ERROR:INDEPX\n No column colInChr defined. Please set colInChr.", sep=""))
	if(objINDEPX@colInPos == "") 
		stop(paste(" EASY ERROR:INDEPX\n No column colInPos defined. Please set colInPos.", sep=""))


	# ## check fileRecombRate
	# if(objINDEPX@fileRecombRate=="") 
		# stop(paste("EASY ERROR:INDEPX\n File fileRecombRate undefined.\n Please set --fileRecombRate or reset --blnRecombRate 0.", sep=""))
	
	if(!objINDEPX@fileRecombRate=="") {
	
		if(!file.exists(objINDEPX@fileRecombRate))
			stop(paste("EASY ERROR:INDEPX\n File fileRecombRate\n ",objINDEPX@fileRecombRate,"\n does not exist.\n Please check path or remove --fileRecombRate or reset --blnRecombRate 0.", sep=""))
		
		tr = read.table(objINDEPX@fileRecombRate, sep="\t", header=T, stringsAsFactors=F, nrows=1)
			
		isAv = objINDEPX@colRecombRateChr %in% names(tr)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colRecombRateChr \n",objINDEPX@colRecombRateChr, "\n is not available in recombination rate file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))
		
		isAv = objINDEPX@colRecombRatePos %in% names(tr)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colRecombRatePos \n",objINDEPX@colRecombRatePos, "\n is not available in recombination rate file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))

		isAv = objINDEPX@colRecombRate %in% names(tr)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colRecombRate \n",objINDEPX@colRecombRate, "\n is not available in recombination rate file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))
	}
	
	## check fileGene
	if(objINDEPX@fileGene != "") {
		if(!file.exists(objINDEPX@fileGene))
			stop(paste("EASY ERROR:INDEPX\n File fileGene\n ",objINDEPX@fileGene,"\n does not exist.\n Please check path or remove --fileGene.", sep=""))
			
		tg = read.table(objINDEPX@fileGene, sep="\t", header=T, stringsAsFactors=F, nrows=1)
		
		isAv = objINDEPX@colGeneChr %in% names(tg)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colGeneChr \n",objINDEPX@colGeneChr, "\n is not available in gene file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))
		
		isAv = objINDEPX@colGenePosStart %in% names(tg)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colGenePosStart \n",objINDEPX@colGenePosStart, "\n is not available in gene file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))

		isAv = objINDEPX@colGenePosStop %in% names(tg)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colGenePosStop \n",objINDEPX@colGenePosStop, "\n is not available in gene file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))
	
		isAv = objINDEPX@colGeneName %in% names(tg)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colGeneName \n",objINDEPX@colGeneName, "\n is not available in gene file \n",objINDEPX@fileGene,"\n PLease specify correct column name.", sep=""))
		
	}
	
	
	## check fileAnnot
	if(objINDEPX@fileAnnot != "") {
		if(!file.exists(objINDEPX@fileAnnot))
			stop(paste("EASY ERROR:INDEP\n File --fileAnnotGene\n ",objINDEPX@fileAnnot,"\n does not exist.\n Please check path or remove --fileAnnot.", sep=""))
			
		tk = read.table(objINDEPX@fileAnnot, sep="\t", header=T, stringsAsFactors=F, nrows=1)
		
		isAv = objINDEPX@colAnnotChr %in% names(tk)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colAnnotChr \n",objINDEPX@colAnnotChr, "\n is not available in known loci file \n",objINDEPX@fileAnnot,"\n PLease specify correct column name.", sep=""))
		
		isAv = objINDEPX@colAnnotPos %in% names(tk)
		if(!isAv)
			stop(paste(" EASY ERROR:INDEPX\n Defined column --colAnnotPos \n",objINDEPX@colAnnotPos, "\n is not available in known loci file \n",objINDEPX@fileAnnot,"\n PLease specify correct column name.", sep=""))
	
		if(objINDEPX@colAnnotTag != "") {
			isAv = objINDEPX@colAnnotTag %in% names(tk)
			if(!isAv)
				stop(paste(" EASY ERROR:INDEPX\n Defined column --colAnnotTag \n",objINDEPX@colAnnotTag, "\n is not available in known loci file \n",objINDEPX@fileAnnot,"\n PLease specify correct column name.", sep=""))
		}
	}
	
	return(TRUE)
}
INDEPX.GWADATA.valid <- function(objINDEPX, objGWA) {
	
	aisMatch = objINDEPX@acolPval %in% objGWA@aHeader
	if(any(!aisMatch))
		stop(paste("EASY ERROR:INDEPX\n Column \n",objINDEPX@acolPval[which(!aisMatch)[1]]," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	isAv <- objINDEPX@colInChr %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:INDEPX\n Defined column colInChr \n",objINDEPX@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objINDEPX@colInPos %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:INDEPX\n Defined column colInPos \n",objINDEPX@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if(length(objINDEPX@anumPvalLim)==1 & length(objINDEPX@acolPval)>1) objINDEPX@anumPvalLim = rep(objINDEPX@anumPvalLim[1], length(objINDEPX@acolPval))
	
	if(all(objINDEPX@astrPvalTag=="")) objINDEPX@astrPvalTag = objINDEPX@acolPval
	
	return(objINDEPX)
	
}

INDEPX.read <- function(objINDEPX, blnReadAll) {
	
	fR = objINDEPX@fileRecombRate
	
	if(blnReadAll) {
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "")
	} else {
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", nrows = 10)
	}
	
	tblRShort = tblR[,c(objINDEPX@colRecombRateChr,objINDEPX@colRecombRatePos,objINDEPX@colRecombRate)]
	names(tblRShort) <- c("chr","pos","recomb_rate_cM_Mb")
	
	###sort by chr and pos
	tblRShortOrdered = tblRShort[order(as.integer(tblRShort$chr),as.integer(tblRShort$pos)),]
	
	return(tblRShortOrdered)
}

#############################################################################################################################
INDEPX.run <- function(objINDEPX, objGWA, objREPORT, tblRR, isValidScript) {
	
	rcdCriterion 	<- objINDEPX@rcdCriterion
	acolPval		<- objINDEPX@acolPval
	astrPvalTag 	<- objINDEPX@astrPvalTag
	anumPvalLim		<- objINDEPX@anumPvalLim
	colInChr		<- objINDEPX@colInChr
	colInPos		<- objINDEPX@colInPos
	numPosLim		<- objINDEPX@numPosLim
	fileRecombRate <- objINDEPX@fileRecombRate
	numRecombRateLim <- objINDEPX@numRecombRateLim
	
	fileGene 	<- objINDEPX@fileGene
	colGeneChr 	<- objINDEPX@colGeneChr
	colGenePosStart <- objINDEPX@colGenePosStart
	colGenePosStop 	<- objINDEPX@colGenePosStop
	colGeneName 	<- objINDEPX@colGeneName
	
	fileAnnot 	<- objINDEPX@fileAnnot
	strAnnotTag 	<- objINDEPX@strAnnotTag
	colAnnotTag 	<- objINDEPX@colAnnotTag
	colAnnotChr 	<- objINDEPX@colAnnotChr
	colAnnotPos 	<- objINDEPX@colAnnotPos
	numAnnotPosLim 	<- objINDEPX@numAnnotPosLim
	
	blnAddIndepInfo	<- objINDEPX@blnAddIndepInfo
	strTag			<- objINDEPX@strTag
	
	#### 
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	blnRecombRate = !(fileRecombRate=="")
	blnGene = !(fileGene=="")
	blnAnnot = !(fileAnnot=="")
	if(numAnnotPosLim == -1) numAnnotPosLim = numPosLim
	
	### 
	
	if(rcdCriterion != "") {
		objRCD 	<- RCD(rcdCriterion)
		out 	<- RCD.eval(objRCD, objGWA)
		numIndepCrit = length(which(out))
		objGWA.indep <- GWADATA.getrows(objGWA, which(out))
	} else {
		numIndepCrit <- nrow(objGWA@tblGWA)
		objGWA.indep <- objGWA
	}
	
	if(numIndepCrit == 0) {
	
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepIn",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numPosMissing",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numRegion",sep=""),0)
		if(blnRecombRate) objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSignal",sep=""),0)
		
		if(blnAddIndepInfo) {
			
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"pMin",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionId",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionLead",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionTag",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionCoordinates",sep=""))
			
			if(blnRecombRate) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalId",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalLead",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalTag",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalCoordinates",sep=""))
			}
			
			if(blnAnnot) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"",sep=""))
				if(blnRecombRate) objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalAnnot",sep=""))
			}
			
			if(blnGene) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGene",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGeneDistance",sep=""))
			}

		}
		
		return(list(objGWA,objGWA.indep,objGWA.indep,objGWA.indep,objREPORT))
	}
	
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepIn",sep=""),numIndepCrit)
	
	
	##############
	### remove missing positions
	
	isNaPos = is.na(GWADATA.getcol(objGWA.indep, colInChr)) | is.na(GWADATA.getcol(objGWA.indep, colInPos))
	numPosMissing = length(which(isNaPos))

	objGWA.indep@tblGWA <- 	objGWA.indep@tblGWA[!isNaPos,]

	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numPosMissing",sep=""),numPosMissing)


	##############
	### reduce to significant variants and count significant variants by tag
	
	isSignif = rep(FALSE,nrow(objGWA.indep@tblGWA))
	
	for(i in 1:length(acolPval)) { 
		
		apvalTmp = GWADATA.getcol(objGWA.indep, acolPval[i])
		
		isSignifTmp = apvalTmp < anumPvalLim[i] & !is.na(apvalTmp)
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numVarSignif.",astrPvalTag[i],sep=""),length(which(isSignifTmp)))
		
		isSignif = isSignif | isSignifTmp
	}
	
	if(all(!isSignif)) {
		
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numRegion",sep=""),0)
		if(blnRecombRate) objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSignal",sep=""),0)
		
		if(blnAddIndepInfo) {
			
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"pMin",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionId",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionLead",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionTag",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"regionCoordinates",sep=""))
			
			if(blnRecombRate) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalId",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalLead",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalTag",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalCoordinates",sep=""))
			}
			
			if(blnAnnot) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"",sep=""))
				if(blnRecombRate) objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"signalAnnot",sep=""))
			}
			
			if(blnGene) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGene",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGeneDistance",sep=""))
			}

		}
		
		return(list(objGWA,objGWA.indep,objGWA.indep,objGWA.indep,objREPORT))
		
	}
	### return if no variants are left
	
	objGWA.indep <- GWADATA.getrows(objGWA.indep, which(isSignif))
	
	##############
	### start independetization
		
	tIn = as.data.frame(objGWA.indep@tblGWA)
	
	if(length(acolPval)>1) {
		aPmin = apply(tIn[,acolPval],1,min,na.rm=T)
	} else {
		aPmin = tIn[,acolPval]
	}
	aidxsort = order(aPmin)
	
	tInSort = tIn[aidxsort,]
	aPminSort = aPmin[aidxsort]
	
	aChr = tInSort[,colInChr]
	aPos = tInSort[,colInPos]
	
	### work with aChr, aPos, aPminSort and tInSort[,acolPval] to obtain : 
	
	####################################
	### 1. Obtain regionId, regionLead
	
	regionId <- regionLead <- regionCoordinates <- regionSize <- rep(NA, nrow(tInSort))
	
	regioncount = 1
	
	aPminSortBackup = aPminSort
	
	while(any(is.na(regionId))) {
			
		# print(regioncount)
		
		iTmpExtr = which(aPminSort == min(aPminSort))[1]
				
		chrExtr =  aChr[iTmpExtr]
		posExtr =  aPos[iTmpExtr]
		
		### use position mapping to obtain regions
		
		isCurRegion = NA
		isCurRegionNew = aChr==chrExtr & aPos>=(posExtr-numPosLim) & aPos<=(posExtr+numPosLim)
		
		while(!identical(isCurRegion,isCurRegionNew)) {
			
			isCurRegion = isCurRegionNew
			
			regionPos1 = min(aPos[isCurRegion])
			regionPos2 = max(aPos[isCurRegion])
			
			isCurRegionNew = aChr==chrExtr & aPos>=(regionPos1-numPosLim) & aPos<=(regionPos2+numPosLim)
			
		}
		
		regionId[isCurRegionNew] = regioncount
		regionLead[iTmpExtr] = regioncount
		regionCoordinates[isCurRegionNew] = paste(chrExtr,":",regionPos1-numPosLim,"_",regionPos2+numPosLim,sep="")
		regionSize[isCurRegionNew] = (regionPos2+numPosLim) - (regionPos1-numPosLim)
		
		regioncount = regioncount + 1
		
		aPminSort[isCurRegionNew] = Inf
		
	}
	
	aPminSort = aPminSortBackup
	
	####################################
	### 2. Obtain signalId, signalLead, signalCoordinates
	
	if(blnRecombRate) {
	
		signalId <- signalLead <- signalCoordinates <- signalSize <- rep(NA, nrow(tInSort))
		
		tblRR = tblRR[order(tblRR$chr,tblRR$pos),]
		
		for(rid in unique(regionId)) {
			
			# if(rid==82) stop()
			
			isRegion = regionId == rid
			
			signalcount = 1
			
			aPminSortBackup = aPminSort
			
			while(any(is.na(signalId[isRegion]))) {
				
				# print(paste(rid,signalcount,sep="."))
				
				iTmpExtr = which(aPminSort == min(aPminSort[isRegion]) & isRegion)[1]
				
				chrExtr =  aChr[iTmpExtr]
				posExtr =  aPos[iTmpExtr]
				
				isChr = tblRR$chr==chrExtr
				isPosLow = tblRR$pos<=posExtr
				isPosHigh = tblRR$pos>=posExtr
				
				if(any(isChr & isPosLow)) {
					pos1 = tblRR$pos[max(which(isChr & isPosLow))]
				} else {
					pos1 = 0
				}
				
				if(any(isChr & isPosHigh)) {
					pos2 = tblRR$pos[min(which(isChr & isPosHigh))]
				} else {
					pos2 = Inf
				}
				
				isCurSignal = aChr == chrExtr & aPos >= pos1 & aPos <= pos2 & is.na(signalId) & isRegion
				
				# signalPos1 = min(aPos[isCurSignal])
				# signalPos2 = min(aPos[isCurSignal])
				
				signalId[isCurSignal] = signalcount
				signalLead[iTmpExtr] = signalcount
				signalCoordinates[isCurSignal] = paste(chrExtr,":",pos1,"_",pos2,sep="")
				if(pos2==Inf) {
					signalSize[isCurSignal] = max(aPos[isCurSignal])-pos1
				} else {
					signalSize[isCurSignal] = pos2-pos1
				}
				
				signalcount = signalcount + 1
				
				aPminSort[isCurSignal] = Inf
				
			}
			
			aPminSort = aPminSortBackup
			
		}
		
		signalCoordinates = gsub("Inf","EoCHR",signalCoordinates)
		
		signalId = paste(regionId,signalId,sep=".")
		signalLead = ifelse(!is.na(signalLead), signalId, NA)
	}
	
	####################################
	### 3. Obtain signalTag and regionTag 

	regionTag <- regionNumVariants <- regionNumSignals <- rep(NA, nrow(tInSort))
	
	for(rid in unique(regionId)) {
		
		# print(rid)
		
		isRegion = regionId == rid
		
		numvar = length(which(isRegion))
		if(blnRecombRate) numsig = length(unique(signalId[isRegion]))
		
		artag = c()
		arnumvar = c()
		arnumsig = c()
		
		for(i in 1:length(acolPval)) { 
			
			colp = acolPval[i]
			tag = astrPvalTag[i]
			plim = anumPvalLim[i]
			
			isRidSignif = isRegion & tInSort[,colp]<plim
			
			if(any(isRidSignif)) { 
				artag = c(artag, astrPvalTag[i])
				arnumvar = c(arnumvar, length(which(isRidSignif)))
				if(blnRecombRate) arnumsig = c(arnumsig, length(unique(signalId[isRidSignif])))
			} 
		}
		
		strrtag = ifelse(length(artag)==0,NA,ifelse(length(artag)==1, artag, paste(artag,collapse=";")))
		regionTag[isRegion] = strrtag
		
		strNumVar = ifelse(length(arnumvar)==0,NA,ifelse(length(arnumvar)==1, as.character(arnumvar), paste(numvar,"(",paste(arnumvar,collapse=";"),")",sep="")))
		regionNumVariants[isRegion] = strNumVar
		
		if(blnRecombRate) {
			strNumSig = ifelse(length(arnumsig)==0,NA,ifelse(length(arnumsig)==1, as.character(arnumsig), paste(numsig,"(",paste(arnumsig,collapse=";"),")",sep="")))
			regionNumSignals[isRegion] = strNumSig
		}
	}
	
	if(blnRecombRate) {
		signalTag <- signalNumVariants <- rep(NA, nrow(tInSort))
		
		for(sid in unique(signalId)) {
			
			isSignal = signalId == sid
			
			numvar = length(which(isSignal))
			
			astag = c()
			asnumvar = c()
			
			for(i in 1:length(acolPval)) { 
				
				colp = acolPval[i]
				tag = astrPvalTag[i]
				plim = anumPvalLim[i]
				
				isSidSignif = isSignal & tInSort[,colp]<plim
				
				if(any(isSidSignif)) {
					astag = c(astag, astrPvalTag[i])
					asnumvar = c(asnumvar, length(which(isSidSignif)))
				}
			}
			
			strstag = ifelse(length(astag)==0,NA,ifelse(length(astag)==1, astag, paste(astag,collapse=";")))
			signalTag[isSignal] = strstag
			
			strNumVar = ifelse(length(asnumvar)==0,NA,ifelse(length(asnumvar)==1, as.character(asnumvar), paste(numvar,"(",paste(asnumvar,collapse=";"),")",sep="")))
			signalNumVariants[isSignal] = strNumVar
		}
	}
	
	####################################
	### 4. Optional Annotation
	
	aNearestGene <- aNearestGeneDistance <- rep(NA,length(aPos))
	
	if(blnGene) {
		
		cat("\n   -> Starting gene annotation ... ")
		
		tG = read.table(fileGene,sep="\t",stringsAsFactors=F,header=T)
				
		gchr = tG[,colGeneChr]
		gpos1 = as.integer(tG[,colGenePosStart])
		gpos2 = as.integer(tG[,colGenePosStop])
		gene = tG[,colGeneName]
		
		tG = data.frame(gchr,gpos1,gpos2,gene,stringsAsFactors=F)
		
		## existant from above
		# aChr 		= GWADATA.getcol(objGWA.indep, colInChr)
		# aPos 		= GWADATA.getcol(objGWA.indep, colInPos)
		
		for(i in 1:length(aChr)) {
			
			chr=aChr[i]
			pos=aPos[i]
			
			isWithinGene = (chr == tG$gchr) & (pos >= tG$gpos1) & (pos <= tG$gpos2)
			
			if(any(isWithinGene)) {
				aNearestGene[i] = paste(tG$gene[isWithinGene],collapse=",")
				aNearestGeneDistance[i] = 0
			} else {
				isChr = 
				tGChr = tG[tG$gchr==chr,]
				adismin = pmin(abs(pos-tGChr$gpos1),abs(pos-tGChr$gpos2))
				idxMin = which.min(adismin)
				aNearestGene[i] = tGChr$gene[idxMin[1]] # 1 if variant has equal dis to two genes
				aNearestGeneDistance[i] = min(adismin)
				## duplicates in glist -> take union !!!
				# if(pos-tGChr$gpos1[idxMin[1]]<0) aNearestGeneDistance[i] = - aNearestGeneDistance[i]
				if(pos-tGChr$gpos1[idxMin[1]]>0) aNearestGeneDistance[i] = - aNearestGeneDistance[i]
			}
		}
	}
	
	regionAnnot <- annotDistance <- signalAnnot <- rep(NA,length(aPos))
	
	if(blnAnnot) {
	
		cat("\n   -> Starting annotation ... ")
		
		tAn<-read.table(fileAnnot, header=T, sep="\t", stringsAsFactors=FALSE)
		
		anchr = tAn[,colAnnotChr]
		anpos = as.integer(tAn[,colAnnotPos])
		
		if(colAnnotTag != "") {
			antag = tAn[, colAnnotTag]
		} else {
			antag = rep(strAnnotTag,length(anpos))
		}
		
		## if known lead variant is < numAnnotPosLim distant from region border (outer gws variant!; not (outer gws variant + numPosLim) """")
		
		for(rid in unique(regionId)) {
			
			isRegion = regionId == rid
			rchr = unique(aChr[isRegion])
			rpos1 = min(aPos[isRegion],na.rm=T)
			rpos2 = max(aPos[isRegion],na.rm=T)
			
			## do not use regionCoordinates because those were extended by numPoslim
			
			isKnown = anchr == rchr & anpos >= (rpos1-numAnnotPosLim) & anpos <= (rpos2+numAnnotPosLim)
			
			if(any(isKnown)) {
				unitag = unique(antag[isKnown])
				unitag = unitag[!is.na(unitag)]	
				if(length(unitag)>0) regionAnnot[isRegion] = paste(unitag,collapse=";")	
				
				aanpostmp = anpos[isKnown]
				countannots = 1
				for(anpostmp in aanpostmp) {
					if(countannots == 1) {
						adistmp = anpostmp - aPos[isRegion]
					} else {
						adistmp = paste(adistmp, anpostmp - aPos[isRegion], sep=";")
					}
					countannots = countannots + 1
				}
				annotDistance[isRegion] = adistmp
			}
		}
		
		if(blnRecombRate) {
			for(sid in unique(signalId)) {
				
				isSignal = signalId == sid
				schr = unique(aChr[isSignal])
				spos = unique(unlist(lapply(strsplit(signalCoordinates[isSignal],":"), function(x) x[2])))
				spos = gsub("EoCHR","Inf",spos)
				spos1 = as.numeric(unlist(lapply(strsplit(spos,"_"), function(x) x[1])))
				spos2 = as.numeric(unlist(lapply(strsplit(spos,"_"), function(x) x[2])))
				
				isKnown = anchr == schr & anpos >= spos1 & anpos <= spos2
				
				if(any(isKnown)) {
					unitag = unique(antag[isKnown])
					unitag = unitag[!is.na(unitag)]	
					if(length(unitag)>0) signalAnnot[isSignal] = paste(unitag,collapse=";")
				}
			}
		}

	}
	
	####################################
	### 5. Add variables

	## reset objGWA to sorted table
	objGWA.indep@tblGWA <- as.data.table(tInSort)
		
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionId, paste(strTag,"regionId",sep=""))
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionLead, paste(strTag,"regionLead",sep=""))
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionTag, paste(strTag,"regionTag",sep=""))
	
	if(blnRecombRate) objGWA.indep = GWADATA.cbind(objGWA.indep, regionNumSignals, paste(strTag,"regionNumSignals",sep=""))
	
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionNumVariants, paste(strTag,"regionNumVariants",sep=""))
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionCoordinates, paste(strTag,"regionCoordinates",sep=""))
	objGWA.indep = GWADATA.cbind(objGWA.indep, regionSize, paste(strTag,"regionSize",sep=""))
	
	if(blnAnnot) {
		objGWA.indep = GWADATA.cbind(objGWA.indep, regionAnnot, paste(strTag,"regionAnnot",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, annotDistance, paste(strTag,"annotDistance",sep=""))
	}	
	
	if(blnRecombRate) {
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalId, paste(strTag,"signalId",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalLead, paste(strTag,"signalLead",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalTag, paste(strTag,"signalTag",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalNumVariants, paste(strTag,"signalNumVariants",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalCoordinates, paste(strTag,"signalCoordinates",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, signalSize, paste(strTag,"signalSize",sep=""))
		
		if(blnAnnot) objGWA.indep = GWADATA.cbind(objGWA.indep, signalAnnot, paste(strTag,"signalAnnot",sep=""))
	}
	
	if(blnGene) {
		objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGene, paste(strTag,"NearestGene",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGeneDistance, paste(strTag,"NearestGeneDistance",sep=""))
	}
	
	####################################
	### 6. Resort and prepare output

	## no resort: 
	objGWA.indep@tblGWA <- objGWA.indep@tblGWA
	objGWA.indep.regionLeads <- GWADATA.getrows(objGWA.indep, which(!is.na(regionLead)))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numRegions",sep=""),length(unique(regionId)))
	## regionTag
	artleads = regionTag[which(!is.na(regionLead))]
	for(rt in sort(unique(artleads))) {
		nrt = length(which(artleads==rt))
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numRegions.",rt,sep=""),nrt)
	}
	
	if(blnRecombRate) {
		objGWA.indep.signalLeads <- GWADATA.getrows(objGWA.indep, which(!is.na(signalLead)))
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSignals",sep=""),length(unique(signalId)))
		## signalTag
		astleads = signalTag[which(!is.na(signalLead))]
		for(st in sort(unique(astleads))) {
			nst = length(which(astleads==st))
			objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSignals.",st,sep=""),nst)
		}
	} else {
		objGWA.indep.signalLeads <- objGWA.indep.regionLeads
	}
	
	if(blnAddIndepInfo) {
		### merges by intersect(names)		
		objGWA <- GWADATA.merge(objGWA,objGWA.indep, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
	}	

	return(list(objGWA,objGWA.indep,objGWA.indep.regionLeads,objGWA.indep.signalLeads,objREPORT))

}

INDEPX <- function(strEqcCommand){ 
	## Wrapper for class definition
	INDEPXout <- setINDEPX(new("INDEPX", strEqcCommand = strEqcCommand))
	validINDEPX(INDEPXout)
	#INDEPXout.valid <- validINDEPX(INDEPXout)
	return(INDEPXout)
}

