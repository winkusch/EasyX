setClass("INDEP",
	representation = representation(
						strEqcCommand		=	"character",
						rcdCriterion		=	"character",
						colIndep			=	"character",
						blnIndepMin			=	"logical",
						colInChr			=	"character",
						colInPos			=	"character",
						numPosLim			=	"numeric",
						##blnRegion2Signal 	=	"logical",
						blnStepDown		 	= 	"logical",
						blnRecombRate 		= 	"logical",
						fileRecombRate		=	"character",
						colRecombRateChr	=	"character",
						colRecombRatePos	=	"character",
						colRecombRateRate	=	"character",
						numRecombRateLim 	= 	"numeric",
						blnRecombRateMaximize = "logical",
						blnRecombRatePosLim = "logical",
						filePrio			=	"character",
						colPrioMarker		=	"character",
						colInMarker			=	"character",
						blnCandidates		=	"logical",
						blnCandidateRegions	=	"logical",
						numCandidateRegionsPosBoundary 	= "numeric",
						blnCandidateVariants	=	"logical",
						rcdCritCandidateVariants =	"character", ## defined by another threshold
						blnCandidateGenes		= "logical",
						colInGene 				= "character",
						blnWriteCandidateRegionsInOneFile	=	"logical",
						blnWriteCandidateRegionsInSeparateFiles	=	"logical",
						blnWriteCandidateVariants	=	"logical",
						blnAddNearestGene 	= "logical",
						fileAnnotGene		= "character",
						blnAddIndepInfo 	= 	"logical",
						fileAnnot			=	"character",
						strAnnotTag			=	"character",
						colAnnotTag			=	"character",
						colAnnotChr			=	"character",
						colAnnotPos			=	"character",
						numAnnotPosLim		=	"numeric",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						rcdCriterion		=	"",
						colIndep			=	"",
						blnIndepMin			=	TRUE,
						colInChr			=	"",
						colInPos			=	"",
						numPosLim			=	500000,
						blnStepDown		 	= 	FALSE,
						blnRecombRate 		= 	FALSE,
						fileRecombRate		=	"",
						colRecombRateChr	=	"chr",
						colRecombRatePos	=	"position",
						colRecombRateRate	=	"COMBINED_rate.cM.Mb.",
						numRecombRateLim 	= 	20,
						blnRecombRateMaximize = FALSE,
						blnRecombRatePosLim  = TRUE,
						filePrio			=	"",
						colPrioMarker		=	"",
						colInMarker			=	"",
						blnCandidates	=	FALSE,
						blnCandidateRegions	=	FALSE,
						numCandidateRegionsPosBoundary 	= 500000, # 500000
						blnCandidateVariants	=	FALSE,
						rcdCritCandidateVariants=	"",
						blnCandidateGenes 		= FALSE,
						colInGene 	= "",
						blnWriteCandidateRegionsInOneFile	=	FALSE,
						blnWriteCandidateRegionsInSeparateFiles	=	FALSE, 
						blnWriteCandidateVariants	=	FALSE,
						blnAddNearestGene 			= FALSE,
						fileAnnotGene			= "",
						blnAddIndepInfo 	= 	FALSE,
						fileAnnot 			= 	"",
						strAnnotTag 		= 	"",
						colAnnotTag 		= 	"", 
						colAnnotChr			=	"Chr",
						colAnnotPos			=	"Pos",
						numAnnotPosLim		=	-1,
						strTag				=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setINDEP", function(object) standardGeneric("setINDEP"))
setMethod("setINDEP", signature = (object = "INDEP"), function(object) {
	
	aEqcSlotNamesIn = c("rcdCriterion","colIndep","blnIndepMin","colInChr","colInPos","numPosLim","blnAddIndepInfo","strTag","blnStepDown","filePrio","colPrioMarker","colInMarker","blnCandidateRegions","numCandidateRegionsPosBoundary","rcdCritCandidateVariants",
							"blnWriteCandidateRegionsInOneFile", "blnWriteCandidateRegionsInSeparateFiles", "blnWriteCandidateRegionsCandidates","blnCandidateGenes","colInGene","blnCandidateVariants","blnCandidates","fileAnnotGene","blnAddNearestGene",
							"blnRecombRate","fileRecombRate","numRecombRateLim","colRecombRateChr","colRecombRatePos","colRecombRateRate","blnRecombRateMaximize","blnRecombRatePosLim",
							"fileAnnot","colAnnotTag","strAnnotTag","colAnnotChr","colAnnotPos","numAnnotPosLim")

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
validINDEP <- function(objINDEP) {
	
	### Valid with GWADATA?
	
	if(objINDEP@rcdCriterion == "") 
		cat(paste(" EASY WARNING:INDEP\n No criterion rcdCriterion defined. All data will be used for independentisation.", sep=""))
	if(objINDEP@colIndep == "") 
		stop(paste(" EASY ERROR:INDEP\n No column colIndep defined. Please set colIndep.", sep=""))
	if(objINDEP@colInChr == "") 
		stop(paste(" EASY ERROR:INDEP\n No column colInChr defined. Please set colInChr.", sep=""))
	if(objINDEP@colInPos == "") 
		stop(paste(" EASY ERROR:INDEP\n No column colInPos defined. Please set colInPos.", sep=""))
	
	if(objINDEP@filePrio != "") {
		if(!file.exists(objINDEP@filePrio))
			stop(paste("EASY ERROR:INDEP\n File filePrio\n ",objINDEP@filePrio,"\n does not exist.\n Please check path or remove --filePrio.", sep=""))
		if(objINDEP@colPrioMarker == "")
			stop(paste(" EASY ERROR:INDEP\n No SNP file Marker column defined. \n Please define colPrioMarker that will be used for merging the SNP file to the data-set.", sep=""))
		if(objINDEP@colInMarker == "")
			stop(paste(" EASY ERROR:INDEP\n No input Marker column defined. \n Please define colInMarker that will be used for merging the data-set to the SNP file.", sep=""))
	}
	
	if(objINDEP@fileAnnotGene != "") {
		if(!file.exists(objINDEP@fileAnnotGene))
			stop(paste("EASY ERROR:INDEP\n File fileAnnotGene\n ",objINDEP@fileAnnotGene,"\n does not exist.\n Please check path or remove --fileAnnotGene.", sep=""))
	}
	
	if(objINDEP@blnRecombRate) {
		
		if(objINDEP@fileRecombRate=="")
			stop(paste("EASY ERROR:INDEP\n File fileRecombRate undefined.\n Please set --fileRecombRate or reset --blnRecombRate 0.", sep=""))
			
		if(!file.exists(objINDEP@fileRecombRate))
			stop(paste("EASY ERROR:INDEP\n File fileRecombRate\n ",objINDEP@fileRecombRate,"\n does not exist.\n Please check path or remove --fileRecombRate or reset --blnRecombRate 0.", sep=""))
	}
		
	return(TRUE)
}
INDEP.GWADATA.valid <- function(objINDEP, objGWA) {
	
	isAv <- objINDEP@colIndep %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:INDEP\n Defined column colIndep \n",objINDEP@colIndep, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objINDEP@colInChr %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:INDEP\n Defined column colInChr \n",objINDEP@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isAv <- objINDEP@colInPos %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:INDEP\n Defined column colInPos \n",objINDEP@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	
	if(objINDEP@filePrio != "") {
		isAv <- objINDEP@colInMarker %in% objGWA@aHeader
		if(!isAv)
			stop(paste(" EASY ERROR:INDEP\n Defined column colInMarker \n",objINDEP@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	}
	
	if(objINDEP@blnCandidates) {
		objINDEP@blnCandidateRegions = TRUE
		objINDEP@blnCandidateVariants = TRUE
		objINDEP@blnCandidateGenes = TRUE
	}
	
	# if(objINDEP@blnWriteCandidateVariants) objINDEP@blnCandidateVariants = TRUE
	# if(objINDEP@blnWriteCandidateRegionsInOneFile|objINDEP@blnWriteCandidateRegionsInSeparateFiles) objINDEP@blnCandidateRegions = TRUE
	
	if(objINDEP@blnCandidateVariants) {
		objINDEP@blnCandidateRegions = TRUE
		objINDEP@blnWriteCandidateVariants = TRUE
		
		if(objINDEP@rcdCritCandidateVariants == "")
			stop(paste(" EASY ERROR:INDEP\n No criterion rcdCritCandidateVariants defined. \n Required for blnCandidateVariants=1. Please define rcdCritCandidateVariants that will be used to obtain candidate SNPs in the candidate regions.", sep=""))
	}
	
	
	if(objINDEP@blnCandidateGenes) {
		
		objINDEP@blnCandidateRegions = TRUE
		
		if(objINDEP@fileAnnotGene != "") {
		
			objINDEP@colInGene = "aNearestGene"
			
		} else {
			
			if(objINDEP@colInGene == "") {
				stop(paste(" EASY ERROR:INDEP\n No colInGene defined. \n Required for blnCandidateGenes=1. Please set.", sep=""))
			}
				
			isAv <- objINDEP@colInGene %in% objGWA@aHeader
			if(!isAv)
				stop(paste(" EASY ERROR:INDEP\n Defined column colInGene \n",objINDEP@colInGene, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))	
		}
	}
	
	if(objINDEP@blnAddNearestGene) {
		
		if(!file.exists(objINDEP@fileAnnotGene)) {
			stop(paste(" EASY ERROR:INDEP\n File \n",objINDEP@fileAnnotGene, "\n is not available / does not exist. Please specify gene file.", sep=""))	
		}
	}
	
	
	if(objINDEP@blnCandidateRegions) {
		objINDEP@blnWriteCandidateRegionsInOneFile <- TRUE
		objINDEP@blnWriteCandidateRegionsInSeparateFiles <- TRUE
	}
	
	return(objINDEP)
	
}

INDEP.read <- function(objINDEP, blnReadAll) {
	
	fR = objINDEP@fileRecombRate
	
	if(blnReadAll) {
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "")
	} else {
		tblR = read.table(fR, header=T, stringsAsFactors=FALSE, strip.white = TRUE, comment.char = "", nrows = 10)
	}
	
	if(dim(tblR)[1]==0)
		stop(paste("EASY ERROR:INDEP\n There are no rows available in \n",fR,"\n The file is empty!!!\n", sep=""))
	
	if(!(objINDEP@colRecombRateChr%in%names(tblR))) 
		stop(paste(" EASY ERROR:INDEP\n Column colRecombRateChr \n",objINDEP@colRecombRateChr," \n cannot be found in fileRecombRate \n", fR,".", sep=""))
	
	if(!(objINDEP@colRecombRatePos%in%names(tblR)))
		stop(paste(" EASY ERROR:INDEP\n Column colRecombRatePos \n",objINDEP@colRecombRatePos," \n cannot be found in fileRecombRate \n", fR,".", sep=""))
	
	if(!(objINDEP@colRecombRateRate%in%names(tblR))) 
		stop(paste(" EASY ERROR:INDEP\n Column colRecombRateRate \n",objINDEP@colRecombRateRate," \n cannot be found in fileRecombRate \n", fR,".", sep=""))
	
	
	tblRShort = tblR[,c(objINDEP@colRecombRateChr,objINDEP@colRecombRatePos,objINDEP@colRecombRateRate)]
	names(tblRShort) <- c("chr","pos","recomb_rate_cM_Mb")
	
	###sort by chr and pos
	tblRShortOrdered = tblRShort[order(as.integer(tblRShort$chr),as.integer(tblRShort$pos)),]
	
	return(tblRShortOrdered)
}

#############################################################################################################################
INDEP.run <- function(objINDEP, objGWA, objREPORT, tblRR, isValidScript) {
	
	rcdCriterion 	<- objINDEP@rcdCriterion
	colIndep		<- objINDEP@colIndep
	blnIndepMin		<- objINDEP@blnIndepMin
	colInChr		<- objINDEP@colInChr
	colInPos		<- objINDEP@colInPos
	numPosLim		<- objINDEP@numPosLim
	blnAddIndepInfo	<- objINDEP@blnAddIndepInfo
	strTag			<- objINDEP@strTag
	blnStepDown		<- objINDEP@blnStepDown
	filePrio		<- objINDEP@filePrio
	colPrioMarker	<- objINDEP@colPrioMarker
	colInMarker		<- objINDEP@colInMarker
	
	blnCandidateRegions <- objINDEP@blnCandidateRegions
	blnCandidateVariants <- objINDEP@blnCandidateVariants
	blnCandidateGenes 	<- objINDEP@blnCandidateGenes
	blnAddNearestGene <- objINDEP@blnAddNearestGene
	numCandidateRegionsPosBoundary <- objINDEP@numCandidateRegionsPosBoundary
	rcdCritCandidateVariants <- objINDEP@rcdCritCandidateVariants
	colInGene 		<- objINDEP@colInGene
	
	fileAnnotGene 	<- objINDEP@fileAnnotGene
	
	blnRecombRate 	<- objINDEP@blnRecombRate
	numRecombRateLim 	<- objINDEP@numRecombRateLim
	blnRecombRateMaximize <- objINDEP@blnRecombRateMaximize
	blnRecombRatePosLim <- objINDEP@blnRecombRatePosLim
	
	fileAnnot = objINDEP@fileAnnot
	strAnnotTag = objINDEP@strAnnotTag
	colAnnotTag = objINDEP@colAnnotTag
	colAnnotChr = objINDEP@colAnnotChr
	colAnnotPos = objINDEP@colAnnotPos
	numAnnotPosLim = objINDEP@numAnnotPosLim
	
	if(numAnnotPosLim == -1) numAnnotPosLim = numPosLim
	blnAnnot = ifelse(fileAnnot=="", FALSE, TRUE)
	
	#### 
	
	
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	blnUsePrioFile <- !(filePrio=="")
	blnUseAnnotGeneFile <- !(fileAnnotGene=="")


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
	
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepCrit",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepNA",sep=""),0)
		objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepLoci",sep=""),0)
		
		if(blnAddIndepInfo) {
			
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aLociTag",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aTopHit",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aNumLocusSNPs",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aLocusCoordinates",sep=""))
			objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aLocusSize",sep=""))
			
			if(blnRecombRate) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aRRCoordinates",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aRRSize",sep=""))
			}
		
			if(blnAddNearestGene) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGene",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"NearestGeneDistance",sep=""))
			}
			
			if(blnCandidateRegions) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aCandidateRegions",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aCandidateVariant",sep=""))
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"numCandidateVariants",sep=""))
				
				if(blnCandidateGenes) {
					objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"strCandidateGenes",sep=""))
				}
			}
			
			if(blnAnnot) {
				objGWA <- GWADATA.cbind(objGWA, rep(NA,nrow(objGWA@tblGWA)), paste(strTag,"aLocusAnnot",sep=""))
			}
			
		}
		
		return(list(objGWA,objGWA.indep,objGWA.indep,objGWA.indep,objGWA.indep,objREPORT))
	}
	
	is_na_indep = is.na(GWADATA.getcol(objGWA.indep, colIndep)) | is.na(GWADATA.getcol(objGWA.indep, colInChr)) | is.na(GWADATA.getcol(objGWA.indep, colInPos))

	objGWA.indep@tblGWA <- 	objGWA.indep@tblGWA[!is_na_indep,]

	objGWA.indep = GWADATA.sort(objGWA.indep, colIndep, ablnDescending = !blnIndepMin)

	numIndepNA = length(which(is_na_indep))
	
	aTopHit <- aLociTag <- aNumLocusSNPs <- aLocusCoordinates <- aLocusSize <- aLocusAnnot <- aRRCoordinates <- aRRSize <- rep(NA, nrow(objGWA.indep@tblGWA))
	
	hitcount=1
	
	aIndepVal 	= GWADATA.getcol(objGWA.indep, colIndep)
	aChr 		= GWADATA.getcol(objGWA.indep, colInChr)
	aPos 		= GWADATA.getcol(objGWA.indep, colInPos)
	
	aIndepValBak <- aIndepVal
	# j=1
	# h=1
	
	if(blnRecombRate) {
		lsRecombRateChrThrs = list()
		# for(chr in 1:22) lsGmap = c(lsGmap, tblGmap[tblGmap$chr==chr,])
		for(chr in 1:22) {
			
			tblRRi = tblRR[tblRR$chr==chr,]
			
			if(blnRecombRateMaximize) {
				## only required if the input rates have not been maximized by locus
				aidxUsed = which(tblRRi$recomb_rate_cM_Mb>numRecombRateLim) 	
				lsClumps = list()

				### Find clumps
				i = 1
				idxOld = 0
				
				for(idxUsed in aidxUsed) {
					
					if(i == 1) {
						## start 
						aClump = idxUsed
					} else if((idxUsed - idxOld)==1) {
						## add to current clump
						aClump = c(aClump, idxUsed)
					} else {
						## clean up current clump and strar over
						lsClumps = c(lsClumps, list(aClump))
						aClump = idxUsed
					}
					
					if(i == length(aidxUsed)) {
						## close list element 
						lsClumps = c(lsClumps, list(aClump))
					}
					
					idxOld = idxUsed
					i = i + 1
				}
				
				for(aClump in lsClumps) { 
					## keep max and set RR of others to -1
					acm = tblRRi$recomb_rate_cM_Mb[aClump]
					idxMax = which(acm == max(acm, na.rm = TRUE))
					if(length(idxMax)>1) idxMax = floor(mean(idxMax))
					tblRRi$recomb_rate_cM_Mb[aClump[-idxMax]] = -1
				}
			} 
			## redo comparison 
			aidxUsed = which(tblRRi$recomb_rate_cM_Mb>numRecombRateLim) 	
			# ### reduce map to signif breaks
			# isChrTmpAndUsed = tblRR$chr==chr & tblRR$recomb_rate_cM_Mb>numRecombRateLim
			lsRecombRateChrThrs = c(lsRecombRateChrThrs, list(tblRRi[aidxUsed,]))
		}
	}
	
	while(any(is.na(aLociTag))) {
		#if(h==69) stop()
		if(blnIndepMin) {
			iTmpExtr = which(aIndepVal == min(aIndepVal))[1]
		} else { 
			iTmpExtr = which(aIndepVal == max(aIndepVal))[1]
		}
				
		chrExtr =  aChr[iTmpExtr]
		posExtr =  aPos[iTmpExtr]
		
		# if(chrExtr==17 & posExtr==59236155) stop()
		
		if(blnRecombRate & isValidScript) {
			####################################
			## isCurrentLocus all within <20 rr upp and down
			## chrExtr, posExtr lead variant
			if(!chrExtr%in%as.character(c(1:22))) stop(paste(" EASY ERROR:INDEP\n blnRecombRate 1 can only be used for autosomal input! \n", sep=""))
			
			tr = lsRecombRateChrThrs[[as.integer(chrExtr)]]
			
			posdiff = posExtr-tr$pos
			
			if(any(posdiff>0) & any(posdiff<0)) {
				## current SNP lies between two RR hotspots
				poslow = min(posdiff[posdiff>0])
				idxLow = which(tr$pos==(posExtr-poslow))
				## variant lies before other RR hotspot
				idxUpp = idxLow + 1
				pos1 = tr$pos[idxLow]
				pos2 = tr$pos[idxUpp]
			} else if(any(posdiff>0) & !any(posdiff<0)) {
				## current SNP lies after all RR hotspots
				## use highest hotspot to end of GWAS data
				poslow = min(posdiff[posdiff>0])
				idxLow = which(tr$pos==(posExtr-poslow))
				pos1 = tr$pos[idxLow]
				pos2 = Inf
			} else if(!any(posdiff>0) & any(posdiff<0)) {
				## current SNP lies before all RR hotspots
				## use highest hotspot to end of GWAS data
				posupp = min(abs(posdiff[posdiff<0]))
				idxUpp = which(tr$pos==(posExtr+posupp))
				pos1 = 0
				pos2 = tr$pos[idxUpp]
			} else {
				stop("EASY ERROR: INDEP\n UNCLEAR POSITION MAPPING.")
			}
			
			isCurrentLocus = aChr == chrExtr & aPos>=pos1 & aPos<=pos2
			
			aTopHit[iTmpExtr] = hitcount
			aLociTag[which(isCurrentLocus & is.na(aLociTag))] = hitcount
			aRRCoordinates[which(isCurrentLocus & is.na(aRRCoordinates))] = paste(chrExtr,":",pos1,"_",pos2,sep="")
			aRRSize[which(isCurrentLocus & is.na(aRRSize))] = pos2-pos1
			
			hitcount = hitcount + 1
			
			if(blnRecombRatePosLim) {
				
				## check for distant variants withing Recomb rate thresholds and separate them out into two signals!
								
				apostmp = aPos[isCurrentLocus]
				idxorder = order(apostmp)
				apostmpsorted = apostmp[idxorder]
				adiff = apostmpsorted[-1]-apostmpsorted[-length(apostmpsorted)]
				aidxGap = which(adiff>numPosLim)
				
				if(length(aidxGap)>0) {
					# there is at least one gap 
					nsignal = 1
					
					for(idxGap in aidxGap) {
						# get indices to increase hitcount (keep first and add hits)
						aidxincrease = seq(idxGap+1, length(apostmpsorted))
						aposincrease = apostmpsorted[aidxincrease]
						isReplace = aChr == chrExtr & aPos %in% aposincrease
						aLociTag[isReplace] = hitcount
						hitcount = hitcount + 1 
						nsignal = nsignal + 1
					}
					
					ahitcountsregion = (hitcount-nsignal):(hitcount-1)
					
					for(locitagtmp in ahitcountsregion) {
						
						isSignal = aLociTag == locitagtmp & !is.na(aLociTag)
						
						if(blnIndepMin) {
							iTmpExtrSignal = which(isSignal & aIndepVal == min(aIndepVal[isSignal]))[1]
						} else { 
							iTmpExtrSignal = which(isSignal & aIndepVal == max(aIndepVal[isSignal]))[1]
						}
						aTopHit[iTmpExtrSignal] = locitagtmp
					}
				}
			}
		} else {
			####################################
			### use default position mapping to fill aTopHit, aLociTag and hitcount:
			isCurrentLocus = aChr == chrExtr & abs(aPos - posExtr) <= numPosLim	
			
			posCurrentLocusMin = min(aPos[isCurrentLocus],na.rm=T)
			posCurrentLocusMax = max(aPos[isCurrentLocus],na.rm=T)
			
			#isCurrentLocusExtend = aChr == chrExtr & (abs(aPos-posCurrentLocusMin) <= numPosLim) | (abs(aPos-posCurrentLocusMax) <= numPosLim)
			
			isCurrentLocusExtended = aChr == chrExtr & aPos>=(posCurrentLocusMin-numPosLim) & aPos<=(posCurrentLocusMax+numPosLim)
			
			isCurrentLocusAlreadySet = any(!is.na(aLociTag[which(isCurrentLocusExtended)]))
					
			if(blnStepDown & isCurrentLocusAlreadySet) {
				### step down
				## get set Locus IDs
				aLocusIDsSet = unique(aLociTag[which(isCurrentLocusExtended)])
				aLocusIDsSet = aLocusIDsSet[!is.na(aLocusIDsSet)]
				## usually this should be a single locus
				## however in theory it is possible that loci overlap to the left and to the right
				## get lociNum with lowest P and reset all SNPs in the locus to that 
				## this is always the lower locus number because the data set is sorted
				isCurrentLocusWide = isCurrentLocusExtended | aLociTag%in%aLocusIDsSet
				locusIdNew = min(aLocusIDsSet)
				## this assumes that the data set is sorted by p value !
				
				aLociTag[which(isCurrentLocusWide)] = locusIdNew
				aTopHit[which(isCurrentLocusWide & aTopHit!=locusIdNew)] = NA
				
			} else {
				aTopHit[iTmpExtr] = hitcount
				aLociTag[which(isCurrentLocus & is.na(aLociTag))] = hitcount
				hitcount = hitcount + 1			
			}
		}


		if(blnIndepMin) {
			aIndepVal[which(isCurrentLocus)] = Inf
		} else {
			aIndepVal[which(isCurrentLocus)] = -Inf
		}
		#h=h+1
	}
	
	## reformat alocitag, atophit to ascending order without gaps
	aLociTagUniSort = sort(unique(aLociTag))
	for(i in 1:length(aLociTagUniSort)) {
		iTagTmp = aLociTagUniSort[i]
		aLociTag[aLociTag==iTagTmp]=i
		aTopHit[aTopHit==iTagTmp&!is.na(aTopHit)]=i
	}
	hitcount=max(aTopHit,na.rm=T)
	
	if(blnUsePrioFile) {
		# aTopHit, aLociTag, aIndepVal
		tblPrio<-read.table(filePrio,header=T, sep="",  stringsAsFactors=FALSE)
		icolMarkerPrio = match(colPrioMarker,names(tblPrio))[1]
		aMarkerIn 	= GWADATA.getcol(objGWA.indep, colInMarker)
		ablnPrio <- aMarkerIn%in%tblPrio[,icolMarkerPrio]
		for(locusnum in unique(na.omit(aTopHit))) {
			iTopHit <- which(aTopHit == locusnum & !is.na(aTopHit))
			aiLocus <- which(aLociTag == locusnum)
			if(!ablnPrio[iTopHit] & any(ablnPrio[aiLocus])) {
				iTopHitOld <- iTopHit
				if(blnIndepMin) {
					aIndepValTmp <- ifelse(aLociTag == locusnum & ablnPrio, aIndepValBak, Inf)
					iTopHitNew <- which.min(aIndepValTmp)
				} else {
					aIndepValTmp <- ifelse(aLociTag == locusnum & ablnPrio, aIndepValBak, -Inf)
					iTopHitNew <- which.max(aIndepValTmp)
				}
				aTopHit[iTopHitOld] <- NA
				aTopHit[iTopHitNew] <- locusnum
			} 
		}
	}
	objGWA.indep = GWADATA.cbind(objGWA.indep, aTopHit, "aTopHit")
	objGWA.indep = GWADATA.cbind(objGWA.indep, aLociTag, "aLociTag")
	
	### add number of locus SNPs
	
	if(blnAnnot) {
		tblAnnot<-read.table(fileAnnot, header=T, sep="\t", stringsAsFactors=FALSE)
		aRefChr = tblAnnot[, which(names(tblAnnot) == colAnnotChr)]
		aRefPos = tblAnnot[, which(names(tblAnnot) == colAnnotPos)]
		
		if(colAnnotTag != "") {
			aRefTag = tblAnnot[, which(names(tblAnnot) == colAnnotTag)]
		} else {
			aRefTag = rep(strAnnotTag,length(aRefPos))
		}
	}
	
	for(locusNum in unique(na.omit(aTopHit))) {
		isLocus = aLociTag == locusNum
		aNumLocusSNPs[isLocus] <- length(which(aLociTag == locusNum))
		chrtmp = unique(aChr[isLocus])
		pos1 = min(aPos[isLocus],na.rm=T)
		pos2 = max(aPos[isLocus],na.rm=T)
		aLocusCoordinates[isLocus] <- paste(chrtmp,":",pos1,"_",pos2,sep="")
		aLocusSize[isLocus] <- pos2-pos1
		
		if(blnAnnot) {
			isKnown = chrtmp==aRefChr & aRefPos>=(pos1-numAnnotPosLim) & aRefPos<=(pos2+numAnnotPosLim)
			if(any(isKnown)) {
				unitag = unique(aRefTag[isKnown])
				unitag = unitag[!is.na(unitag)]	
				if(length(unitag)>0) aLocusAnnot[isLocus] = paste(unitag,collapse=";")	
			}
		}
	}
	objGWA.indep = GWADATA.cbind(objGWA.indep, aNumLocusSNPs, "aNumLocusSNPs")
	objGWA.indep = GWADATA.cbind(objGWA.indep, aLocusCoordinates, "aLocusCoordinates")
	objGWA.indep = GWADATA.cbind(objGWA.indep, aLocusSize, "aLocusSize")
	objGWA.indep = GWADATA.cbind(objGWA.indep, aLocusAnnot, "aLocusAnnot")
	
	if(blnRecombRate) {
		
		aRRCoordinates = as.character(aRRCoordinates)
		aRRCoordinates = gsub("Inf","EoCHR",aRRCoordinates)
		aRRSize = as.character(aRRSize)
		aRRSize = gsub("Inf","EoCHR",aRRSize)
		
		objGWA.indep = GWADATA.cbind(objGWA.indep, aRRCoordinates, "aRRCoordinates")
		objGWA.indep = GWADATA.cbind(objGWA.indep, aRRSize, "aRRSize")
	}
	
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aLociTag", paste(strTag,"aLociTag",sep=""))
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aTopHit", paste(strTag,"aTopHit",sep=""))
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aNumLocusSNPs", paste(strTag,"aNumLocusSNPs",sep=""))
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aLocusCoordinates", paste(strTag,"aLocusCoordinates",sep=""))
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aLocusSize", paste(strTag,"aLocusSize",sep=""))
	objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aLocusAnnot", paste(strTag,"aLocusAnnot",sep=""))
	
	if(blnRecombRate) {
		objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aRRCoordinates", paste(strTag,"aRRCoordinates",sep=""))
		objGWA.indep <- GWADATA.renamecol(objGWA.indep, "aRRSize", paste(strTag,"aRRSize",sep=""))
	}
	
	if(blnUseAnnotGeneFile | blnAddNearestGene) {
		## read gene file 
		tG = read.table(fileAnnotGene,sep=" ",stringsAsFactors=F,header=F,colClasses=c("character","integer","integer","character"))
		
		## t:\M_GENETICS\ReferenceData\glist-hg19.txt
		## 19 58858171 58864865 A1BG
		
		names(tG) <- c("chr","pos1","pos2","gene")
		
		aChr 		= GWADATA.getcol(objGWA.indep, colInChr)
		aPos 		= GWADATA.getcol(objGWA.indep, colInPos)
		
		aNearestGene <- aNearestGeneDistance <- rep(NA,length(aPos))
		
		print("Starting annotation ... ")
		for(i in 1:length(aChr)) {
			
			chr=aChr[i]
			pos=aPos[i]
			
			isWithinGene = (chr == tG$chr) & (pos >= tG$pos1) & (pos <= tG$pos2)
			
			if(any(isWithinGene)) {
				aNearestGene[i] = paste(tG$gene[isWithinGene],collapse=",")
				aNearestGeneDistance[i] = 0
			} else {
				tGChr = tG[tG$chr==chr,]
				adismin = pmin(abs(pos-tGChr$pos1),abs(pos-tGChr$pos2))
				idxMin = which.min(adismin)
				aNearestGene[i] = tGChr$gene[idxMin[1]] # 1 if variant has equal dis to two genes
				aNearestGeneDistance[i] = min(adismin)
				## duplicates in glist -> take union !!!
				if(pos-tGChr$pos1[idxMin[1]]<0) aNearestGeneDistance[i] = - aNearestGeneDistance[i]
			}
		}
		
		# paste(strTag,"NearestGene",sep="")
		
		objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGene, paste(strTag,"NearestGene",sep=""))
		objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGeneDistance, paste(strTag,"NearestGeneDistance",sep=""))
		
		colInGene = paste(strTag,"NearestGene",sep="")
		
		# objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGene, "aNearestGene")
		# objGWA.indep = GWADATA.cbind(objGWA.indep, aNearestGeneDistance, "aNearestGeneDistance")
		
		# cbind
	}
	

	
	objGWA.indep.x <- GWADATA.getrows(objGWA.indep, which(!is.na(aTopHit)))
	
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepCrit",sep=""),numIndepCrit)
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepNA",sep=""),numIndepNA)
	# objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepLoci",sep=""),hitcount-1)
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepLoci",sep=""),hitcount)
	
	objGWA.indep.candidateregions <- GWADATA.copy(objGWA)
	objGWA.indep.candidatevariants <- GWADATA.copy(objGWA)
	
	if(blnAddIndepInfo | blnCandidateRegions) {
		### merges by intersect(names)
		objGWA.indep.tmp <- objGWA.indep

		if(paste(strTag,"aLociTag",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aLociTag to large data-set failed because column\n",paste(strTag,"aLociTag",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aTopHit",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aTopHit to large data-set failed because column\n",paste(strTag,"aTopHit",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aNumLocusSNPs",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aNumLocusSNPs to large data-set failed because column\n",paste(strTag,"aNumLocusSNPs",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aLocusCoordinates",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aLocusCoordinates to large data-set failed because column\n",paste(strTag,"aLocusCoordinates",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aLocusSize",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aLocusSize to large data-set failed because column\n",paste(strTag,"aLocusSize",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")			
		if(paste(strTag,"aLocusAnnot",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aLocusAnnot to large data-set failed because column\n",paste(strTag,"aLocusAnnot",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aRRCoordinates",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aRRCoordinates to large data-set failed because column\n",paste(strTag,"aRRCoordinates",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")
		if(paste(strTag,"aRRSize",sep="") %in% objGWA@aHeader) 
			stop("EASY ERROR:INDEP\nAdding column aRRSize to large data-set failed because column\n",paste(strTag,"aRRSize",sep=""),"\n is already present GWADATA. PLease use --strTag to influence the column name.")			
			
		### merges by intersect(names)		
		objGWA.tmp <- GWADATA.merge(objGWA,objGWA.indep.tmp, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
		
		if(blnCandidateRegions) {
			colLociTag = paste(strTag,"aLociTag",sep="")
			
			aLociTag = GWADATA.getcol(objGWA.tmp, colLociTag)
			aChr = GWADATA.getcol(objGWA.tmp, colInChr)
			aPos = GWADATA.getcol(objGWA.tmp, colInPos)
				
			aLociTagUni = unique(aLociTag)
			aLociTagUni = aLociTagUni[!is.na(aLociTagUni)]
			
			objRCD 	<- RCD(rcdCritCandidateVariants)
			isCandidateVariant 	<- RCD.eval(objRCD, objGWA.tmp)
			
			aGenes = GWADATA.getcol(objGWA.tmp, colInGene)
			
			# objGWA.indep.candidateregions <- GWADATA.cbind(objGWA.indep.candidateregions, ifelse(is.na(out),NA,ifelse(out,1,0)), paste(strTag,"isCandidateVariant",sep=""))
			
			aCandidateRegions <- aCandidateVariant <- numCandidateVariants <- strCandidateGenes <- rep(NA,nrow(objGWA.tmp@tblGWA))
			
			for(locitag in aLociTagUni) {
				
				isCurLocus = aLociTag==locitag&!is.na(aLociTag)
				
				curChr = unique(aChr[isCurLocus])
				curPosmin = min(aPos[isCurLocus],na.rm=T)
				curPosmax = max(aPos[isCurLocus],na.rm=T)
				
				isCandidateRegion = aChr==curChr & aPos>=(curPosmin-numCandidateRegionsPosBoundary)& aPos<=(curPosmax+numCandidateRegionsPosBoundary)
				
				isUnset = is.na(aCandidateRegions)
				if(any(isCandidateRegion&isUnset)) aCandidateRegions[isCandidateRegion&isUnset] = locitag
				if(any(isCandidateRegion&!isUnset)) aCandidateRegions[isCandidateRegion&!isUnset] = paste(aCandidateRegions[isCandidateRegion&!isUnset],locitag,sep=";")
				
				if(blnCandidateVariants) {
					isUnset = is.na(aCandidateVariant)
					if(any(isCandidateRegion&isCandidateVariant&isUnset)) aCandidateVariant[isCandidateRegion&isCandidateVariant&isUnset] = locitag
					if(any(isCandidateRegion&isCandidateVariant&!isUnset)) aCandidateVariant[isCandidateRegion&isCandidateVariant&!isUnset] = paste(aCandidateVariant[isCandidateRegion&isCandidateVariant&!isUnset],locitag,sep=";")
					# aCandidateVariant[isCandidateRegion&isCandidateVariant] = locitag
					numCandidateVariants[isCandidateRegion&isCandidateVariant] = length(which(isCandidateRegion&isCandidateVariant))
				}
				
				if(blnCandidateGenes) { 
					
					aGenesRegion =  aGenes[isCandidateRegion&!is.na(aGenes)]
					aGenesRegionUni = unique(aGenesRegion)
					
					if(length(aGenesRegionUni)>0) {
						strGenes = paste(aGenesRegionUni,collapse=",")
						isUnset = is.na(strCandidateGenes)
						if(any(isCandidateRegion&isUnset)) strCandidateGenes[isCandidateRegion&isUnset] = strGenes
						if(any(isCandidateRegion&!isUnset)) strCandidateGenes[isCandidateRegion&!isUnset] = paste(strCandidateGenes[isCandidateRegion&!isUnset],strGenes,sep=";")
					}
				}
				
			}
			objGWA.tmp = GWADATA.cbind(objGWA.tmp, aCandidateRegions,  paste(strTag,"aCandidateRegions",sep=""))
				
			if(blnCandidateVariants) {
				objGWA.tmp = GWADATA.cbind(objGWA.tmp, aCandidateVariant,  paste(strTag,"aCandidateVariant",sep=""))
				objGWA.tmp = GWADATA.cbind(objGWA.tmp, numCandidateVariants,  paste(strTag,"numCandidateVariants",sep=""))
			}
			if(blnCandidateGenes) objGWA.tmp = GWADATA.cbind(objGWA.tmp, strCandidateGenes,  paste(strTag,"strCandidateGenes",sep=""))
			
			objGWA.indep.candidateregions = GWADATA.getrows(objGWA.tmp, which(!is.na(aCandidateRegions)))
			
			if(blnCandidateVariants) {
				objGWA.indep.candidatevariants = GWADATA.getrows(objGWA.tmp, which(!is.na(aCandidateVariant)))
			}
			
			## add to indep and indepx
			objGWA.indep <- GWADATA.merge(objGWA.indep,objGWA.tmp, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = FALSE, strBy.In = NA, strBy.Add = NA)
			objGWA.indep.x <- GWADATA.merge(objGWA.indep.x,objGWA.tmp, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = FALSE, strBy.In = NA, strBy.Add = NA)	

		}
		if(blnAddIndepInfo) {
			objGWA <- objGWA.tmp
		}
	}	

	return(list(objGWA,objGWA.indep,objGWA.indep.x,objGWA.indep.candidateregions,objGWA.indep.candidatevariants,objREPORT))
		
}

INDEP <- function(strEqcCommand){ 
	## Wrapper for class definition
	INDEPout <- setINDEP(new("INDEP", strEqcCommand = strEqcCommand))
	validINDEP(INDEPout)
	#INDEPout.valid <- validINDEP(INDEPout)
	return(INDEPout)
}

