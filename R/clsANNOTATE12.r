setClass("ANNOTATE",
	representation = representation(
						strEqcCommand	=	"character",
						colInChr		=	"character",
						colInPos		=	"character",
						colInPval		=	"character",
						numPvalLim		= 	"numeric",
						fileAnnot		=	"character",
						colAnnotChr		=	"character",
						colAnnotPos		=	"character",
						strAnnotTag		=	"character",
						colAnnotTag		=	"character",
						colAnnotCoord	= 	"character",
						colOutAnnot		=	"character",
						numAnnotPosLim	=	"numeric"
						),
	prototype = prototype(
						strEqcCommand	=	"",
						colInChr		=	"",
						colInPos		=	"",
						colInPval		=	"",
						numPvalLim		= 	1,
						fileAnnot		=	"",
						colAnnotChr		=	"Chr",
						colAnnotPos		=	"Pos",
						strAnnotTag		=	"",
						colAnnotTag		=	"",
						colAnnotCoord	=   "",
						colOutAnnot		=	"AnnotTag",
						numAnnotPosLim	=	500000
						)
	#contains = c("EcfReader")
)

setGeneric("setANNOTATE", function(object) standardGeneric("setANNOTATE"))
setMethod("setANNOTATE", signature = (object = "ANNOTATE"), function(object) {
	
	#aEqcSlotNamesIn = c("colInChr","colInPos","colInPval","numPvalLim","fileAnnot","colAnnotChr","colAnnotPos","strAnnotTag","colOutAnnot","numAnnotPosLim")
	aEqcSlotNamesIn = c("colInChr","colInPos","colInPval","numPvalLim","fileAnnot","strAnnotTag","colOutAnnot","numAnnotPosLim","colAnnotTag","colAnnotCoord")

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
validANNOTATE <- function(objANNOTATE) {
	
	if(objANNOTATE@colInChr == "") 
		stop(paste(" EASY ERROR:ANNOTATE\n No column colInChr defined. Please set colInChr.", sep=""))
	if(objANNOTATE@colInPos == "") 
		stop(paste(" EASY ERROR:ANNOTATE\n No column colInPos defined. Please set colInPos.", sep=""))
	if(objANNOTATE@fileAnnot == "") 
		stop(paste(" EASY ERROR:ANNOTATE\n No reference file defined. Please set fileAnnot.", sep=""))
	
	if(objANNOTATE@strAnnotTag == "" & objANNOTATE@colAnnotTag == "") 
		stop(paste(" EASY ERROR:ANNOTATE\n No strAnnotTag or colAnnotTag defined. Please set one of the two.", sep=""))
			
	if(objANNOTATE@strAnnotTag != "" & objANNOTATE@colAnnotTag != "") 		 
		warning(paste(" EASY ERROR:ANNOTATE\n Both strAnnotTag and colAnnotTag were defined. ANNOTATE will use colAnnotTag and ignore the strAnnotTag.", sep=""))
	
	
	### Valid with GWADATA?
	
	if(objANNOTATE@fileAnnot != "") {
		if(!file.exists(objANNOTATE@fileAnnot))
			stop(paste("EASY ERROR:ANNOTATE\n File fileAnnot\n ",objANNOTATE@fileAnnot,"\n does not exist.", sep=""))
		### Cols exist?
		
		tblAnnot<-read.table(objANNOTATE@fileAnnot,header=T, sep="",  stringsAsFactors=FALSE)
		
		isUseCoord = objANNOTATE@colAnnotCoord != ""
		
		if(isUseCoord) {
			isAv = objANNOTATE@colAnnotCoord %in% names(tblAnnot)
				if(!isAv)
					stop(paste(" EASY ERROR:ANNOTATE\n Defined column --colAnnotCoord \n",objANNOTATE@colAnnotCoord, "\n is not available in known loci file \n",objANNOTATE@fileAnnot,"\n PLease specify correct column name.", sep=""))
		} else {
			isAv <- objANNOTATE@colAnnotChr %in% names(tblAnnot)
			if(!isAv)
				stop(paste(" EASY ERROR:ANNOTATE\n Defined column colAnnotChr \n",objANNOTATE@colAnnotChr, "\n is not available in fileAnnot. PLease specify correct column name.", sep=""))
			isAv <- objANNOTATE@colAnnotPos %in% names(tblAnnot)	
			if(!isAv)
				stop(paste(" EASY ERROR:ANNOTATE\n Defined column colAnnotPos \n",objANNOTATE@colAnnotPos, "\n is not available in fileAnnot. PLease specify correct column name.", sep=""))
			
		}
		
		if(objANNOTATE@colAnnotTag != "") {
			isAv <- objANNOTATE@colAnnotTag %in% names(tblAnnot)
			if(!isAv)
				stop(paste(" EASY ERROR:ANNOTATE\n Defined column colAnnotTag \n",objANNOTATE@colAnnotTag, "\n is not available in fileAnnot. PLease specify correct column name.", sep=""))
		}
	}
	
	return(TRUE)
}
ANNOTATE.GWADATA.valid <- function(objANNOTATE, objGWA) {
	
	isNotAv <- !(objANNOTATE@colInChr %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:ANNOTATE\n Defined column colInChr \n",objANNOTATE@colInChr, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	isNotAv <- !(objANNOTATE@colInPos %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:ANNOTATE\n Defined column colInPos \n",objANNOTATE@colInPos, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	iPos = match(objANNOTATE@colInPos, objGWA@aHeader)
	isPosNumeric <- objGWA@aClasses[iPos] == "numeric" | objGWA@aClasses[iPos] == "integer" | objGWA@aClasses[iPos] == "double"

	if(!isPosNumeric)
		stop(paste(" EASY ERROR:ANNOTATE\n Defined column colInPos \n",objANNOTATE@colInPos, "\n is not numeric for GWA data-set \n",objGWA@fileIn,"\n . Please cast colInPos to numeric, integer or double.", sep=""))
	
	
	isDefinedNotAv <- objANNOTATE@colInPval != "" & !(objANNOTATE@colInPval %in% objGWA@aHeader)
	if(isDefinedNotAv)
		stop(paste(" EASY ERROR:ANNOTATE\n Defined column colInPval \n",objANNOTATE@colInPval, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	if(objANNOTATE@numPvalLim < 1) {
		if(!(objANNOTATE@colInPval %in% objGWA@aHeader))
			stop(paste(" EASY ERROR:ANNOTATE\n To use the Pvalue threshold for the annotation of loci, you have to define colInPval as well.","\n PLease specify colInPval with the ANNOTATE command !!!", sep=""))
		if(objANNOTATE@numPvalLim <=0)
			stop(paste(" EASY ERROR:ANNOTATE\n PLease specify numPvalLim within the range ]0,1[ within the ANNOTATE command !!!", sep=""))
	}
	
}
#############################################################################################################################
ANNOTATE.run <- function(objANNOTATE, objGWA) {
	
	colInChr		=	objANNOTATE@colInChr
	colInPos		=	objANNOTATE@colInPos
	colInPval		=	objANNOTATE@colInPval
	numPvalLim		=	objANNOTATE@numPvalLim
	colAnnotChr		=	objANNOTATE@colAnnotChr
	colAnnotPos		=	objANNOTATE@colAnnotPos
	colAnnotTag		=	objANNOTATE@colAnnotTag
	strAnnotTag		=	objANNOTATE@strAnnotTag
	colOutAnnot		= 	objANNOTATE@colOutAnnot
	numAnnotPosLim		=	objANNOTATE@numAnnotPosLim
	colAnnotCoord  	= objANNOTATE@colAnnotCoord
	
	aInChr = GWADATA.getcol(objGWA, colInChr)
	aInPos = GWADATA.getcol(objGWA, colInPos)
	
	if(numPvalLim < 1) {
		aInPval = GWADATA.getcol(objGWA, colInPval)
	} else {
		aInPval = rep(0, dim(objGWA@tblGWA)[1])
	}

	tblLoci<-read.table(objANNOTATE@fileAnnot, header=T, sep="\t", stringsAsFactors=FALSE)
	numLoci = dim(tblLoci)[1]
	
	if(colAnnotCoord!="") {
		## extract values from coord 1:123_466
		acoord = tblLoci[,colAnnotCoord]
		aRefChr = unlist(lapply(strsplit(acoord,":"),function(x) x[1]))
		strpos = unlist(lapply(strsplit(acoord,":"),function(x) x[2]))
		aRefPos1 = as.integer(unlist(lapply(strsplit(strpos,"_"),function(x) x[1])))
		aRefPos2 = as.integer(unlist(lapply(strsplit(strpos,"_"),function(x) x[2])))
	} else {
		aRefChr = tblLoci[,colAnnotChr]
		anpos = as.integer(tblLoci[,colAnnotPos])
		aRefPos1 = anpos
		aRefPos2 = anpos
	}
	
	# aRefChr = tblLoci[, which(names(tblLoci) == colAnnotChr)]
	# aRefPos = tblLoci[, which(names(tblLoci) == colAnnotPos)]
	
	if(colAnnotTag != "") {
		aRefTag = tblLoci[, which(names(tblLoci) == colAnnotTag)]
	} else {
		aRefTag = rep(strAnnotTag,length(aRefPos1))
	}
	
	
	if(colOutAnnot%in%objGWA@aHeader) {
		aTagOut = GWADATA.getcol(objGWA, colOutAnnot) # is already annotated
	} else {
		aTagOut = rep(NA, dim(objGWA@tblGWA)[1]) # not used before
	}
	
	for(i in 1:numLoci) {
		chrTmp = aRefChr[i]
		# posTmp = aRefPos[i]
		pos1Tmp = aRefPos1[i]
		pos2Tmp = aRefPos2[i]
		tagTmp = aRefTag[i]
		
		pos1Tmp = pos1Tmp-numAnnotPosLim
		pos2Tmp = pos2Tmp+numAnnotPosLim
		
		#isCurrentLocus = aInChr == chrTmp & abs(aInPos - posTmp) <= numAnnotPosLim & aInPval <= numPvalLim
		# isCurrentLocus = aInChr == chrTmp & abs(aInPos - posTmp) <= numAnnotPosLim
		# isCurrentLocus[is.na(isCurrentLocus)] = FALSE
		
		isCurrentLocus = aInChr == chrTmp & aInPos>=pos1Tmp & aInPos<=pos2Tmp
		isCurrentLocus[is.na(isCurrentLocus)] = FALSE
		
		if(any(isCurrentLocus)) {
			isAnyPvalLow = any(aInPval[isCurrentLocus] <= numPvalLim)
			if(isAnyPvalLow) {
				
				if(any(!is.na(aTagOut[isCurrentLocus]))) {
					# isNa = is.na(aTagOut[isCurrentLocus])
					isAvail = unlist(lapply(strsplit(aTagOut[isCurrentLocus],";"),function(x) any(x==tagTmp)))
					## NA, TRUE or FALSE
					aTagOut[isCurrentLocus] = ifelse(is.na(isAvail), tagTmp, ifelse(isAvail, aTagOut[isCurrentLocus], paste(aTagOut[isCurrentLocus],tagTmp, sep=";")))
				} else {
					aTagOut[isCurrentLocus] = tagTmp
				}
			}
		}
	}
	
	objGWA <- GWADATA.cbind(objGWA, aTagOut, colOutAnnot, blnOverwrite=TRUE)
	
	return(objGWA)
}

ANNOTATE <- function(strEqcCommand){ 
	## Wrapper for class definition
	ANNOTATEout <- setANNOTATE(new("ANNOTATE", strEqcCommand = strEqcCommand))
	validANNOTATE(ANNOTATEout)
	#ANNOTATEout.valid <- validANNOTATE(ANNOTATEout)
	return(ANNOTATEout)
}

