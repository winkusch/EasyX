setClass("ADJUSTALLELESSIMPLE",
	representation = representation(
						strEqcCommand		=	"character",
						colInA1				=	"character",
						colInA2				=	"character",
						acolInFreq			=	"character",
						acolInBeta			=	"character",
						acolInOr			=	"character",
						colRefA1			=	"character",
						colRefA2			=	"character",
						blnWriteMissingIn	=	"logical",
						blnRemoveMissingIn	=	"logical",
						blnWriteMissingRef	=	"logical",
						blnRemoveMissingRef	=	"logical",
						blnWriteMismatch	=	"logical",
						blnRemoveMismatch	=	"logical",
						strTag				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInA1				=	"",
						colInA2				=	"",
						acolInFreq			=	"",
						acolInBeta			=	"",
						acolInOr			=	"",
						colRefA1			=	"",
						colRefA2			=	"",
						blnWriteMissingIn	=	TRUE,
						blnRemoveMissingIn	=	FALSE,
						blnWriteMissingRef	=	TRUE,
						blnRemoveMissingRef	=	FALSE,
						blnWriteMismatch	=	TRUE,
						blnRemoveMismatch	=	FALSE,
						strTag				=	"AAS"
						)
)

setGeneric("setADJUSTALLELESSIMPLE", function(object) standardGeneric("setADJUSTALLELESSIMPLE"))
setMethod("setADJUSTALLELESSIMPLE", signature = (object = "ADJUSTALLELESSIMPLE"), function(object) {
	
	aEqcSlotNamesIn = c("colInA1", "colInA2", 
						"colRefA1", "colRefA2", 
						"acolInFreq","acolInBeta","acolInOr",
						"blnWriteMissingIn","blnRemoveMissingIn",
						"blnWriteMissingRef","blnRemoveMissingRef",
						"blnWriteMismatch","blnRemoveMismatch",
						"strTag"
						) 
					
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
ADJUSTALLELESSIMPLE.valid <- function(objAAS) {
	
	if(objAAS@colInA1 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n colInA1 is not being specified. \n PLease set colInA1.", sep=""))
	if(objAAS@colInA2 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n colInA2 is not being specified. \n PLease set colInA2.", sep=""))
	if(objAAS@colRefA1 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n colRefA1 is not being specified. \n PLease set colRefA1.", sep=""))
	if(objAAS@colRefA2 == "")
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n colRefA2 is not being specified. \n PLease set colRefA2.", sep=""))
	if(all(objAAS@acolInFreq==""))
		warning(paste(" EASY WARNING:ADJUSTALLELESSIMPLE\n No --acolInFreq defined. Alleles are adjusted to ref without flipping the frequency !!!!!", sep=""))
	if(all(objAAS@acolInBeta=="")&all(objAAS@acolInOr==""))
		warning(paste(" EASY WARNING:ADJUSTALLELESSIMPLE\n No --acolInBeta or --acolInOr defined. Alleles are adjusted to ref without flipping the effect or odds ratio !!!!!", sep=""))

	return(TRUE)
		
}

ADJUSTALLELESSIMPLE.GWADATA.valid <- function(objAAS, objGWA) {
	
	if(!(objAAS@colInA1 %in% objGWA@aHeader) & (objAAS@colInA1 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined column colInA1 \n",objAAS@colInA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAAS@colInA2 %in% objGWA@aHeader) & (objAAS@colInA2 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined column colInA2 \n",objAAS@colInA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAAS@colRefA1 %in% objGWA@aHeader) & (objAAS@colRefA1 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined column colRefA1 \n",objAAS@colRefA1, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(!(objAAS@colRefA2 %in% objGWA@aHeader) & (objAAS@colRefA2 != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined column colRefA2 \n",objAAS@colRefA2, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objAAS@acolInFreq %in% objGWA@aHeader)) & (objAAS@acolInFreq[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined acolInFreq columns \n",paste(objAAS@acolInFreq[which(!(objAAS@acolInFreq %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objAAS@acolInBeta %in% objGWA@aHeader)) & (objAAS@acolInBeta[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined acolInBeta columns \n",paste(objAAS@acolInBeta[which(!(objAAS@acolInBeta %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	if(any(!(objAAS@acolInOr %in% objGWA@aHeader)) & (objAAS@acolInOr[1] != ""))
		stop(paste(" EASY ERROR:ADJUSTALLELESSIMPLE\n Defined acolInOr columns \n",paste(objAAS@acolInOr[which(!(objAAS@acolInOr %in% objGWA@aHeader))],collapse=","), "\n are not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
}

#############################################################################################################################
ADJUSTALLELESSIMPLE.run <- function(objAAS, objGWA, objREPORT, isValidScript) {
	
	colInA1 <- objAAS@colInA1
	colInA2 <- objAAS@colInA2
	colRefA1 <- objAAS@colRefA1
	colRefA2 <- objAAS@colRefA2
	acolInFreq	<- objAAS@acolInFreq
	acolInBeta <- objAAS@acolInBeta
	acolInOr <- objAAS@acolInOr
	blnWriteMissingIn	<- objAAS@blnWriteMissingIn
	blnRemoveMissingIn <- objAAS@blnRemoveMissingIn
	blnWriteMissingRef	<- objAAS@blnWriteMissingRef
	blnRemoveMissingRef <- objAAS@blnRemoveMissingRef
	blnWriteMismatch <- objAAS@blnWriteMismatch
	blnRemoveMismatch <- objAAS@blnRemoveMismatch
	strTag <- objAAS@strTag
	
	####
	
	# isMissing = is.na(objGWA@tblGWA[[colInA1]])|is.na(objGWA@tblGWA[[colInA2]])|is.na(objGWA@tblGWA[[colRefA1]])|is.na(objGWA@tblGWA[[colRefA2]])
	isMissingIn = is.na(objGWA@tblGWA[[colInA1]])|is.na(objGWA@tblGWA[[colInA2]])
	isMissingRef = is.na(objGWA@tblGWA[[colRefA1]])|is.na(objGWA@tblGWA[[colRefA2]])
	isOk = !isMissingIn & !isMissingRef & objGWA@tblGWA[[colInA1]]==objGWA@tblGWA[[colRefA1]] & objGWA@tblGWA[[colInA2]]==objGWA@tblGWA[[colRefA2]]
	isSwitched = !isMissingIn & !isMissingRef & objGWA@tblGWA[[colInA1]]==objGWA@tblGWA[[colRefA2]] & objGWA@tblGWA[[colInA2]]==objGWA@tblGWA[[colRefA1]]
	isMismatch = !isMissingIn & !isMissingRef & !isOk & !isSwitched
	
	### change allele dir:
	if(any(isSwitched)) {
		
		a1old <- objGWA@tblGWA[[colInA1]]
		a2old <- objGWA@tblGWA[[colInA2]]
		
		objGWA@tblGWA[[colInA1]] <- ifelse(isSwitched, a2old, a1old)
		objGWA@tblGWA[[colInA2]] <- ifelse(isSwitched, a1old, a2old)
		rm(a1old)
		rm(a2old)
		
		if(acolInFreq[1]!="") {
			for(colInFreq in acolInFreq) {
				freqold <- as.numeric(objGWA@tblGWA[[colInFreq]])
				objGWA@tblGWA[[colInFreq]] <- ifelse(isSwitched, 1-freqold,freqold)
			}
			rm(freqold)
		}
		if(acolInBeta[1]!="") {
			for(colInBeta in acolInBeta) {
				betaold <- as.numeric(objGWA@tblGWA[[colInBeta]])
				objGWA@tblGWA[[colInBeta]] <- ifelse(isSwitched, -betaold,betaold)
			}
			rm(betaold)
		}
		if(acolInOr[1]!="") {
			for(colInOr in acolInOr) {
				orold <- as.numeric(objGWA@tblGWA[[colInOr]])
				objGWA@tblGWA[[colInOr]] <- ifelse(isSwitched, 1/orold,orold)
			}
			rm(orold)
		}
	}
	
	### write report variables
	strPrefix <- ifelse(strTag!="",paste(strTag,".",sep=""),"")
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"Checked",sep=""),length(isOk))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"AlleleMatch",sep=""),length(which(isOk)))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"AlleleChange",sep=""),length(which(isSwitched)))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"AlleleMissingIn",sep=""),length(which(isMissingIn)))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"AlleleMissingRef",sep=""),length(which(isMissingRef)))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"AlleleMismatch",sep=""),length(which(isMismatch)))
	
	if(isValidScript) {
		### write output files
		if(objAAS@blnWriteMissingIn & any(isMissingIn)) {
			objGWA.Missing <- GWADATA.getrows(objGWA,which(isMissingIn))
			GWADATA.write(objGWA.Missing, strSuffix = paste(".",strPrefix,"missing.in",sep=""))
			rm(objGWA.Missing)
		}
		if(objAAS@blnWriteMissingRef & any(isMissingRef)) {
			objGWA.Missing <- GWADATA.getrows(objGWA,which(isMissingRef))
			GWADATA.write(objGWA.Missing, strSuffix = paste(".",strPrefix,"missing.ref",sep=""))
			rm(objGWA.Missing)
		}
		if(objAAS@blnWriteMismatch & any(isMismatch)) {
			objGWA.Mismatch <- GWADATA.getrows(objGWA,which(isMismatch))
			GWADATA.write(objGWA.Mismatch, strSuffix = paste(".",strPrefix,"mismatch",sep=""))
			rm(objGWA.Mismatch)
		}
	}
	
	return(list(objGWA,objREPORT))
	
}
#############################################################################################################################
ADJUSTALLELESSIMPLE <- function(strEqcCommand){ 
	## Wrapper for class definition
	ADJUSTALLELESSIMPLEout <- setADJUSTALLELESSIMPLE(new("ADJUSTALLELESSIMPLE", strEqcCommand = strEqcCommand))
	ADJUSTALLELESSIMPLE.valid(ADJUSTALLELESSIMPLEout)
	return(ADJUSTALLELESSIMPLEout)

}
