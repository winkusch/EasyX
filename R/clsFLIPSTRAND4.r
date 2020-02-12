setClass("FLIPSTRAND",
	representation = representation(
						strEqcCommand			=	"character",
						colInMarker				=	"character",
						colInStrand				=	"character",
						fileRef					=	"character",
						colRefMarker			=	"character",
						strTag					=	"character"
						),
	prototype = prototype(
						colInMarker				=	"",
						colInStrand				=	"",
						fileRef					=	"",
						colRefMarker			=	"",
						strTag					=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setFLIPSTRAND", function(object) standardGeneric("setFLIPSTRAND"))
setMethod("setFLIPSTRAND", signature = (object = "FLIPSTRAND"), function(object) {
	
	aEqcSlotNamesIn = c("colInMarker", "colInStrand", "fileRef" , "colRefMarker","strTag")
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

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
validFLIPSTRAND <- function(objFLIPSTRAND) {
	## Paste validity checks specific for FLIPSTRAND
	## Pval <-> se
	## fileGcSnps exists
	## colMarkerGcSnps exists
	
	if(objFLIPSTRAND@colInStrand == "") 
		stop(paste(" EASY ERROR:FLIPSTRAND\n No column colInStrand defined for FLIPSTRAND.\n Please set colInStrand.", sep=""))
	
	if(objFLIPSTRAND@colInMarker == "") 
		stop(paste(" EASY ERROR:FLIPSTRAND\n No column colInMarker defined for FLIPSTRAND.\n Please set colInMarker.", sep=""))
	
	if(objFLIPSTRAND@colRefMarker == "") 
		stop(paste(" EASY ERROR:FLIPSTRAND\n No column colRefMarker defined for FLIPSTRAND.\n Please set colRefMarker.", sep=""))
	
	if(objFLIPSTRAND@fileRef == "") 
		stop(paste(" EASY ERROR:FLIPSTRAND\n No reference file defined. Please set fileRef.", sep=""))
	
	return(TRUE)
}

FLIPSTRAND.GWADATA.valid <- function(objFLIPSTRAND, objGWA) {
	
	isAv <- objFLIPSTRAND@colInMarker %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:FLIPSTRAND\n Defined column colInMarker \n",objFLIPSTRAND@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))

}

#############################################################################################################################
FLIPSTRAND.run <- function(objFLIPSTRAND, objGWA, objREPORT) {
		
	colInMarker		=	objFLIPSTRAND@colInMarker
	colInStrand		=	objFLIPSTRAND@colInStrand
	fileRef			=	objFLIPSTRAND@fileRef
	strTag			=	objFLIPSTRAND@strTag
	colRefMarker	=	objFLIPSTRAND@colRefMarker
	
	tblRef<-read.table(objFLIPSTRAND@fileRef, header=T, sep="\t", stringsAsFactors=FALSE)
	
	if(!colRefMarker%in%names(tblRef)) 
		stop(paste(" EASY ERROR:FLIPSTRAND\n Defined column colRefMarker \n",colRefMarker, "\n is not available in reference data-set \n",fileRef,"\n PLease specify correct column name.", sep=""))
	
	aRefMarker = as.character(tblRef[, which(names(tblRef) == colRefMarker)])
	
	aInMarker = GWADATA.getcol(objGWA, colInMarker)
	idxStrand = match(colInStrand, objGWA@aHeader)
	aStrand = GWADATA.getcol(objGWA, colInStrand)
	
	isSwitchStrand = aInMarker%in%aRefMarker 
	isPlus = aStrand == "+"
	isMinus = aStrand == "-"
	
	aStrand[which(isSwitchStrand & isPlus)] <- "-"
	aStrand[which(isSwitchStrand & isMinus)] <- "+"
	
	objGWA@tblGWA[[idxStrand]] <- aStrand
	
	strTag <- ifelse(strTag!="", paste(strTag, ".", sep=""), "")
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numFlipStrand",sep=""), as.character(length(which(isSwitchStrand))))
	lsOut <- list(objGWA, objREPORT)
	
	return(lsOut)
}

FLIPSTRAND <- function(strEqcCommand){ 
	## Wrapper for class definition
	FLIPSTRANDout <- setFLIPSTRAND(new("FLIPSTRAND", strEqcCommand = strEqcCommand))
	validFLIPSTRAND(FLIPSTRANDout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(FLIPSTRANDout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

# setValidity("ADDCOL", function(object){
	# print("ADDCOL-CHECK")
	
	
	
	# print(TRUE)
	# return(TRUE)
# })

