setClass("REMOVESNPS",
	representation = representation(
						strEqcCommand	=	"character",
						colInMarker		=	"character",
						fileRef			=	"character",
						colRefMarker	=	"character", 
						strTag			=	"character"
						),
	prototype = prototype(
						strEqcCommand	=	"",
						colInMarker		=	"",
						fileRef			=	"",
						colRefMarker	=	"SNP",
						strTag			=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setREMOVESNPS", function(object) standardGeneric("setREMOVESNPS"))
setMethod("setREMOVESNPS", signature = (object = "REMOVESNPS"), function(object) {
	
	aEqcSlotNamesIn = c("colInMarker","fileRef","colRefMarker","strTag")

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
validREMOVESNPS <- function(objREMOVESNPS) {
	
	if(objREMOVESNPS@colInMarker == "") 
		stop(paste(" EASY ERROR:REMOVESNPS\n No column colInMarker defined. Please set colInMarker.", sep=""))
	if(objREMOVESNPS@fileRef == "") 
		stop(paste(" EASY ERROR:REMOVESNPS\n No reference file defined. Please set fileRef.", sep=""))
	
	if(!file.exists(objREMOVESNPS@fileRef))
			stop(paste("EASY ERROR:REMOVESNPS\n File fileRef\n ",objREMOVESNPS@fileRef,"\n does not exist.", sep=""))
		### Cols exist?
		
	tblRef<-read.table(objREMOVESNPS@fileRef, header=T, sep="",  nrows=1, stringsAsFactors=FALSE)
		
	isAv <- objREMOVESNPS@colRefMarker %in% names(tblRef)
	if(!isAv)
		stop(paste(" EASY ERROR:REMOVESNPS\n Defined column colRefMarker \n",objREMOVESNPS@colRefMarker, "\n is not available in fileRef. PLease specify correct column name.", sep=""))
	
	return(TRUE)
}
REMOVESNPS.GWADATA.valid <- function(objREMOVESNPS, objGWA) {
	
	isNotAv <- !(objREMOVESNPS@colInMarker %in% objGWA@aHeader)
	if(isNotAv)
		stop(paste(" EASY ERROR:REMOVESNPS\n Defined column colInMarker \n",objREMOVESNPS@colInMarker, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
	
	return(TRUE)
}
#############################################################################################################################
REMOVESNPS.run <- function(objREMOVESNPS, objGWA, objREPORT) {
	
	colInMarker		=	objREMOVESNPS@colInMarker
	fileRef 		=	objREMOVESNPS@fileRef
	colRefMarker	=	objREMOVESNPS@colRefMarker
	strTag			=	objREMOVESNPS@strTag
	
	tblRef <- read.table(objREMOVESNPS@fileRef, header=T, sep="", stringsAsFactors=FALSE)
	
	iMarker = match(colRefMarker, names(tblRef))
	aRefMarker = tblRef[,iMarker]
	
	aInMarker = GWADATA.getcol(objGWA, colInMarker)
	
	iRemove = which(aInMarker%in%aRefMarker)
	
	objGWA.Removed <- GWADATA.copy(objGWA)
	
	if(length(iRemove>0)) {
		numRemove = length(iRemove)
		if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
		objREPORT <- REPORT.addval(objREPORT,paste(objREMOVESNPS@strTag,"numRemove",sep=""),numRemove)
		objGWA.Removed <- GWADATA.getrows(objGWA, iRemove)
		objGWA <- GWADATA.removerows(objGWA, iRemove)
	}
	
	lsOut <- list(objGWA, objREPORT, objGWA.Removed)
	
	return(lsOut)
	
}

REMOVESNPS <- function(strEqcCommand){ 
	## Wrapper for class definition
	REMOVESNPSout <- setREMOVESNPS(new("REMOVESNPS", strEqcCommand = strEqcCommand))
	validREMOVESNPS(REMOVESNPSout)
	#REMOVESNPSout.valid <- validREMOVESNPS(REMOVESNPSout)
	return(REMOVESNPSout)
}

