setClass("CLEANDUPLICATES",
	representation = representation(
						strEqcCommand		=	"character",
						colInMarker			=	"character",
						strMode				=	"character",
						colN				=	"character",						
						colCrit				=	"character",
						strCritDir			=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInMarker			=	"",
						strMode				=	"keepfirst",
						colN				=	"",
						colCrit				=	"",
						strCritDir			=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setCLEANDUPLICATES", function(object) standardGeneric("setCLEANDUPLICATES"))
setMethod("setCLEANDUPLICATES", signature = (object = "CLEANDUPLICATES"), function(object) {
	
	aEqcSlotNamesIn = c("colInMarker","strMode","colN","colCrit","strCritDir")
	
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
validCLEANDUPLICATES <- function(objCD) {
	
	if(!(objCD@strMode == "keepfirst" | objCD@strMode == "removeall" | objCD@strMode == "keepall" | objCD@strMode == "samplesize" | objCD@strMode == "criterion" ))
		stop(paste(" EASY ERROR:CLEANDUPLICATES\n Wrong strMode defined.\n Please use 'keepfirst', 'removeall', 'keepall', 'samplesize' or 'criterion'.", sep=""))
	
	return(TRUE)
}
CLEANDUPLICATES.GWADATA.valid <- function(objCD, objGWA) {
	
	if(objCD@strMode == "samplesize") {
		isOk = objCD@colN%in%objGWA@aHeader
		if(!isOk)
			stop(paste(" EASY ERROR:CLEANDUPLICATES\n In order to use 'samplesize' mode, you have to define 'colN' properly.\n Please check 'colN'!", sep=""))
	}
	if(objCD@strMode == "criterion") {
		isOk = objCD@colCrit%in%objGWA@aHeader
		if(!isOk)
			stop(paste(" EASY ERROR:CLEANDUPLICATES\n In order to use 'criterion' mode, you have to define 'colCrit' properly.\n Please check 'colCrit'!", sep=""))
		if(!(objCD@strCritDir=="min"|objCD@strCritDir=="max")) 
			stop(paste(" EASY ERROR:CLEANDUPLICATES\n Wrong strCritDir defined.\n Please use 'min' or 'max'.", sep=""))
	}
	
	return(TRUE)
}


#############################################################################################################################
CLEANDUPLICATES.run <- function(objCD, objGWA, objREPORT) {
	
	strMode = objCD@strMode
	
	iMarker = match(objCD@colInMarker, objGWA@aHeader)[1]
	
	if(is.na(iMarker)) 
		stop(paste(" EASY ERROR:CLEANDUPLICATES\n Column colInMarker \n ",objCD@colInMarker," does not exist in the data set.\n Please check the Marker column name.", sep=""))
	
	if(strMode == "samplesize") {
		# order tbl and reset mode to keepfirst
		iN = which(objGWA@aHeader == objCD@colN)
		iOrder = order(objGWA@tblGWA[[iN]], decreasing=TRUE)
		objGWA@tblGWA <- objGWA@tblGWA[iOrder,]
		strMode <- "keepfirst"
	}
	if(strMode == "criterion") {
		# order tbl and reset mode to keepfirst
		iCrit = which(objGWA@aHeader == objCD@colCrit)
		if(objCD@strCritDir == "min") {
			## keep minimum of both 
			iOrder = order(objGWA@tblGWA[[iCrit]], decreasing=FALSE)
		} else {
			## keep maximum of both 
			iOrder = order(objGWA@tblGWA[[iCrit]], decreasing=TRUE)
		}
		objGWA@tblGWA <- objGWA@tblGWA[iOrder,]
		strMode <- "keepfirst"
	}
	
	isDuplicated = duplicated(objGWA@tblGWA[[iMarker]])
	iSecondDuplicates = which(isDuplicated)
	
	aSNPduplicated 	= objGWA@tblGWA[[iMarker]][isDuplicated]
	iAllDuplicates 	= which(!is.na(match(objGWA@tblGWA[[iMarker]],aSNPduplicated)))
	
	objGWA.Duplicates <- GWADATA.getrows(objGWA, iAllDuplicates)
	#if(length(iAllDuplicates)>0) GWADATA.write(objGWA.Duplicates, "duplicates", "2")
	if(length(iAllDuplicates)>0) GWADATA.write(objGWA.Duplicates, strSuffix = paste(".duplicates.",objCD@colInMarker,sep=""))
	
	if(strMode == "keepfirst" & any(iSecondDuplicates)) 	objGWA <- GWADATA.removerows(objGWA, -iSecondDuplicates)
	else if(strMode == "removeall" & any(iAllDuplicates)) 	objGWA <- GWADATA.removerows(objGWA, -iAllDuplicates)
	
	numDuplicates = length(iSecondDuplicates)
	
	objREPORT <- REPORT.addval(objREPORT,paste("numDuplicates",objCD@colInMarker,sep="."), numDuplicates)
	#objREPORT <- REPORT.addval(objREPORT,"numColAddNACol", as.character(numColAddNACol))
	
	lsOut <- list(objGWA, objREPORT)
	
	return(lsOut)
	
}

CLEANDUPLICATES <- function(strEqcCommand){ 
	## Wrapper for class definition
	CLEANDUPLICATESout <- setCLEANDUPLICATES(new("CLEANDUPLICATES", strEqcCommand = strEqcCommand))
	validCLEANDUPLICATES(CLEANDUPLICATESout)
	#CLEANDUPLICATESout.valid <- validCLEANDUPLICATES(CLEANDUPLICATESout)
	return(CLEANDUPLICATESout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

# setValidity("CLEANDUPLICATES", function(object){
	# print("CLEANDUPLICATES-CHECK")
	
	
	
	# print(TRUE)
	# return(TRUE)
# })

