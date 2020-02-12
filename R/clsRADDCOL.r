setClass("RADDCOL",
	representation = representation(
						strEqcCommand		=	"character",
						rcdRAddCol			=	"character",
						colROut				=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						rcdRAddCol			=	"",
						colROut				=	""
						)
	#contains = c("EcfReader")
)

setGeneric("setRADDCOL", function(object) standardGeneric("setRADDCOL"))
setMethod("setRADDCOL", signature = (object = "RADDCOL"), function(object) {
	
	aEqcSlotNamesIn = c("rcdRAddCol", "colROut")

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
validRADDCOL <- function(objRCALC) {
	
	# if(length(objADDCOL@arcdAddCol)>length(objADDCOL@astrAddColNames)) 
		# stop(paste("EASY ERROR:ADDCOL\n No new Column name defined in astrAddColNames for RCD \n",objADDCOL@arcdAddCol[length(objADDCOL@astrAddColNames)+1]," !!!", sep=""))
	# if(length(objADDCOL@arcdAddCol)<length(objADDCOL@astrAddColNames)) 
		# stop(paste("EASY ERROR:ADDCOL\n No rcd defined in arcdAddCol for New Column \n",objADDCOL@astrAddColNames[length(objADDCOL@arcdAddCol)+1]," !!!", sep=""))
	
	if(objRCALC@colROut == "") 
		stop(paste(" EASY ERROR:RADDCOL\n No colROut defined for rcdRAddCol \n ",objRCALC@rcdRAddCol," \n Please set colROut.", sep=""))
	
	if(objRCALC@rcdRAddCol == "") 
		stop(paste(" EASY ERROR:RADDCOL\n No rcdRAddCol defined.\n Please set rcdRAddCol or remove RADDCOL function.", sep=""))
	
	
	return(TRUE)
}

#############################################################################################################################
RADDCOL.run <- function(objRCALC, objREPORT) {
			
	rcdRAddCol 		<- objRCALC@rcdRAddCol
	colROut 	<- objRCALC@colROut
	
	objRCD 	<- RCD(rcdRAddCol)
	out 	<- RCD.eval.report(objRCD, objREPORT)
	out		<- out[length(out)]
	
	objREPORT <- REPORT.addval(objREPORT,colROut,as.numeric(out))

	return(objREPORT)
}

RADDCOL <- function(strEqcCommand){ 
	## Wrapper for class definition
	RADDCOLout <- setRADDCOL(new("RADDCOL", strEqcCommand = strEqcCommand))
	validRADDCOL(RADDCOLout)
	#RADDCOLout.valid <- validRADDCOL(RADDCOLout)
	return(RADDCOLout)
}

