setClass("AFCHECK",
	representation = representation(
						strEqcCommand	=	"character",
						colInFreq		=	"character",
						colRefFreq		=	"character",
						blnWriteOutlier	=	"logical",
						blnRemoveOutlier=	"logical",
						numLimOutlier	=	"numeric",
						blnPlotAll		=	"logical",
						blnAdjAllele	=	"logical",
						strTag			=	"character"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						colInFreq			=	"", ## in class ADJUSTALLELES
						colRefFreq			=	"",
						blnWriteOutlier		=	TRUE,
						blnRemoveOutlier	=	FALSE,
						numLimOutlier		=	0.2,
						blnPlotAll			=	TRUE,
						blnAdjAllele		=	FALSE,
						strTag				=	"AFCHECK"
						),
	contains = c("SPLOT")
)

setGeneric("setAFCHECK", function(object) standardGeneric("setAFCHECK"))
setMethod("setAFCHECK", signature = (object = "AFCHECK"), function(object) {
	
	## Default settings for SPLOT that can be overwritten if set!:
	object@strDefaultColour 	<- "black"
	object@arcdAdd2Plot 		<- "abline(a=0,b=1,col='red',lty=1)"
	object@strAxes 				<- "lim(0,1,0,1)"
	object@numDefaultCex 		<- 0.1
	object@strMode 				<- "subplot"
	
	#object@blnInAll <- FALSE
	
	aEqcSlotNamesIn = c("colRefFreq", 
						"blnWriteOutlier", 
						"blnRemoveOutlier",
						"numLimOutlier",
						"blnPlotAll", 
						"colInFreq", 
						"strTag",
						# Inherited SPLOT: 
						"strDefaultColour",
						"numDefaultSymbol",
						"numDefaultCex",
						"arcdColourCrit", 
						"astrColour",
						"strAxes",
						"strXlab",
						"strYlab",
						"arcdAdd2Plot",
						"strFormat",
						"rcdExclude",
						"numCexAxis",
						"numCexLab",
						"numWidth",
						"numHeight",
						"anumParMar",
						"anumParMgp",
						"strParBty",
						"blnGrid",
						"strMode"
						#"strPlotName"
						)
						
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
		tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
		tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
		
		if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
	}
	
	object@rcdSPlotX <- object@colRefFreq
	object@rcdSPlotY <- object@colInFreq
	if(object@strXlab == "") object@strXlab <- object@rcdSPlotX
	if(object@strYlab == "") object@strYlab <- object@rcdSPlotY
	
	## UPDATE
	# object@blnInAll <- FALSE
	# object@blnRefAll <- FALSE
	## Allow setting it!
	
	object@strPlotName <- object@strTag
	
	if(!object@blnPlotAll)
		object@rcdExclude <- paste("abs(",object@colRefFreq,"-",object@colInFreq,")<=",object@numLimOutlier ,sep="")
		line1 <- paste("abline(a=",object@numLimOutlier,",b=1,col='orange',lty=1)",sep="")
		line2 <- paste("abline(a=",-object@numLimOutlier,",b=1,col='orange',lty=1)",sep="")
		object@arcdAdd2Plot <- c(object@arcdAdd2Plot,line1,line2)
	
	if(object@arcdColourCrit[1] == "") {
		object@arcdColourCrit <- paste("abs(",object@colRefFreq,"-",object@colInFreq,")>",object@numLimOutlier ,sep="")
		object@astrColour <- "red"
	}
	
	return(object)
})


#############################################################################################################################
validAFCHECK <- function(objAFCHECK) {
	## Paste validity checks specific for SPLOT
	## Pval <-> se
	## fileGcSnps exists
	## colMarkerGcSnps exists

	
	return(TRUE)
}

AFCHECK.GWADATA.valid <- function(objAFCHECK, objGWA){ 

	if(!(objAFCHECK@colRefFreq %in% objGWA@aHeader))
		stop(paste("EASY ERROR:AFCHECK\n Column colRefFreq\n",objAFCHECK@colRefFreq," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
	if(!(objAFCHECK@colInFreq %in% objGWA@aHeader))
		stop(paste("EASY ERROR:AFCHECK\n Column colInFreq\n",objAFCHECK@colInFreq," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
	
}
AFCHECK.run <- function(objAFCHECK, objGWA, objREPORT) {
	### Calculate correlation and save outliers
	
	#cor(EAF_QTL, Freq1.ref, use='na.or.complete')
	
	strPrefix = ifelse(objAFCHECK@strTag!="",paste(objAFCHECK@strTag,".",sep=""),"")
	
	rcdCorr 		<- paste("cor(",objAFCHECK@colRefFreq,",",objAFCHECK@colInFreq,",use='na.or.complete')", sep="")
	strCorrName 	<- paste(strPrefix,"cor_",objAFCHECK@colRefFreq,"_",objAFCHECK@colInFreq, sep="")
	
	objRCD 	<- RCD(rcdCorr)
	out 	<- RCD.eval(objRCD, objGWA)
	
	objREPORT <- REPORT.addval(objREPORT,strCorrName,out)
	
	##
	#objGWA.miss <- objGWA
	rcdOutlier		<- paste("abs(",objAFCHECK@colRefFreq,"-",objAFCHECK@colInFreq,")>",objAFCHECK@numLimOutlier ,sep="")
	
	objRCD 	<- RCD(rcdOutlier)
	out 	<- RCD.eval(objRCD, objGWA)
	
	numOutlier = length(which(out))
	objREPORT <- REPORT.addval(objREPORT,paste(strPrefix,"numOutlier",sep=""),numOutlier)
	
	if(objAFCHECK@blnWriteOutlier & numOutlier > 0) {
		objGWA.outlier <- GWADATA.getrows(objGWA, which(out))
		GWADATA.write(objGWA.outlier, strSuffix = paste(".",strPrefix,"outlier",sep=""))
	}
	if(objAFCHECK@blnRemoveOutlier) {
		objGWA <- GWADATA.removerows(objGWA, which(out))
	}
	
	return(list(objGWA,objREPORT))
}
AFCHECK <- function(strEqcCommand){ 
	## Wrapper for class definition
	
	AFCHECKout <- setAFCHECK(new("AFCHECK", strEqcCommand = strEqcCommand))
	validAFCHECK(AFCHECKout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(AFCHECKout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}
