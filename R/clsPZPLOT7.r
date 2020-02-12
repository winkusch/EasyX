setClass("PZPLOT",
	representation = representation(
						strEqcCommand	=	"character",
						colBeta			=	"character",
						colSe			=	"character",
						colPval			=	"character",
						numPvalOffset	=	"numeric"
						),
	prototype = prototype(
						strEqcCommand	=	"",
						colBeta			=	"",
						colSe			=	"",
						colPval			=	"",
						numPvalOffset	=	0.05
						),
	contains = c("SPLOT")
)

setGeneric("setPZPLOT", function(object) standardGeneric("setPZPLOT"))
setMethod("setPZPLOT", signature = (object = "PZPLOT"), function(object) {
	
	object@arcdAdd2Plot <- "abline(a=0,b=1,col='grey',lty=1)"
	object@numDefaultCex <- 0.3
	object@blnGrid <- TRUE
	object@strMode <- "subplot"
	object@strDefaultColour <- "black"
	object@strPlotName <- "PZ"
	object@strAxes <- "zeroequal"
	object@strPlotName <- "PZ-PLOTS"
	
	aEqcSlotNamesIn = c("colBeta",
						"colSe",
						"colPval",
						"numPvalOffset",
						# Inherited SPLOT: 
						"strDefaultColour",
						"numDefaultSymbol",
						"numDefaultCex",
						"arcdColourCrit", 
						"astrColour",
						"astrColourLegend",
						"arcdSymbolCrit",
						"anumSymbol",
						"astrSymbolLegend",
						"arcdCexCrit",
						"anumCex",
						"strAxes",
						"strXlab",
						"strYlab",
						"strTitle",
						#"blnLegend",
						"arcdAdd2Plot",
						"strMode",
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
						"strPlotName"
						)
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
		tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
		tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
		
		if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
	}
	if(object@rcdExclude == "")	object@rcdExclude <- paste(object@colPval,">=",object@numPvalOffset, sep="")
	else object@rcdExclude <- paste(object@rcdExclude, "|", paste(object@colPval,">=",object@numPvalOffset, sep=""),sep="")
	
	object@rcdSPlotX <- paste("-log10(",object@colPval,")", sep="")
	object@rcdSPlotY <- paste("-log10(2*pnorm(abs(",object@colBeta,"/",object@colSe,"),lower.tail=FALSE))", sep="")
	if(object@strXlab == "") object@strXlab <- object@rcdSPlotX
	if(object@strYlab == "") object@strYlab <- "-log10(P.Zstat)"
	
	return(object)
})


#############################################################################################################################
validPZPLOT <- function(objPZPLOT) {
	## Paste validity checks specific for SPLOT
	## Pval <-> se
	## fileGcSnps exists
	## colMarkerGcSnps exists

	
	return(TRUE)
}

PZPLOT.GWADATA.valid <- function(objPZPLOT, objGWA){ 

	if(!(objPZPLOT@colBeta %in% objGWA@aHeader))
		stop(paste("EASY ERROR:PZPLOT\n Column colBeta\n",objPZPLOT@colBeta," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
	if(!(objPZPLOT@colSe %in% objGWA@aHeader))
		stop(paste("EASY ERROR:PZPLOT\n Column colSe\n",objPZPLOT@colSe," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
	if(!(objPZPLOT@colPval %in% objGWA@aHeader))
		stop(paste("EASY ERROR:PZPLOT\n Column colPval\n",objPZPLOT@colPval," does not exist in file\n",objGWA@fileInShortName,"\n !!!", sep=""))
		
}

PZPLOT <- function(strEqcCommand){ 
	## Wrapper for class definition

	PZPLOTout <- setPZPLOT(new("PZPLOT", strEqcCommand = strEqcCommand))
	validPZPLOT(PZPLOTout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(PZPLOTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}
