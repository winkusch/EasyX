setClass("FORESTPLOT",
	representation = representation(
						strEqcCommand		=	"character",
						### general params:
						rcdCriterion		=	"character",
						acolBeta			=	"character",
						acolSe				=	"character", 	
						acolOr				=	"character",
						acolCilow			=	"character",
						acolCiupp			=	"character",
						astrColour			=	"character",	## per acolBeta
						astrColourCi		=	"character",	## per acolBeta
						astrSymbol			=	"character",	## per acolBeta
						#anumSymbol			=	"numeric",		## per acolBeta
						#anumCex			=	"numeric",		## per acolBeta
						anumXAxisLim		=	"numeric",
						strXlab				=	"character",
						numCexXLabel		=	"numeric",
						numCexXTicks		=	"numeric",
						arcdAdd2Plot		=	"character",
						acolText			=	"character",	## these are the columns of text to be added on the left side 
						numCexText			=	"numeric",
						acolTextLabel 		=	"character",
						blnLegend			=	"logical",
						astrLegendText		=	"character",
						strTitle			=	"character",
						strFormat			=	"character",
						numWidth			=	"numeric",
						numHeight			=	"numeric",
						strPlotName			= 	"character",
						anumParMar			=	"numeric",
						anumParMgp			=	"numeric",
						strParBty			=	"character",
						numParLas			=	"numeric",
						strTag				=	"character",
						isBeta				=	"logical",
						isSe				=	"logical",
						isXlog				= 	"logical",
						colSummary			= 	"character",
						numBoxsize			=	"numeric"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						## general params
						rcdCriterion		=	"",
						acolBeta			=	"",
						acolSe				=	"", 
						acolOr				=	"",
						acolCilow			=	"",
						acolCiupp			=	"",						
						astrColour			=	"black",	## per acolBeta
						astrColourCi		=	"darkgrey",	## per acolBeta
						astrSymbol			=	"square",	## per acolBeta
						#anumSymbol			=	1,		## per acolBeta
						#anumCex			=	1,		## per acolBeta
						anumXAxisLim		=	-1,
						strXlab				=	"Beta",
						numCexXLabel		=	1,
						numCexXTicks		=	1,
						arcdAdd2Plot		=	"",
						acolText			=	"",
						numCexText			=	1,
						acolTextLabel		=	"",
						blnLegend			=	TRUE,
						astrLegendText		=	"",
						strTitle			=	"",
						strFormat			=	"png",
						numWidth			=	600,
						numHeight			=	800,
						strPlotName			= 	"fp",
						anumParMar			=	c(5.1, 4.1, 4.1, 2.1),
						anumParMgp			=	c(3, 1, 0),
						strParBty			=	"o",
						numParLas			=	3,
						strTag				=	"",
						isBeta				= 	TRUE,
						isSe				= 	TRUE,
						isXlog				= 	FALSE,
						colSummary			= 	"",
						numBoxsize			=	-1
						)
	#contains = c("EcfReader")
)

setGeneric("setFORESTPLOT", function(object) standardGeneric("setFORESTPLOT"))
setMethod("setFORESTPLOT", signature = (object = "FORESTPLOT"), function(object) {
	
	aEqcSlotNamesIn = c("rcdCriterion",
						"acolBeta",
						"acolSe",
						"acolOr",
						"acolCilow",
						"acolCiupp",		
						"astrLegendText",
						"astrColour",
						"astrColourCi",
						"astrSymbol",
						"anumSymbol",
						"anumCex",
						"acolText",
						"numCexText",
						"anumXAxisLim",
						"strXlab",
						"numCexXLabel",
						"numCexXTicks",
						"arcdAdd2Plot",
						"blnLegend",
						"strTitle",
						"strFormat",
						"numWidth",
						"numHeight",
						"strPlotName",
						"anumParMar",
						"anumParMgp",
						"strParBty",
						"numParLas",
						"strTag",
						"acolTextLabel",
						"colSummary",
						"numBoxsize")

	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			# if(tmpSlot == "arcdExclude") tmpSlotVal[is.na(tmpSlotVal)]=""
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	isDefaultPngSize = object@numWidth == 600 & object@numHeight == 800
	if(object@strFormat == "pdf" & isDefaultPngSize) {
		object@numWidth <- 6
		object@numHeight <- 8
	}
	
	if(object@strTag != "") object@strPlotName <- paste(object@strPlotName,object@strTag,sep=".")
	
	return(object)
})

#############################################################################################################################
validFORESTPLOT <- function(objFP) {
	## Paste validity checks specific for FORESTPLOT
	
	if(!(objFP@strFormat %in% c("png","pdf","tiff","jpeg")))
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong strFormat defined, PLease use 'png','pdf','tiff' or 'jpeg' \n !!!", sep=""))
			
	if(objFP@numCexText <= 0) 
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong numCexText defined, PLease use numCexText > 0 \n !!!", sep=""))
		
	if(objFP@numCexXLabel <= 0) 
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong numCexXLabel defined, PLease use numCexXLabel > 0 \n !!!", sep=""))
	
	if(objFP@numCexXTicks <= 0) 
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong numCexXTicks defined, PLease use numCexXTicks > 0 \n !!!", sep=""))
	
	if(length(objFP@anumParMar) != 4) 
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong anumParMar defined, Please use anumParMar of length 4 (Default=5.1;4.1;4.1;2.1) \n !!!", sep=""))				
	
	if(length(objFP@anumParMgp) != 3) 
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong anumParMgp defined, Please use anumParMgp of length 3 (Default=3;1;0) \n !!!", sep=""))				
		
	if(!(objFP@strParBty%in%c("o","l","7","c","u","]")))
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong strParBty defined, Please use 'o','l','7','c','u' or ']' \n !!!", sep=""))				

	if(all(objFP@acolTextLabel!="")) {
		if(length(objFP@acolTextLabel) != length(objFP@acolText)) {
			stop(paste("EASY ERROR:FORESTPLOT\n Length of acolTextLabel must match length of acolText \n !!!", sep=""))
		}
	}
	
	if(any(!(objFP@astrSymbol%in%c("square","circle"))))
		stop(paste("EASY ERROR:FORESTPLOT\n Wrong astrSymbol defined, Please use 'square','circle' \n !!!", sep=""))
	
	return(TRUE)
}

FORESTPLOT.GWADATA.valid <- function(objFP, objGWA) {
	
	if(objFP@acolOr[1] != "") objFP@isBeta = FALSE
	if(objFP@acolCilow[1] != "") objFP@isSe = FALSE
	
	if(objFP@isBeta) {
		
		numGroups = length(objFP@acolBeta)
		
		if(all(objFP@astrLegendText!="")) {
			if(length(objFP@astrLegendText) != numGroups) {
				stop(paste("EASY ERROR:FORESTPLOT\n Length of astrLegendText must match length of acolBeta \n !!!", sep=""))
			}
		}
		if(objFP@astrLegendText[1]=="") objFP@astrLegendText = objFP@acolBeta
	
		if(length(objFP@astrSymbol) == 1) objFP@astrSymbol = rep(objFP@astrSymbol[1],numGroups)

		if(length(objFP@astrSymbol)!=numGroups) 
			stop(paste("EASY ERROR:FORESTPLOT\n Length of astrSymbol must match length of acolBeta \n !!!", sep=""))		
	
		aisMatch = objFP@acolBeta %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolBeta[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		
		
		if(objFP@isSe) {
			if(length(objFP@acolSe) != numGroups) 
				stop(paste("EASY ERROR:FORESTPLOT\n Length of acolSe must match length of acolBeta \n !!!", sep=""))
			
			aisMatch = objFP@acolSe %in% objGWA@aHeader
			if(!all(aisMatch))
				stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolSe[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		
		} else {
			if(length(objFP@acolCilow) != numGroups | length(objFP@acolCiupp) != numGroups)
				stop(paste("EASY ERROR:FORESTPLOT\n Length of acolCilow and acolCiupp must match length of acolBeta \n !!!", sep=""))
			
			aisMatch = objFP@acolCilow %in% objGWA@aHeader
			if(!all(aisMatch))
				stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolCilow[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
			
			aisMatch = objFP@acolCiupp %in% objGWA@aHeader
			if(!all(aisMatch))
				stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolCiupp[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		}

		
	} else {
		
		objFP@isXlog <- TRUE
		if(objFP@strXlab == "Beta") objFP@strXlab <- "OR"
		
		numGroups = length(objFP@acolOr)
		
		if(all(objFP@astrLegendText!="")) {
			if(length(objFP@astrLegendText) != numGroups) {
				stop(paste("EASY ERROR:FORESTPLOT\n Length of astrLegendText must match length of acolOr \n !!!", sep=""))
			}
		}
		
		if(objFP@astrLegendText[1]=="") objFP@astrLegendText = objFP@acolOr
		
		if(length(objFP@astrSymbol) == 1) objFP@astrSymbol = rep(objFP@astrSymbol[1],numGroups)

		if(length(objFP@astrSymbol)!=numGroups) 
			stop(paste("EASY ERROR:FORESTPLOT\n Length of astrSymbol must match length of acolBeta \n !!!", sep=""))	
		
		aisMatch = objFP@acolOr %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolOr[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		
		if(length(objFP@acolCilow) != numGroups) 
			stop(paste("EASY ERROR:FORESTPLOT\n Length of acolCilow must match length of acolOr \n !!!", sep=""))
				
		if(length(objFP@acolCiupp) != numGroups) 
			stop(paste("EASY ERROR:FORESTPLOT\n Length of acolCiupp must match length of acolOr \n !!!", sep=""))
		
		aisMatch = objFP@acolCilow %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolCilow[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
		
		aisMatch = objFP@acolCiupp %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolCiupp[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
			
	}
	
	
	
	if(length(objFP@anumXAxisLim)!=2 & all(objFP@anumXAxisLim!=-1)) {
		stop(paste("EASY ERROR:FORESTPLOT\n Length of anumXAxisLim must be 2: c(x_min,x_max). \n !!!", sep=""))
	}

	if(objFP@acolText[1]!="") {
	aisMatch = objFP@acolText %in% objGWA@aHeader
	if(!all(aisMatch))
		stop(paste("EASY ERROR:FORESTPLOT\n Column \n",paste(objFP@acolText[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	}
	
	if(all(objFP@acolTextLabel=="") & all(objFP@acolText!="")) {
		## set label to col Name 
		objFP@acolTextLabel = objFP@acolText
	}
	
	if(objFP@colSummary!="" & !objFP@colSummary%in%objGWA@aHeader) 
		stop(paste("EASY ERROR:FORESTPLOT\n Column \n",objFP@colSummary," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	return(objFP)
	
}


FORESTPLOT.run <- function(objFP, objGWA, objREPORT) {
	#function(tblPlot,acolQQPlot,astrColour,blnCombined,rcdNumCIBounds,fileOut)
	
	rcdCriterion 	<- objFP@rcdCriterion
	acolBeta 		<- objFP@acolBeta
	acolSe 			<- objFP@acolSe
	acolOr			<- objFP@acolOr
	acolCilow		<- objFP@acolCilow
	acolCiupp		<- objFP@acolCiupp
	astrLegendText 	<- objFP@astrLegendText
	astrColour 		<- objFP@astrColour
	astrColourCi	<- objFP@astrColourCi
	astrSymbol		<- objFP@astrSymbol
	#anumSymbol 	<- objFP@anumSymbol
	#anumCex 		<- objFP@anumCex
	acolText 		<- objFP@acolText
	numCexText 		<- objFP@numCexText
	strXlab 		<- objFP@strXlab
	numCexXLabel 	<- objFP@numCexXLabel
	numCexXTicks 	<- objFP@numCexXTicks
	arcdAdd2Plot 	<- objFP@arcdAdd2Plot
	blnLegend 		<- objFP@blnLegend
	strTitle 		<- objFP@strTitle
	strFormat 		<- objFP@strFormat
	numWidth 		<- objFP@numWidth
	numHeight 		<- objFP@numHeight
	strPlotName 	<- objFP@strPlotName
	anumParMar 		<- objFP@anumParMar
	anumParMgp 		<- objFP@anumParMgp
	strParBty 		<- objFP@strParBty
	numParLas 		<- objFP@numParLas
	acolTextLabel 	<- objFP@acolTextLabel
	strTag 			<- objFP@strTag
	anumXAxisLim	<- objFP@anumXAxisLim
	isBeta			<- objFP@isBeta
	isSe			<- objFP@isSe
	isXlog			<- objFP@isXlog
	colSummary		<- objFP@colSummary
	numBoxsize		<- objFP@numBoxsize
	############################
	
	###### Apply criterion
	
	if(rcdCriterion != "") {
		objRCD 	<- RCD(rcdCriterion)
		out 	<- RCD.eval(objRCD, objGWA)
		numCrit = length(which(out))
		objGWA.crit <- GWADATA.getrows(objGWA, which(out))
	} else {
		numCrit <- nrow(objGWA@tblGWA)
		objGWA.crit <- objGWA
	}
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	###### Copy plotting data.frame
	
	tblPlot <- as.data.frame(objGWA.crit@tblGWA,stringsAsFactors=FALSE)
	
	numGroups = ifelse(isBeta,length(acolBeta),length(acolOr))
	numRowVals = nrow(tblPlot)
	
	
	
	###### set graphical params to default if unset explicitly
	
	
	if(length(astrColour) == 1 & numGroups>1) astrColour = rep(astrColour , length(acolBeta))
	if(length(astrColourCi) == 1 & numGroups>1) astrColourCi = rep(astrColourCi , length(acolBeta))
	
	# if(length(anumSymbol) == 1 & length(acolBeta)>1)
		# anumSymbol 	= rep(anumSymbol , length(acolBeta))
		
	# if(length(anumCex) == 1 & length(acolBeta)>1)
		# anumCex = rep(anumCex , length(acolBeta))
	
	### Reset some graphical params
	
	if(strTitle == "") {
		plot.title = objGWA@fileInShortName
	} else { 
		plot.title = strTitle
	}
	
	#### Create plotting tables
	
	if(isBeta) {
		if(isSe) {
			## create ci columns
			acolCilow <- acolCiupp <- c()
			for(i in 1:numGroups) {
				cilow = tblPlot[,acolBeta[i]]-1.96*tblPlot[,acolSe[i]]
				tblPlot = cbind(tblPlot,cilow)
				names(tblPlot)[ncol(tblPlot)] = paste(acolBeta[i],".cilow.fptmp",sep="")
				acolCilow = c(acolCilow, paste(acolBeta[i],".cilow.fptmp",sep=""))
				
				ciupp = tblPlot[,acolBeta[i]]+1.96*tblPlot[,acolSe[i]]
				tblPlot = cbind(tblPlot,ciupp)
				names(tblPlot)[ncol(tblPlot)] = paste(acolBeta[i],".ciupp.fptmp",sep="")
				acolCiupp = c(acolCiupp, paste(acolBeta[i],".ciupp.fptmp",sep=""))
			}
		}
	} 
	
	if(isBeta) {
		tMean = as.data.frame(tblPlot[,acolBeta],stringsAsFactors=FALSE)
		names(tMean) = acolBeta
	} else {
		tMean = as.data.frame(tblPlot[,acolOr],stringsAsFactors=FALSE)
		names(tMean) = acolOr
	}
	
	tCiLow = as.data.frame(tblPlot[,acolCilow],stringsAsFactors=FALSE)
	names(tCiLow) = acolCilow
	tCiUpp = as.data.frame(tblPlot[,acolCiupp],stringsAsFactors=FALSE)
	names(tCiUpp) = acolCiupp
	
	if(colSummary!="") {
		aisSummary = as.logical(tblPlot[,colSummary])
	} else {
		aisSummary = rep(FALSE,numRowVals)
	}
	
	tText = as.data.frame(tblPlot[,acolText],stringsAsFactors=FALSE)
	names(tText) = acolText
	
	isAddTextLabel = all(acolTextLabel!="")
	
	if(isAddTextLabel) {
		
		## do not add this to tPlot because it might change the class of a numeric column
		
		tblLabel  = as.data.frame(t(rep(NA,ncol(tText))))
		names(tblLabel) = names(tText)
		tblLabel[1,acolText] = t(acolTextLabel)
		tText = rbind(tblLabel,tText,stringsAsFactors=FALSE)
		
		tblLabel  = as.data.frame(t(rep(NA,ncol(tMean))))
		names(tblLabel) = names(tMean)
		tMean = rbind(tblLabel,tMean,stringsAsFactors=FALSE)
		
		tblLabel  = as.data.frame(t(rep(NA,ncol(tCiLow))))
		names(tblLabel) = names(tCiLow)
		tCiLow = rbind(tblLabel,tCiLow,stringsAsFactors=FALSE)
		
		tblLabel  = as.data.frame(t(rep(NA,ncol(tCiUpp))))
		names(tblLabel) = names(tCiUpp)
		tCiUpp = rbind(tblLabel,tCiUpp,stringsAsFactors=FALSE)
		
		aisSummary = c(TRUE,aisSummary)
		
	}
		
	if(all(anumXAxisLim==-1)) {
		anumXAxisLim[1] = min(tCiLow,na.rm=T)
		anumXAxisLim[2] = max(tCiUpp,na.rm=T)
	}
	
	if(length(astrLegendText)==1) blnLegend = FALSE ## otherwise get the closure err
	
	fpcommand = "forestplot(tText, 
					mean = tMean,
					lower = tCiLow,
					upper = tCiUpp,
					is.summary = aisSummary,
					col = fpColors(box=astrColour, 
									lines = astrColourCi,
									summary = astrColour),
					ci.vertices=TRUE,
					clip = anumXAxisLim,
					xlab=strXlab,
					txt_gp = fpTxtGp(label = list(gpar(cex=numCexText)),
									ticks = gpar(cex=numCexXTicks),
									xlab  = gpar(cex = numCexXLabel),
									summary = gpar(fontface='bold')),
					hrzl_lines = gpar(col='grey'),
					xlog=isXlog
				)"
	
	if(blnLegend) fpcommand = paste(substring(fpcommand,1,nchar(fpcommand)-1),"legend = astrLegendText)",sep=",")
	if(numBoxsize != -1) fpcommand = paste(substring(fpcommand,1,nchar(fpcommand)-1),"boxsize = numBoxsize)",sep=",")
	if(any(astrSymbol!="square")) {
		scommand = paste(astrSymbol,collapse=",")
		scommand = gsub("square","fpDrawNormalCI",scommand)
		scommand = gsub("circle","fpDrawCircleCI",scommand)
		scommand = paste("fn.ci_norm=c(",scommand,")",sep="")
		# fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI)
		fpcommand = paste(substring(fpcommand,1,nchar(fpcommand)-1),scommand,")",sep=",")
	}
	
	eval(parse(text = fpcommand))
	
	return(TRUE)
	
}


FORESTPLOT <- function(strEqcCommand){ 
	## Wrapper for class definition
	FORESTPLOTout <- setFORESTPLOT(new("FORESTPLOT", strEqcCommand = strEqcCommand))
	validFORESTPLOT(FORESTPLOTout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(FORESTPLOTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

