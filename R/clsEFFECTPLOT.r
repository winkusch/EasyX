setClass("EFFECTPLOT",
	representation = representation(
						strEqcCommand		=	"character",
						### general params:
						rcdCriterion		=	"character",
						acolBeta			=	"character",
						colGroupBy			=	"character",
						## 
						blnPlotNonSgnAsZero	= 	"logical",
						numPvalOffset		=	"numeric", 		
						blnPlotBubble		= 	"logical",
						anumPvalOffset		=	"numeric", 		
						anumPvalOffsetCex	=	"numeric",
						acolPval			=	"character", 	## only needed if numPvalOffset or anumPvalOffsetCex is given!
						### graphical params:
						colXlabels			=	"character",
						astrColour			=	"character",	## per acolBeta
						anumSymbol			=	"numeric",		## per acolBeta
						anumCex				=	"numeric",		## per acolBeta
						blnLegend			=	"logical",
						astrLegendText		=	"character",
						strYaxis			=	"character", 
						strYlab				=	"character",
						strXlab				=	"character",
						strTitle			=	"character",
						arcdAdd2Plot		=	"character",
						strFormat			=	"character",
						numCexAxis			=	"numeric",
						numCexLab			=	"numeric",
						numWidth			=	"numeric",
						numHeight			=	"numeric",
						anumParMar			=	"numeric",
						anumParMgp			=	"numeric",
						strParBty			=	"character",
						strPlotName			= 	"character",
						numParLas			=	"numeric",
						strTag				=	"character",
						blnPlotXAxisLabels  = 	"logical"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						## general params
						rcdCriterion		=	"",
						acolBeta			=	"",
						colGroupBy			=	"",
						##
						blnPlotNonSgnAsZero = 	FALSE,
						numPvalOffset		=	0.05,
						blnPlotBubble		= 	FALSE,
						anumPvalOffset		=	c(1,0.05,5e-8),
						anumPvalOffsetCex	=	c(0.5,1,2),
						acolPval			=	"", 
						## graphical params
						colXlabels			=	"",
						astrColour			=	"black",
						anumSymbol			=	1,
						anumCex				=	2.5,
						blnLegend			=	TRUE,
						astrLegendText		=	"",
						strYaxis			=	"equal", 
						strYlab				=	"Beta",
						strXlab				=	"",
						strTitle			=	"",
						arcdAdd2Plot		=	"",
						strFormat			=	"png",
						numCexAxis			=	1,
						numCexLab			=	1,
						numWidth			=	800,
						numHeight			=	600,
						anumParMar			=	-1,
						anumParMgp			=	c(3, 1, 0),
						strParBty			=	"o",
						strPlotName			= 	"ep",
						numParLas			=	3,
						strTag				=	"",
						blnPlotXAxisLabels  =	FALSE
						)
	#contains = c("EcfReader")
)

setGeneric("setEFFECTPLOT", function(object) standardGeneric("setEFFECTPLOT"))
setMethod("setEFFECTPLOT", signature = (object = "EFFECTPLOT"), function(object) {
	
	aEqcSlotNamesIn = c("rcdCriterion",
						"acolBeta",
						"colGroupBy",
						"blnPlotNonSgnAsZero",
						"numPvalOffset",
						"blnPlotBubble",
						"anumPvalOffset",
						"anumPvalOffsetCex",
						"acolPval",
						"colXlabels",
						"astrColour",
						"anumSymbol",
						"anumCex",
						"blnLegend",
						"astrLegendText",
						"strYaxis",
						"strYlab",
						"strXlab",
						"strTitle",
						"arcdAdd2Plot",
						"strFormat",
						"numCexAxis",
						"numCexLab",
						"numWidth",
						"numHeight",
						"anumParMar",
						"anumParMgp",
						"strParBty",
						"strPlotName",
						"numParLas",
						"strTag"
						)
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
	isDefaultPngSize = object@numWidth == 800 & object@numHeight == 600
	if(object@strFormat == "pdf" & isDefaultPngSize) {
		object@numWidth <- 8
		object@numHeight <- 6
	}
	
	if(object@strTag != "") object@strPlotName <- paste(object@strPlotName,object@strTag,sep=".")
	
	if(object@colXlabels != "") object@blnPlotXAxisLabels <- TRUE
	if(object@anumParMar[1]==-1) {
		object@anumParMar <- c(5.1, 4.1, 4.1, 2.1)
		if(object@blnPlotXAxisLabels) {
			object@anumParMar[1] <- 8
		} 
		if(object@blnLegend) {
			object@anumParMar[4] <- 8
		}
	}
	
	return(object)
})

#############################################################################################################################
validEFFECTPLOT <- function(objEP) {
	## Paste validity checks specific for EFFECTPLOT
	
	if(objEP@numPvalOffset<=0 | objEP@numPvalOffset>1)
		stop(paste("EASY ERROR:EFFECTPLOT\n numPvalOffset must be within ]0;1] \n !!!" ,sep=""))
		
	if(!(objEP@strFormat %in% c("png","pdf","tiff","jpeg")))
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong strFormat defined, PLease use 'png','pdf','tiff' or 'jpeg' \n !!!", sep=""))
			
	if(objEP@numCexAxis <= 0) 
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong numCexAxis defined, PLease use numCexAxis > 0 \n !!!", sep=""))
		
	if(objEP@numCexLab <= 0) 
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong numCexLab defined, PLease use numCexLab > 0 \n !!!", sep=""))				
	
	if(length(objEP@anumParMar) != 4) 
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong anumParMar defined, Please use anumParMar of length 4 (Default=5.1;4.1;4.1;2.1) \n !!!", sep=""))				
	
	if(length(objEP@anumParMgp) != 3) 
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong anumParMgp defined, Please use anumParMgp of length 3 (Default=3;1;0) \n !!!", sep=""))				
		
	if(!(objEP@strParBty%in%c("o","l","7","c","u","]")))
		stop(paste("EASY ERROR:EFFECTPLOT\n Wrong strParBty defined, Please use 'o','l','7','c','u' or ']' \n !!!", sep=""))				
	
	if(objEP@numPvalOffset!=1 & objEP@acolPval[1]=="") {
		stop(paste("EASY ERROR:EFFECTPLOT\n Defining numPvalOffset requires to set acolPval (which is missing). Please set --acolPval \n !!!", sep=""))	
	}
	
	if(objEP@blnPlotBubble & objEP@acolPval[1]=="") {
		stop(paste("EASY ERROR:EFFECTPLOT\n Defining blnPlotBubble requires to set acolPval (which is missing). Please set --acolPval \n !!!", sep=""))	
	}
		
	return(TRUE)
}

EFFECTPLOT.GWADATA.valid <- function(objEP, objGWA) {
	
	aisMatch = objEP@acolBeta %in% objGWA@aHeader
	if(!all(aisMatch))
		stop(paste("EASY ERROR:EFFECTPLOT\n Column \n",paste(objEP@acolBeta[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	if(objEP@acolPval[1]!="") {
		aisMatch = objEP@acolPval %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:EFFECTPLOT\n Column \n",paste(objEP@acolPval[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	}
	
	if(objEP@colGroupBy!="") {
		aisMatch = objEP@colGroupBy %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:EFFECTPLOT\n Column \n",paste(objEP@colGroupBy[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	}
	
	if(objEP@colXlabels!="") {
		aisMatch = objEP@colXlabels %in% objGWA@aHeader
		if(!all(aisMatch))
			stop(paste("EASY ERROR:EFFECTPLOT\n Column \n",paste(objEP@colXlabels[which(!aisMatch)],collapse="\n")," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	}
	
	return(TRUE)
	
}


EFFECTPLOT.run <- function(objEP, objGWA, objREPORT) {
	#function(tblPlot,acolQQPlot,astrColour,blnCombined,rcdNumCIBounds,fileOut)
	
	rcdCriterion 	<- objEP@rcdCriterion
	acolBeta 		<- objEP@acolBeta
	colGroupBy 		<- objEP@colGroupBy
	numPvalOffset 	<- objEP@numPvalOffset
	acolPval 		<- objEP@acolPval
	colXlabels		<- objEP@colXlabels
	astrColour 		<- objEP@astrColour
	anumSymbol 		<- objEP@anumSymbol
	anumCex 		<- objEP@anumCex
	blnLegend 		<- objEP@blnLegend
	astrLegendText 	<- objEP@astrLegendText
	strYaxis 		<- objEP@strYaxis
	strYlab 		<- objEP@strYlab
	strXlab 		<- objEP@strXlab
	strTitle 		<- objEP@strTitle
	arcdAdd2Plot 	<- objEP@arcdAdd2Plot
	numCexAxis 		<- objEP@numCexAxis
	numCexLab 		<- objEP@numCexLab
	numParLas		<- objEP@numParLas
	blnPlotNonSgnAsZero <- objEP@blnPlotNonSgnAsZero
	strTag			<- objEP@strTag
	blnPlotXAxisLabels <- objEP@blnPlotXAxisLabels
	
	blnPlotBubble	<- objEP@blnPlotBubble
	anumPvalOffset	<- objEP@anumPvalOffset
	anumPvalOffsetCex <- objEP@anumPvalOffsetCex
	
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
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPEffectPlotCrit",sep=""),numCrit)
	
	# if(numCrit == 0) {
	
		# objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepCrit",sep=""),0)
		# objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numSNPIndepNA",sep=""),0)
		# objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numIndepLoci",sep=""),0)
		
		# return(list(objGWA,objGWA.indep,objGWA.indep,objREPORT))
	# }
	
	###### Copy plotting data.frame
	
	tblPlot 		<- as.data.frame(objGWA.crit@tblGWA,stringsAsFactors=FALSE)
	
	######
	
	if(colGroupBy != "") {
		tblPlot <- tblPlot[order(tblPlot[,colGroupBy]),]
	}
		
	###########################
	numTraits <- length(acolBeta)
	
	#if(objEP@strMode == "singleplot") plot.title = objGWA@fileInShortName
	#else plot.title = ""
	
	if(length(astrColour) == 1 & length(acolBeta)>1)
		astrColour 	= rep(astrColour , length(acolBeta))
		
	if(length(anumSymbol) == 1 & length(acolBeta)>1)
		anumSymbol 	= rep(anumSymbol , length(acolBeta))
		
	if(length(anumCex) == 1 & length(acolBeta)>1)
		anumCex = rep(anumCex , length(acolBeta))
	
	### Reset some graphical params
	
	if(strTitle == "") plot.title = objGWA@fileInShortName
	else plot.title = strTitle
	
	if(strXlab == "") strXlab <- "SNPs"
	if(strYlab=="") {
		if(length(acolBeta) == 1) strYlab = acolBeta[1]
		else strYlab = "Betas"
	}

	#### Init Beta and P-Values and get min/max Y for ploting
	ymin <- ymax <- 0
	for(i in 1:numTraits) {
		tblPlot[,acolBeta[i]] <- as.numeric(tblPlot[,acolBeta[i]])
		
		if(blnPlotNonSgnAsZero) {
			tblPlot[,acolBeta[i]] <- ifelse(!is.na(tblPlot[,acolBeta[i]])&(tblPlot[,acolPval[i]] >= numPvalOffset), 0 ,tblPlot[,acolBeta[i]])
		}
		
		# if(numPvalOffset < 1) {
			# tblPlot[,acolPval[i]] <- as.numeric(tblPlot[,acolPval[i]])
			# if(blnPlotNonSgnAsZero) {
				# tblPlot[,acolBeta[i]] <- ifelse(!is.na(tblPlot[,acolBeta[i]])&(tblPlot[,acolPval[i]] >= numPvalOffset), 0 ,tblPlot[,acolBeta[i]])
			# } else {
				# tblPlot[,acolBeta[i]] <- ifelse(tblPlot[,acolPval[i]] >= numPvalOffset, NA ,tblPlot[,acolBeta[i]])
			# }
		# }
		ymin <- min(ymin, min(tblPlot[,acolBeta[i]],na.rm=T))
		ymax <- max(ymax, max(tblPlot[,acolBeta[i]],na.rm=T))
	}
	
	if(length(grep("lim",strYaxis)) > 0)  {
		str_lim = substr(strYaxis,5,nchar(strYaxis)-1)
		## str_lim = "y0,y1"
		arr_str_lim = strsplit(str_lim,",")[[1]]
		ymin = ifelse(arr_str_lim[1]!="NULL",as.numeric(arr_str_lim[1]),ymin)
		ymax = ifelse(arr_str_lim[2]!="NULL",as.numeric(arr_str_lim[2]),ymax)
	} else if(strYaxis == "equal")  {
		ymin <- (-1)*max(abs(ymin),abs(ymax))
		ymax <- max(abs(ymin),abs(ymax))
	} 
		
	##########################################################################################################################################

	cexMain = 1.2
	titleWidth = nchar(strTitle)*par()$cin[1]
	if(titleWidth*1.2 > par()$fin[1]) {
		## Zeilenumbruch in Haelfte einfuegen
		iBreakUp = floor(nchar(strTitle)/2)+1
		strTitle = paste(substr(strTitle, 1, iBreakUp), substr(strTitle, iBreakUp+1, nchar(strTitle)), sep="\n")
		if((iBreakUp+1)*1.2*par()$cin[1] > par()$pin[1]) {
		titleWidth = (iBreakUp+1)*par()$cin[1]
		cexMain = 1.2*par()$fin[1]/titleWidth
		}
	}

	numSnps = dim(tblPlot)[1]
	aX = seq(1:numSnps)
	if(!blnPlotXAxisLabels) aXAxisTickLab <- NULL
	else aXAxisTickLab <- tblPlot[,colXlabels]
	
	# par()$mgp[2] + max(strwidth(aXAxisTickLab))
	# # ## reset mar to fit x axis vals into area

	plot(NULL,NULL, 
			xlab = "", ylab = strYlab, 
			xlim = c(0,numSnps), ylim = c(ymin,ymax), 
			main = strTitle, cex.main=cexMain,
			yaxt="n",xaxt="n")
			
	abline(a=0,b=0,col="black",lty=1,xpd=FALSE)				
	
	# strXlabWidth = max(strwidth(aXAxisTickLab))
	# mgpXLab = anumParMgp[2]
	# mgpXTitle = mgpXLab + strXlabWidth + 1
	# anumParMar[1] <- mgpXTitle + 2
	# par(mar=anumParMar)	
	
	for(i in 1:length(acolBeta)) {
		aBeta = as.numeric(tblPlot[,acolBeta[i]])
		if(blnPlotBubble) {
			aPval = as.numeric(tblPlot[,acolPval[i]])
			anumCexPlot = rep(NA,length(aBeta))
			for(k in 1:length(anumPvalOffset)) {
				numPvalOffsetTmp = anumPvalOffset[k]
				anumCexPlot[aPval<=numPvalOffsetTmp] <- anumPvalOffsetCex[k]
			}
			points(aX,aBeta,pch=anumSymbol[i],col=astrColour[i],cex = anumCexPlot)
		} else {
			points(aX,aBeta,pch=anumSymbol[i],col=astrColour[i],cex = anumCex[i])
		}
	}
	if(blnPlotXAxisLabels) axis(1,at=aX,labels = aXAxisTickLab,cex.axis = numCexAxis,cex.lab=numCexLab,las=numParLas)
	axis(2,cex.axis = numCexAxis, cex.lab=numCexLab, las=numParLas)
	
	xAxisLabelWidthOffset = ifelse(blnPlotXAxisLabels,max(strwidth(aXAxisTickLab))+1,0)
	
	if(colGroupBy != "") {
		aGroups = unique(tblPlot[,colGroupBy])
				
		for(i in 1:length(aGroups)) {
			group = aGroups[i]
			aidxGroup = which(tblPlot[,colGroupBy]==group)
			xlabPos = mean(aidxGroup)
			mtext(group,at = xlabPos,side=1, line = par()$mgp[2]+xAxisLabelWidthOffset, cex=numCexLab,padj=2)
			if(i<length(aGroups)) abline(v = max(aidxGroup)+0.5,col="orange")
		}
		# mtext(strXlab, side=1, line = par()$mgp[2] + max(strwidth(aXAxisTickLab)) + 1, cex=numCexLab,padj=2)
	} else {
		mtext(strXlab, side=1, line = par()$mgp[2] + xAxisLabelWidthOffset, cex=numCexLab,padj=2)
	}
	
	if(blnLegend) {
		if(astrLegendText[1] == "") astrLegendText = acolBeta
		astrLegendColour = astrColour
		anumLegendPch = anumSymbol

		ausr = par()$usr
		xleg = ausr[2]
		yleg = ausr[4]
		
		legend(x=xleg, y=yleg,
				inset=0.005, legend=astrLegendText,
				col =astrLegendColour, pch = anumLegendPch, 
				bg="skyblue", xpd=TRUE)
	}
		
	return(objREPORT)
	
}


EFFECTPLOT <- function(strEqcCommand){ 
	## Wrapper for class definition
	EFFECTPLOTout <- setEFFECTPLOT(new("EFFECTPLOT", strEqcCommand = strEqcCommand))
	validEFFECTPLOT(EFFECTPLOTout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(EFFECTPLOTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}

