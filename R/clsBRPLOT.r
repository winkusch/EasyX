setClass("BRPLOT",
	representation = representation(
						strEqcCommand		=	"character",
						acolBRPlot			=	"character",
						astrColour			=	"character",
						blnLegend			=	"logical",
						astrLegendText		=	"character",
						strAxes				=	"character",
						strXlab				=	"character",
						strYlab				=	"character",
						strTitle			=	"character",
						arcdAdd2Plot		=	"character",
						strFormat			=	"character",
						strMode				=	"character",
						numCexAxis			=	"numeric",
						numCexLab			=	"numeric",
						numWidth			=	"numeric",
						numHeight			=	"numeric",
						anumParMar			=	"numeric",
						anumParMgp			=	"numeric",
						strParBty			=	"character",
						strPlotName			= 	"character",
						blnNamesIdx			= 	"logical",
						numCexNames			=	"numeric",
						numParLas			=	"numeric"
						),
	prototype = prototype(
						strEqcCommand		=	"",
						acolBRPlot			=	"",
						astrColour			=	"grey",
						blnLegend			=	FALSE,
						astrLegendText		=	"",
						strAxes				=	"",
						strXlab				=	"Studies",
						strYlab				=	"N",
						strTitle			=	"",
						arcdAdd2Plot		=	"",
						strFormat			=	"png",
						strMode				=	"singleplot",
						numCexAxis			=	1,
						numCexLab			=	1,
						numWidth			=	-1,	# pixel px # needs change for pdf!
						numHeight			=	640,	# pixel px # needs change for pdf!
						anumParMar			=	c(5.1, 4.1, 4.1, 2.1),
						anumParMgp			=	c(3, 1, 0),
						strParBty			=	"o",
						strPlotName			= 	"brp",
						blnNamesIdx			= 	FALSE,
						numCexNames			= 	1,
						numParLas			=	1
						)
	#contains = c("EcfReader")
)

setGeneric("setBRPLOT", function(object) standardGeneric("setBRPLOT"))
setMethod("setBRPLOT", signature = (object = "BRPLOT"), function(object) {
	
	aEqcSlotNamesIn = c("acolBRPlot", 
						"astrColour",
						"blnLegend",
						"astrLegendText",
						"strAxes",
						"strXlab",
						"strYlab",
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
						"blnNamesIdx",
						"numCexNames",
						"numParLas"
						)
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	
	isDefaultPngSize = object@numWidth == 640 & object@numHeight == 640
	if(object@strFormat == "pdf" & isDefaultPngSize) {
		object@numWidth <- 6
		object@numHeight <- 6
	}
	
	return(object)
})

#############################################################################################################################
validBRPLOT <- function(objBRP) {

	if(!(objBRP@strMode == "singleplot" | objBRP@strMode == "subplot"))
		stop(paste("EASY ERROR:BRPLOT\n Wrong strMode defined, PLease use 'singleplot' or ''subplot' \n !!!", sep=""))
	
	if(objBRP@astrColour[1] != "") {
		for(i in 1:length(objBRP@astrColour)) {
			
			strColour = objBRP@astrColour[i]
			isOkColors = strColour %in% colors()
			
			isOkColorHex =  nchar(strColour)==7 & substring(strColour,1,1)=="#"
			sTemp = substring(strColour,2,7) 
			isOkColorHex = isOkColorHex & all(strsplit(sTemp,"",fixed=TRUE)[[1]]%in%c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"))
			
			if(!isOkColors & !isOkColorHex)
				stop(paste("EASY ERROR:BRPLOT\n Wrong astrColour defined. PLease use color from R colors() or hexadecimal nomenclature, eg #FFFFFF.", sep=""))
		}
	}
		
	if(!(objBRP@strFormat %in% c("png","pdf","tiff","jpeg")))
		stop(paste("EASY ERROR:SPLOT\n Wrong strFormat defined, PLease use 'png','pdf','tiff' or 'jpeg' \n !!!", sep=""))
	
	if(objBRP@numCexAxis <= 0) 
		stop(paste("EASY ERROR:BRPLOT\n Wrong numCexAxis defined, PLease use numCexAxis > 0 \n !!!", sep=""))
		
	if(objBRP@numCexLab <= 0) 
		stop(paste("EASY ERROR:BRPLOT\n Wrong numCexLab defined, PLease use numCexLab > 0 \n !!!", sep=""))				
	
	if(length(objBRP@anumParMar) != 4) 
		stop(paste("EASY ERROR:BRPLOT\n Wrong anumParMar defined, Please use anumParMar of length 4 (Default=5.1;4.1;4.1;2.1) \n !!!", sep=""))				
	
	if(length(objBRP@anumParMgp) != 3) 
		stop(paste("EASY ERROR:BRPLOT\n Wrong anumParMgp defined, Please use anumParMgp of length 3 (Default=3;1;0) \n !!!", sep=""))				
	
	if(!(objBRP@strParBty%in%c("o","l","7","c","u","]")))
		stop(paste("EASY ERROR:BRPLOT\n Wrong strParBty defined, Please use 'o','l','7','c','u' or ']' \n !!!", sep=""))				
	
	return(TRUE)
}

BRPLOT.REPORT.valid <- function(objBRP, objREPORT) {
	
	aisMatch = objBRP@acolBRPlot %in% names(objREPORT@tblReport)
	
	fileReportName = strsplit(objREPORT@fileReport,"/",fixed=T)[[1]][length(strsplit(objREPORT@fileReport,"/",fixed=T)[[1]])]
	
	if(!all(aisMatch))
		stop(paste("EASY ERROR:BRPLOT\n Column \n",paste(objBRP@acolBRPlot[which(!aisMatch)],collapse="\n")," does not exist in report table\n",fileReportName,"\n !!!" ,sep=""))
	
}

BRPLOT.REPORT.resize <- function(objBRP, objREPORT) {
	
	tblReport = objREPORT@tblReport
	
	## remove last NA row:
	if(all(tblReport[nrow(tblReport),]=="NA")|all(is.na(tblReport[nrow(tblReport),]))) tblReport <- tblReport[-nrow(tblReport),]
	
	if(objBRP@numWidth == -1) {
		if(objBRP@numParLas==1) objBRP@numWidth = nrow(tblReport)*120
		else if(objBRP@numParLas==2) {
			objBRP@numWidth = nrow(tblReport)*60
			if(objBRP@anumParMar[1]==5.1) {
				objBRP@anumParMar[1]=7.1
			}
		}
	}
	return(objBRP)
}

BRPLOT.run <- function(objBRP, objREPORT) {
	
	tblPlot					=	objREPORT@tblReport
	
	acolBRPlot 				= 	objBRP@acolBRPlot
	astrColour				=	objBRP@astrColour
	blnLegend				=	objBRP@blnLegend
	astrLegendText			=	objBRP@astrLegendText
	strAxes					=	objBRP@strAxes
	strXlab					=	objBRP@strXlab
	strYlab					=	objBRP@strYlab
	strTitle				=	objBRP@strTitle
	arcdAdd2Plot			=	objBRP@arcdAdd2Plot
	numCexAxis				=	objBRP@numCexAxis
	numCexLab				=	objBRP@numCexLab
	blnNamesIdx				=	objBRP@blnNamesIdx
	numCexNames				=	objBRP@numCexNames
	numParLas 				=	objBRP@numParLas
	strPlotName				=	objBRP@strPlotName
	
	#if(objBRP@strMode == "singleplot") strTitle = objREPORT@fileInShortName
	if(strTitle == "") {
		plot.title = strsplit(objREPORT@fileReportBody,"/",fixed=TRUE)[[1]][length(strsplit(objREPORT@fileReportBody,"/",fixed=TRUE)[[1]])]
		plot.title = paste(plot.title,strPlotName,sep=".")
	} else {
		plot.title = strTitle
	}
	
	### Set Plot colours, symbols and symbol sizes
	
	if(length(astrColour) == 1 & astrColour[1] == "grey") astrColour = rep(astrColour , length(acolBRPlot))
	
	if(length(acolBRPlot) != length(astrColour)) {
		astrColour = rep(astrColour[1] , length(acolBRPlot)) 
		warning(paste("EASY WARNING:BRPLOT\n Length of astrColour does not match length of acolBRPlot. \n First Colour ",astrColour[1]," will be used for all bars." ,sep="" ))
	}
	
	if(astrLegendText[1] == "") astrLegendText = acolBRPlot
	astrLegendColour = astrColour
	
	#### Remove trailing NA row from tblReport:
	if(all(tblPlot[nrow(tblPlot),]=="NA")|all(is.na(tblPlot[nrow(tblPlot),]))) tblPlot <- tblPlot[-nrow(tblPlot),]
	
	aicolBR = match(acolBRPlot, names(tblPlot))
	for(icolBR in aicolBR) {
		tblPlot[,icolBR] <- as.numeric(tblPlot[,icolBR])
	}
	
	###     NRV NCV
	## s1   100  1000
 	## s2   50   900
	## s2   200   700

	### first column is the short filename !!!
	tblPlotShort = tblPlot[,aicolBR]
	tblPlotShortTrans = t(tblPlotShort)

		
	# ## From OpenPlot:
	# par(xpd=F, 
		# bty=objBRP@strParBty, 
		# mar=objBRP@anumParMar, 
		# mgp=objBRP@anumParMgp
		# )
	
	##### want: three stacked bars, stacked by NRV,NCV
	if(blnNamesIdx) {
		astrStudyNames = 1:ncol(tblPlotShortTrans)
	} else {
		astrStudyNames = tblPlot[,1]
		
		for(k in 1:length(astrStudyNames)) {
			sname = astrStudyNames[k]
			
			if(nchar(sname)>10&nchar(sname)<=20) {
				snameout = paste(substr(sname, 1, 10), substr(sname, 11, nchar(sname)), sep="\n")
			} else if(nchar(sname)>20&nchar(sname)<=30) {
				snameout = paste(substr(sname, 1, 10), substr(sname, 11, 20), sep="\n")
				snameout = paste(snameout, substr(sname, 21, nchar(sname)), sep="\n")
			} else if(nchar(sname)>30&nchar(sname)<=40) {
				snameout = paste(substr(sname, 1, 10), substr(sname, 11, 20), sep="\n")
				snameout = paste(snameout, substr(sname, 21, 30), sep="\n")
				snameout = paste(snameout, substr(sname, 31, nchar(sname)), sep="\n")
			} else if(nchar(sname)>40) {
				## four breakpoints & reset cex
				numCexNames = 40/nchar(sname)
				nStep = floor(nchar(sname))/4
				snameout = paste(substr(sname, 1, nStep), substr(sname, nStep+1, nStep*2), sep="\n")
				snameout = paste(snameout, substr(sname, nStep*2+1, nStep*3), sep="\n")
				snameout = paste(snameout, substr(sname, nStep*3+1, nchar(sname)), sep="\n")
			} 
			astrStudyNames[k] = snameout
			
			# numBreaks = floor(nchar(sname)/10)
			
			# if(numBreaks>=1 & numBreaks<=3) {
				# snameout = substr(sname, 1, 10)
				# for(iBreak in 1:numBreaks) {
					# numStop = ifelse(((iBreak+1)*10)<nchar(sname), ((iBreak+1)*10), nchar(sname))
					# snameout = paste(snameout, substr(sname, iBreak*10+1, numStop),sep="\n")
				# }
				# astrStudyNames[k] = snameout
			# }
			# if(numBreaks>=4) {
				# numCharInRow = floor(nchar(sname)/10
				
			# }
			
		}
	}
	
	barplot(tblPlotShortTrans,
		main = plot.title,
		xlab = strXlab,
		ylab = strYlab,
		col = astrColour,
		legend = astrLegendText,
		names.arg = astrStudyNames,
		cex.names = numCexNames,
		las = numParLas
	)
	# axis(1,at=c(1:ncol(tblPlotShortTrans)),labels = astrStudyNames,cex.axis = 0.5)

	
	if(arcdAdd2Plot[1] != "") {
		for(rcdTmp in arcdAdd2Plot) {
			for(colTmp in names(tblPlot)) {
				if(length(grep(colTmp,rcdTmp)) > 0) {
					icolTmp=which(names(tblPlot)==colTmp)
					assign(colTmp,as.numeric(as.character(tblPlot[,icolTmp])))
					# create arrays, that are used within criterion and name them accordingly
					# tblIn$pmen -> create array pmen if criterion e.g. pmen>0.3
				}
			}
			
			expTmp=parse(text=rcdTmp)	
			
			eval(expTmp)
		}
	}
	
}


BRPLOT <- function(strEqcCommand){ 
	## Wrapper for class definition
	
	BRPLOTout <- setBRPLOT(new("BRPLOT", strEqcCommand = strEqcCommand))
	validBRPLOT(BRPLOTout)
	#ADDCOLout.valid <- validADDCOL(ADDCOLout)
	return(BRPLOTout)
	#validECF(ECFout)
	#return(ECFout)
	
	## Identical:
	# ECFin <- new("ECF5", fileECF = fileECFIn) 
	# ECFout <- setECF5(ECFin)
	# return(ECFout)
}


