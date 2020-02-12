setClass("FANCYPLOT",
	representation = representation(
						strEqcCommand			=	"character",
						acolFPlot				=	"character",
						astrFColours			=	"character",
						numFColourPvalLim		=	"numeric",
						rcdFColourCrit			=	"character",
						strYlab					=	"character",
						colInChr				=	"character",
						colInPos				=	"character",
						astrDefaultColourChr	=	"character",
						arcdColourCrit			=	"character",
						astrColour				=	"character",
						numPvalOffset			=	"numeric",
						arcdAdd2Plot			=	"character",
						anumAddPvalLine			=	"numeric",
						astrAddPvalLineCol		=	"character",
						anumAddPvalLineLty		=	"numeric",
						blnYAxisBreak			=	"logical",
						numYAxisBreak			=	"numeric",
						strPlotName				= 	"character",
						strFormat				=	"character",
						numCexAxis				=	"numeric",
						numCexLab				=	"numeric",
						numWidth				=	"numeric",
						numHeight				=	"numeric",
						anumParMar				=	"numeric",
						anumParMgp				=	"numeric",
						strParBty				=	"character",
						blnLogPval				=	"logical",						
						numDefaultSymbol		=	"numeric",
						numDefaultCex			=	"numeric",
						arcdSymbolCrit			=	"character",
						anumSymbol				=	"numeric",
						arcdCexCrit				=	"character",
						anumCex					=	"numeric",
						fileAnnot				=	"character",
						numAnnotPosLim			=	"numeric",
						numAnnotPvalLim			=	"numeric"
						),
	prototype = prototype(
						strEqcCommand			=	"",
						acolFPlot				=	"",
						astrFColours			=	"",
						numFColourPvalLim		=	5e-8,
						rcdFColourCrit			=	"",
						strYlab					=	"-log10(P)",
						colInChr				=	"",
						colInPos				=	"",
						astrDefaultColourChr	= 	c("gray51","gray66"),
						arcdColourCrit			=	"",
						astrColour				=	"",
						numPvalOffset			=	1,
						arcdAdd2Plot			=	"",
						anumAddPvalLine			=	5e-8,
						astrAddPvalLineCol		=	"red",
						anumAddPvalLineLty		=	6,
						blnYAxisBreak			=	FALSE,
						numYAxisBreak			=	22,
						strPlotName				= 	"fp",
						strFormat				=	"png",
						numCexAxis				=	1.5,
						numCexLab				=	2,
						numWidth				=	1600,	# pixel px # needs change for pdf!
						numHeight				=	600,	# pixel px # needs change for pdf!
						anumParMar				=	c(7, 6, 4, 10),
						anumParMgp				=	c(3, 1, 0),
						strParBty				=	"o",
						blnLogPval				=	FALSE,
						numDefaultSymbol		=	19,
						numDefaultCex			=	0.4,
						arcdSymbolCrit			=	"",
						anumSymbol				=	-1,
						arcdCexCrit				=	"",
						anumCex					=	-1,
						fileAnnot				=	"",
						numAnnotPosLim			=	500000,
						numAnnotPvalLim			=	1
						)
	#contains = c("EcfReader")
)
setGeneric("setFANCYPLOT", function(object) standardGeneric("setFANCYPLOT"))
setMethod("setFANCYPLOT", signature = (object = "FANCYPLOT"), function(object) {
	
	aEqcSlotNamesIn = c("acolFPlot","astrFColours","numFColourPvalLim","rcdFColourCrit","colInChr","colInPos","strYlab",
						"astrDefaultColourChr",
						"arcdColourCrit","astrColour",
						"numPvalOffset",
						"arcdAdd2Plot","anumAddPvalLine","astrAddPvalLineCol","anumAddPvalLineLty",
						"blnYAxisBreak","numYAxisBreak","strPlotName",
						"strFormat","numCexAxis","numCexLab","numWidth","numHeight","anumParMar","anumParMgp","strParBty",
						"blnLogPval",
						"numDefaultSymbol","numDefaultCex","arcdSymbolCrit","anumSymbol","arcdCexCrit","anumCex",
						"fileAnnot","numAnnotPosLim","numAnnotPvalLim")
	
	objEqcReader <- EqcReader(object@strEqcCommand,aEqcSlotNamesIn)
	
	if(length(objEqcReader@lsEqcSlotsOut) > 0) {
		for(i in 1:length(objEqcReader@lsEqcSlotsOut)) {
			tmpSlot <- names(objEqcReader@lsEqcSlotsOut)[i]
			tmpSlotVal <- objEqcReader@lsEqcSlotsOut[[i]]
			
			if(all(!is.na(tmpSlotVal))) slot(object, tmpSlot) <- tmpSlotVal
		}
	}
	isDefaultPngSize = object@numWidth == 1600 & object@numHeight == 600
	if(object@strFormat == "pdf" & isDefaultPngSize) {
		object@numWidth <- 12
		object@numHeight <- 5
	}
	
	return(object)
})
#############################################################################################################################
FANCYPLOT.GWADATA.valid <- function(objFANCYPLOT, objGWA) {
	
	if(any(objFANCYPLOT@acolFPlot == ""))
		stop(paste(" EASY ERROR:FANCYPLOT\n No columns acolFPlot defined.\n Please set acolFPlot.", sep=""))
	if(any(objFANCYPLOT@astrFColours == ""))
		stop(paste(" EASY ERROR:FANCYPLOT\n No colours astrFColours defined.\n Please set astrFColours.", sep=""))
				
	aisMatch = objFANCYPLOT@acolFPlot %in% objGWA@aHeader
	if(any(!aisMatch))
		stop(paste("EASY ERROR:FANCYPLOT\n Column \n",objFANCYPLOT@acolFPlot[which(!aisMatch)[1]]," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	# isMatch = objFANCYPLOT@colFPlot %in% objGWA@aHeader
	# if(!isMatch)
		# stop(paste("EASY ERROR:FANCYPLOT\n Column \n",objFANCYPLOT@colFPlot," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	isMatch = objFANCYPLOT@colInChr %in% objGWA@aHeader
	if(!isMatch)
		stop(paste("EASY ERROR:FANCYPLOT\n Column \n",objFANCYPLOT@colInChr," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	isMatch = objFANCYPLOT@colInPos %in% objGWA@aHeader
	if(!isMatch)
		stop(paste("EASY ERROR:FANCYPLOT\n Column \n",objFANCYPLOT@colInPos," does not exist in file\n",objGWA@fileInShortName,"\n !!!" ,sep=""))
	
	if(objFANCYPLOT@numPvalOffset<=0 | objFANCYPLOT@numPvalOffset>1)
		stop(paste("EASY ERROR:FANCYPLOT\n numPvalOffset must be within ]0;1] \n !!!" ,sep=""))
	
	if(any(objFANCYPLOT@anumAddPvalLine<=0 | objFANCYPLOT@anumAddPvalLine>=1)) 
		stop(paste("EASY ERROR:FANCYPLOT\n Each value in anumAddPvalLine must be within ]0;1[ \n !!!" ,sep=""))
	
	if(length(objFANCYPLOT@anumAddPvalLine) != length(objFANCYPLOT@astrAddPvalLineCol)) 
		objFANCYPLOT@astrAddPvalLineCol = rep("red",length(objFANCYPLOT@anumAddPvalLine))
		
	if(length(objFANCYPLOT@anumAddPvalLine) != length(objFANCYPLOT@anumAddPvalLineLty)) 
		objFANCYPLOT@anumAddPvalLineLty = rep(6,length(objFANCYPLOT@anumAddPvalLine))

	#####
	
	if(!(objFANCYPLOT@numDefaultSymbol%in%c(0:25)))
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong numDefaultSymbol defined. PLease use integer value between 0 and 25.", sep=""))

	if(objFANCYPLOT@numDefaultCex <= 0)
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong numDefaultCex defined. PLease use positive numeric value.", sep=""))
	
	astrDefaultColourChr = objFANCYPLOT@astrDefaultColourChr
	
	if(length(astrDefaultColourChr)!=2) 
		stop(paste("EASY ERROR:FANCYPLOT\n Length of astrDefaultColourChr must be 2. PLease use two colours separated by ';'.", sep=""))
		
	for(i in 1:2) {
		strColour = astrDefaultColourChr[i]
		isOkColors =  strColour %in% colors()
		
		isOkColorHex =  nchar(strColour)==7 & substring(strColour,1,1)=="#"
		sTemp = substring(strColour,2,7) 
		isOkColorHex = isOkColorHex & all(strsplit(sTemp,"",fixed=TRUE)[[1]]%in%c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"))
		
		if(!isOkColors & !isOkColorHex)
			stop(paste("EASY ERROR:FANCYPLOT\n Wrong astrDefaultColourChr defined. PLease use color from R colors() or hexadecimal nomenclature, eg #FFFFFF.", sep=""))
	}
	
	if(objFANCYPLOT@astrColour[1] != "") {
		for(i in 1:length(objFANCYPLOT@astrColour)) {
			
			strColour = objFANCYPLOT@astrColour[i]
			isOkColors = strColour %in% colors()
			
			isOkColorHex =  nchar(strColour)==7 & substring(strColour,1,1)=="#"
			sTemp = substring(strColour,2,7) 
			isOkColorHex = isOkColorHex & all(strsplit(sTemp,"",fixed=TRUE)[[1]]%in%c("0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"))
			
			if(!isOkColors & !isOkColorHex)
				stop(paste("EASY ERROR:FANCYPLOT\n Wrong astrColour defined. PLease use color from R colors() or hexadecimal nomenclature, eg #FFFFFF.", sep=""))
		}
	}
	if(objFANCYPLOT@anumSymbol[1] != -1) {
		for(i in 1:length(objFANCYPLOT@anumSymbol)) {
			
			numSymbol = objFANCYPLOT@anumSymbol[i]
			if(!(numSymbol%in%c(0:25)))
				stop(paste("EASY ERROR:FANCYPLOT\n Wrong anumSymbol defined. PLease use integer value between 0 and 25.", sep=""))
		}
	}
	if(objFANCYPLOT@anumCex[1] != -1) {
		for(i in 1:length(objFANCYPLOT@anumCex)) {
			numCex = objFANCYPLOT@anumCex[i]
			if(numCex <= 0)
				stop(paste("EASY ERROR:FANCYPLOT\n Wrong anumCex defined. PLease use positive numeric value.", sep=""))			
		}
	}
	
	# if(!(objFANCYPLOT@strFormat == "png" | objFANCYPLOT@strFormat == "pdf"))
		# stop(paste("EASY ERROR:FANCYPLOT\n Wrong strFormat defined, PLease use 'png' or 'pdf' \n !!!", sep=""))
	if(!(objFANCYPLOT@strFormat %in% c("png","pdf","tiff","jpeg")))
		stop(paste("EASY ERROR:SPLOT\n Wrong strFormat defined, PLease use 'png','pdf','tiff' or 'jpeg' \n !!!", sep=""))
		
	if(objFANCYPLOT@numCexAxis <= 0) 
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong numCexAxis defined, PLease use numCexAxis > 0 \n !!!", sep=""))
		
	if(objFANCYPLOT@numCexLab <= 0) 
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong numCexLab defined, PLease use numCexLab > 0 \n !!!", sep=""))				
	
	if(length(objFANCYPLOT@anumParMar) != 4) 
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong anumParMar defined, Please use anumParMar of length 4 (Default=5.1;4.1;4.1;2.1) \n !!!", sep=""))				
	
	if(length(objFANCYPLOT@anumParMgp) != 3) 
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong anumParMgp defined, Please use anumParMgp of length 3 (Default=3;1;0) \n !!!", sep=""))				
	
	if(!(objFANCYPLOT@strParBty%in%c("o","l","7","c","u","]")))
		stop(paste("EASY ERROR:FANCYPLOT\n Wrong strParBty defined, Please use 'o','l','7','c','u' or ']' \n !!!", sep=""))				
		
	if(objFANCYPLOT@fileAnnot != "") {
		if(!file.exists(objFANCYPLOT@fileAnnot))
			stop(paste("EASY ERROR:FANCYPLOT\n File fileAnnot\n ",objFANCYPLOT@fileAnnot,"\n does not exist.", sep=""))
		### Cols exist?
			tblAnnot10<-read.table(objFANCYPLOT@fileAnnot,header=T, sep="",  stringsAsFactors=FALSE, nrows = 10)
		
			isAv <- "Chr" %in% names(tblAnnot10)
			if(!isAv)
				stop(paste(" EASY ERROR:FANCYPLOT\n Column 'Chr' is not available in fileAnnot. PLease correct file header.", sep=""))
			isAv <- "Pos" %in% names(tblAnnot10)
			if(!isAv)
				stop(paste(" EASY ERROR:FANCYPLOT\n Column 'Pos' is not available in fileAnnot. PLease correct file header.", sep=""))
			isAv <- "Colour" %in% names(tblAnnot10)
			if(!isAv)
				stop(paste(" EASY ERROR:FANCYPLOT\n Column 'Colour' is not available in fileAnnot. PLease correct file header.", sep=""))	
	}
	
}
FANCYPLOT.run <- function(objFANCYPLOT, objGWA) {

	acolFPlot 				<- 	objFANCYPLOT@acolFPlot
	astrFColours			<- 	objFANCYPLOT@astrFColours
	numFColourPvalLim 		<- 	objFANCYPLOT@numFColourPvalLim
	rcdFColourCrit			<- 	objFANCYPLOT@rcdFColourCrit
	
	strYlab					<- 	objFANCYPLOT@strYlab
		
	colInChr 				<- 	objFANCYPLOT@colInChr
	colInPos 				<- 	objFANCYPLOT@colInPos
	astrDefaultColourChr	<- 	objFANCYPLOT@astrDefaultColourChr
	arcdColourCrit			<-	objFANCYPLOT@arcdColourCrit
	astrColour				<-	objFANCYPLOT@astrColour
	numPvalOffset 			<-	objFANCYPLOT@numPvalOffset
	arcdAdd2Plot			<-	objFANCYPLOT@arcdAdd2Plot
	## 130829
	anumAddPvalLine			<-	objFANCYPLOT@anumAddPvalLine
	astrAddPvalLineCol		<-	objFANCYPLOT@astrAddPvalLineCol
	anumAddPvalLineLty		<-	objFANCYPLOT@anumAddPvalLineLty
	blnYAxisBreak			<-	objFANCYPLOT@blnYAxisBreak
	numYAxisBreak			<-	objFANCYPLOT@numYAxisBreak
	numCexAxis				<-	objFANCYPLOT@numCexAxis
	numCexLab				<-	objFANCYPLOT@numCexLab
	blnLogPval				<-	objFANCYPLOT@blnLogPval	
	## 130927
	numDefaultSymbol		=	objFANCYPLOT@numDefaultSymbol
	numDefaultCex			=	objFANCYPLOT@numDefaultCex
	arcdSymbolCrit			=	objFANCYPLOT@arcdSymbolCrit
	anumSymbol				=	objFANCYPLOT@anumSymbol
	arcdCexCrit				=	objFANCYPLOT@arcdCexCrit
	anumCex					=	objFANCYPLOT@anumCex
	fileAnnot 				=	objFANCYPLOT@fileAnnot
	numAnnotPosLim			=	objFANCYPLOT@numAnnotPosLim
	numAnnotPvalLim			=	objFANCYPLOT@numAnnotPvalLim
	
	plot.title 				<-	objGWA@fileInShortName
	############################
	
	iChr = match(colInChr, names(objGWA@tblGWA))
	objGWA@tblGWA[[iChr]] = as.numeric(objGWA@tblGWA[[iChr]])
	iPos = match(colInPos, names(objGWA@tblGWA))
	objGWA@tblGWA[[iPos]] = as.numeric(objGWA@tblGWA[[iPos]])
	isRemove = is.na(objGWA@tblGWA[[iChr]]) | is.na(objGWA@tblGWA[[iPos]])
	if(any(isRemove)) objGWA <- GWADATA.removerows(objGWA, which(isRemove))
		
	tblPlot = data.frame()
	
	objGWAin <- objGWA
	
	for(i in 1:length(acolFPlot)) {
		
		colFPlot = acolFPlot[i]
		
		objGWA <- objGWAin
		
		iVal = match(colFPlot, names(objGWA@tblGWA))
				
		objGWA@tblGWA[[iVal]] = as.numeric(objGWA@tblGWA[[iVal]])
		
		isRemove = is.na(objGWA@tblGWA[[iVal]])
		if(any(isRemove)) objGWA <- GWADATA.removerows(objGWA, which(isRemove))
		
		############################
		#### Compile initital colour/symbol/cex array 
		
		colplot <- plotCritKey <- rep(NA, dim(objGWA@tblGWA)[1])
		symplot <- rep(numDefaultSymbol, dim(objGWA@tblGWA)[1])
		cexplot <- rep(numDefaultCex, dim(objGWA@tblGWA)[1])
		recolour <- rep(TRUE, dim(objGWA@tblGWA)[1])
		
		# rcdFColourCrit
	
		if(rcdFColourCrit != "") {
			objRCD 		<- RCD(rcdFColourCrit)
			recolour 	<- RCD.eval(objRCD, objGWA)
		} 
		
		if(arcdColourCrit[1] != "") {
			for(iColourCrit in 1:length(arcdColourCrit)) {
				rcdColourCrit 		<- arcdColourCrit[iColourCrit]
				strColour 			<- astrColour[iColourCrit]
				objRCD 	<- RCD(rcdColourCrit)
				out 	<- RCD.eval(objRCD, objGWA)
				colplot[out] <- strColour
				plotCritKey[out]= iColourCrit
			}
		} 
		if(arcdSymbolCrit[1] != "") {
			for(iSymbolCrit in 1:length(arcdSymbolCrit)) {
				rcdSymbolCrit 		<- arcdSymbolCrit[iSymbolCrit]
				numSymbol 			<- anumSymbol[iSymbolCrit]
				objRCD 	<- RCD(rcdSymbolCrit)
				out 	<- RCD.eval(objRCD, objGWA)
				symplot[out] <- numSymbol
			}
		}
		if(arcdCexCrit[1] != "") {
			for(iCexCrit in 1:length(arcdCexCrit)) {
				rcdCexCrit 		<- arcdCexCrit[iCexCrit]
				numCex 			<- anumCex[iCexCrit]
				objRCD 	<- RCD(rcdCexCrit)
				out 	<- RCD.eval(objRCD, objGWA)
				cexplot[out] <- numCex
			}
		}
		############################
		#### Log on P-Values
		if(blnLogPval) {
			isInf = objGWA@tblGWA[[iVal]] == Inf
			if(any(isInf)) 
				warning(paste("EASY WARNING:\n There are ",length(which(isInf)), " SNPs with -log10_P=Inf being excluded from the F plot for file \n ", objGWA@fileIn ,sep="" ))
			isOutRange = objGWA@tblGWA[[iVal]] < 0
			if(any(isOutRange))
				warning(paste("EASY WARNING:\n There are ",length(which(isOutRange)), " SNPs with -log10_P<0 being excluded from the F plot for file \n ", objGWA@fileIn ,sep="" ))
			isBelowOffset = objGWA@tblGWA[[iVal]] < -log10(numPvalOffset)
			isRemove = isBelowOffset | isOutRange | isInf
			if(any(isRemove)) {
				objGWA <- GWADATA.removerows(objGWA, which(isRemove))
				colplot <- colplot[-which(isRemove)]
				symplot <- symplot[-which(isRemove)]
				cexplot <- cexplot[-which(isRemove)]
				recolour<- recolour[-which(isRemove)]
				plotCritKey <- plotCritKey[-which(isRemove)]
			}
		} else {
			isZero = objGWA@tblGWA[[iVal]] == 0
			if(any(isZero)) 
				warning(paste("EASY WARNING:\n There are ",length(which(isZero)), " SNPs with P=0 being excluded from the F plot for file \n ", objGWA@fileIn ,sep="" ))
			isOutRange = objGWA@tblGWA[[iVal]] < 0 | objGWA@tblGWA[[iVal]] > 1
			if(any(isOutRange))
				warning(paste("EASY WARNING:\n There are ",length(which(isOutRange)), " SNPs with P<0 or P>1 being excluded from the F plot for file \n ", objGWA@fileIn ,sep="" ))
			isBelowOffset = objGWA@tblGWA[[iVal]] > numPvalOffset
			isRemove = isBelowOffset | isOutRange | isZero
			if(any(isRemove)) {
				objGWA <- GWADATA.removerows(objGWA, which(isRemove))
				colplot <- colplot[-which(isRemove)]
				symplot <- symplot[-which(isRemove)]
				cexplot <- cexplot[-which(isRemove)]
				recolour <- recolour[-which(isRemove)]
				plotCritKey <- plotCritKey[-which(isRemove)]
			}
			objGWA@tblGWA[[iVal]] 		= -log10(objGWA@tblGWA[[iVal]])	
		}
		
		objGWA <- GWADATA.getcols(objGWA, c(colFPlot, colInChr, colInPos))
		
		tblPlotTmp <- as.data.frame(objGWA@tblGWA, stringsAsFactors=FALSE)
		tblPlotTmp <- setNames(tblPlotTmp, c("y","chr","pos"))
		
		tblPlotTmp = cbind(tblPlotTmp, colplot, plotCritKey, symplot, cexplot, recolour, idxPvalCol = rep(i,length(colplot)), stringsAsFactors = FALSE)
		
		tblPlot <- rbind(tblPlot, tblPlotTmp)
		
	}
	
	rm(objGWA,objGWAin)
	
	#####
	
	### Annot
	if(fileAnnot != "") {
		tblAnnot<-read.table(fileAnnot,header=T, sep="",  stringsAsFactors=FALSE)
		isSignif <- tblPlot$y > -log10(numAnnotPvalLim)
		isSignif[is.na(isSignif)] <- FALSE
		for(iLocus in 1:nrow(tblAnnot)) {
			#isLocus = is.na(tblPlot$colplot) & tblPlot$chr == tblAnnot$Chr[iLocus] & abs(tblPlot$pos-tblAnnot$Pos[iLocus])<numAnnotPosLim			
			isLocus = (tblPlot$chr == tblAnnot$Chr[iLocus]) & (abs(tblPlot$pos-tblAnnot$Pos[iLocus])<numAnnotPosLim)
			isColour = isLocus & isSignif
			if(any(isColour)) {
				tblPlot$colplot[isLocus] <- tblAnnot$Colour[iLocus]
				tblPlot$plotCritKey[isLocus] <- Inf
			}
		}
	}
	
	############################
	#### Compile x-axes 
	
	iOrderChrPos = order(tblPlot$chr,tblPlot$pos)
	tblPlot<-tblPlot[iOrderChrPos,] 
	
	##### Calc. position - vector
	arr_pos<-tblPlot$pos
	
	arr_pos_min<-tapply(tblPlot$pos,tblPlot$chr,min)
	arr_pos_max<-tapply(tblPlot$pos,tblPlot$chr,max)
	
	## tapply sortiert Ausgabe nach chr !!!
	
	arr_chr_uni<-sort(unique(tblPlot$chr))
	
	arr_dpos<-arr_pos_max-arr_pos_min+1
	num_chr1<-min(arr_chr_uni)
	
	# Set first appearing chr to minimum position
	arr_pos[tblPlot$chr==num_chr1]<-arr_pos[tblPlot$chr==num_chr1]-arr_pos_min[1]+1
	
	arr_chr2end<-tblPlot$chr[tblPlot$chr>num_chr1]
	arr_chr2end<-unique(arr_chr2end)
	arr_chr2end<-sort(arr_chr2end)

	count=2
	
	pos_lim = arr_labpos = rep(NA,length(arr_chr2end)+1)
	pos_lim[1] = max(arr_pos[tblPlot$chr==num_chr1])
	arr_labpos[1]<- min(arr_pos[tblPlot$chr==num_chr1]) + (max(arr_pos[tblPlot$chr==num_chr1]) - min(arr_pos[tblPlot$chr==num_chr1]))/2

	for(i in arr_chr2end)  #23?
	{
		is_chr = tblPlot$chr==i
		arr_pos[is_chr]<-sum(arr_dpos[1:(count-1)])+arr_pos[is_chr]-arr_pos_min[count]+1
	
		pos_lim[count] = max(arr_pos[is_chr])
		# arr_labpos[count]<-tapply(arr_pos,tblPlot$chr,mean)
		# arr_labname<-paste("chr",arr_chr,sep="")
		arr_labpos[count]<- min(arr_pos[is_chr]) + (max(arr_pos[is_chr]) - min(arr_pos[is_chr]))/2
	
		count=count+1
	}
	
	tblPlot = cbind(tblPlot, "x" = arr_pos, stringsAsFactors = FALSE)
	#tblPlot = cbind(tblPlot, "x" = as.integer(arr_pos))
	
	############################
	##### ReCheck Colouring and symbols:
	# create array, with unique, sorted, appearing chromosomes:
	arr_chr<-c(num_chr1,arr_chr2end)
	arr_labname<-paste("chr",arr_chr,sep="")
	arr_idx2=seq(1,length(arr_chr),by=2)
	# e.g. arr_idx2 = 1,3,5, ...
	#### Set default colours:
	isSetDefaultColour = is.na(tblPlot$colplot)

	tblPlot$colplot[isSetDefaultColour]	<-	ifelse(tblPlot$chr[isSetDefaultColour]%in%arr_chr[arr_idx2],astrDefaultColourChr[1],astrDefaultColourChr[2])
	

	strXlab = "Chromosome"
	# if(!blnLogPval) strYlab = paste("-log10 (",colFPlot,")",sep="")
	# else strYlab = colFPlot

	################################################################################################################
	###
	## FANCYPLOT RE-ORDERING

	#astrFColours			<- 	objFANCYPLOT@astrFColours
	#numFColourPvalLim 		<- 	objFANCYPLOT@numFColourPvalLim
	
	isRecolour = tblPlot$recolour & tblPlot$y > -log10(numFColourPvalLim)
		
	if(any(isRecolour)) {
		
		tblPlotRecolour <- tblPlot[which(isRecolour),]
		tblPlot <- tblPlot[-which(isRecolour),]
		
		## re-set colplot
		for(i in 1:nrow(tblPlotRecolour)) tblPlotRecolour$colplot[i] <- astrFColours[tblPlotRecolour$idxPvalCol[i]]
		
		## reset plotCritKey
		plotCritKey <- tblPlotRecolour$plotCritKey
		chr <- tblPlotRecolour$chr
		pos <- tblPlotRecolour$pos
		yTmp <- tblPlotRecolour$y
		idxPvalCol <- tblPlotRecolour$idxPvalCol
		## keep NA for those that will be overdraw
		## the ones drawn last shall get the highest plotCritKey
		plotCritKey <- rep(NA,length(plotCritKey))
		
		iDrawCount = 1
		while(any(is.na(plotCritKey))) {
			
			idxMax <- which(yTmp == max(yTmp,na.rm=TRUE))[1]
			isMaxLocusMaxTrait = chr == chr[idxMax] & abs(pos - pos[idxMax]) < 500000 & idxPvalCol == idxPvalCol[idxMax]
			plotCritKey[which(isMaxLocusMaxTrait)] <- iDrawCount
			
			yTmp[which(isMaxLocusMaxTrait)] <- NA
			
			iDrawCount = iDrawCount + 1
		}
		tblPlotRecolour$plotCritKey <- plotCritKey
		
		tblPlot <- rbind(tblPlotRecolour,tblPlot)
	}
	
	################################################################################################################

	yAxisTickPos = NULL
	yAxisTickLab = TRUE
	anumAddPvalLinePos = -log10(anumAddPvalLine)
	
	############################	
	####### 130829 - Rescale y axis
	yMax = max(tblPlot$y)
	
	if(blnYAxisBreak & numYAxisBreak<yMax) {
	
		tblPlot = tblPlot[order(tblPlot$y),]
		y = tblPlot$y
		
		yu = y[y<=numYAxisBreak] # [0;22] -> [0;80]
		yo = y[y>numYAxisBreak] # [22;yMax] -> [80;100]
		
		yus = yu*80/numYAxisBreak
		yos = 80 - 20*(numYAxisBreak-yo)/(yMax-numYAxisBreak)
		
		tblPlot$y = c(yus,yos)
		
		yAxisTickLabU = pretty(c(0,numYAxisBreak),n=4,min.n=0)
		yAxisTickLabU = yAxisTickLabU[-which(yAxisTickLabU>=numYAxisBreak)]
		yAxisTickPosU = yAxisTickLabU*80/numYAxisBreak
		
		yAxisTickLabO = pretty(c(numYAxisBreak,yMax),n=4,min.n=0)
		yAxisTickLabO = yAxisTickLabO[-which(yAxisTickLabO<=numYAxisBreak)]
		yAxisTickPosO = 80 - 20*(numYAxisBreak-yAxisTickLabO)/(yMax-numYAxisBreak)
		
		yAxisTickLab = c(yAxisTickLabU,yAxisTickLabO)
		yAxisTickPos = c(yAxisTickPosU,yAxisTickPosO)
			
		isLineU = anumAddPvalLinePos<numYAxisBreak
		anumAddPvalLinePos[isLineU] = anumAddPvalLinePos[isLineU]*80/numYAxisBreak
		anumAddPvalLinePos[!isLineU] = 80 - 20*(numYAxisBreak-anumAddPvalLinePos[!isLineU])/(yMax-numYAxisBreak)
		
	}
	
	
	################################################################################################################
	## Resort (IMPORTANT for correct colouring!)
	tblPlot = tblPlot[order(tblPlot$plotCritKey,na.last=FALSE),]
	################################################################################################################
	####### Start plot
		
	plot( tblPlot$x, tblPlot$y, col=tblPlot$colplot, pch=tblPlot$symplot, cex=tblPlot$cexplot, cex.lab = numCexLab, xaxt="n",yaxt="n", ylab=strYlab, xlab="",ylim=c(0,max(tblPlot$y)))
	
	# Achsenlinien:
	axis(2, at =yAxisTickPos,  labels = yAxisTickLab, cex.axis = numCexAxis)
	mtext("Chromosome", side=1, line=2.5, cex=numCexLab, padj=2)
	axis(1,at=arr_labpos,labels=arr_labname,las=3,cex.axis = numCexAxis)
	abline(a=0,b=0,col="gray3",lty=6)
	for(k in 1:length(pos_lim)) abline(v=pos_lim[k],col="gray3",lty=3)
	grid(nx = NA, ny = NA, col = "gray", lty = "dotted")
	
	for(k in 1:length(anumAddPvalLine)) {
		if(is.na(anumAddPvalLineLty[k])) anumAddPvalLineLty[k] <- 6
		if(is.na(astrAddPvalLineCol[k])) astrAddPvalLineCol[k] <- "red"
		
		abline(anumAddPvalLinePos[k],0,col = astrAddPvalLineCol[k],lty = anumAddPvalLineLty[k])
		axis(4,at=anumAddPvalLinePos[k],labels=anumAddPvalLine[k],las=3,cex.axis = numCexAxis)
	}
	#abline(yGwsLine,0,col = "red",lty = 6)	
	#axis(4,at=yGwsLine,labels="5e-8",las=3,cex.axis = 1.5)
	
	if(blnYAxisBreak & numYAxisBreak<yMax) {
		axis.break(2, 80, style = "zigzag")
		abline(80,0,col = "grey",lty = 1)	
	}
	
	if(all(arcdAdd2Plot != "")) {
		for(rcdTmp in arcdAdd2Plot) {
			# for(colTmp in names(tblPlot)) {
				# if(length(grep(colTmp,rcdTmp)) > 0) {
					# icolTmp=which(names(tblPlot)==colTmp)
					# assign(colTmp,as.numeric(as.character(tblPlot[,icolTmp])))
					# # create arrays, that are used within criterion and name them accordingly
					# # tblIn$pmen -> create array pmen if criterion e.g. pmen>0.3
				# }
			# }
			eval(parse(text=rcdTmp))
		}
	}
	
}
FANCYPLOT <- function(strEqcCommand){ 
	## Wrapper for class definition
	FANCYPLOTout <- setFANCYPLOT(new("FANCYPLOT", strEqcCommand = strEqcCommand))
	return(FANCYPLOTout)
}

