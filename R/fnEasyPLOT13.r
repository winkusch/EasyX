fnOpenPlot <- function(objIn, objPlot, strSuffix="") {
	
	strPlotName = objPlot@strPlotName
	strFormat = objPlot@strFormat
	
	if(class(objIn) == "GWADATA") {
		pathOut <- objIn@pathOut
		if(strsplit(pathOut,"")[[1]][nchar(pathOut)] != "/") pathOut <- paste(pathOut,"/",sep="")	
		fileOutBody	<- paste(pathOut,objIn@fileInShortName,sep="")
		fileOut	<- paste(fileOutBody,".",strPlotName,".",strFormat,sep="")
		if(objIn@blnOverwriteResults & file.exists(fileOut)) file.remove(fileOut)
		i = 1
		while(file.exists(fileOut)) {
			fileOut <- paste(fileOutBody,".",strPlotName,i,".",strFormat,sep="")
			i = i + 1
		}
	}
	if(class(objIn) == "REPORT") {
		fileOutBody <- objIn@fileReportBody
		#fileOut	<- paste(fileOutBody,".",strFormat,sep="")
		fileOut	<- paste(fileOutBody,".",strPlotName,".",strFormat,sep="")
		if(objIn@blnOverwriteReport & file.exists(fileOut)) file.remove(fileOut)
		i = 1
		while(file.exists(fileOut)) {
			#fileOut <- paste(fileOutBody,i,".",strFormat,sep="")
			fileOut <- paste(fileOutBody,".",strPlotName,i,".",strFormat,sep="")
			i = i + 1
		}
	}
	
	if(strFormat == "png")
		CairoPNG(filename = fileOut,width=objPlot@numWidth,height=objPlot@numHeight)
	else if(strFormat == "pdf")
		CairoPDF(file = fileOut, width=objPlot@numWidth,height=objPlot@numHeight)
	else if(strFormat == "tiff")
		CairoTIFF(filename = fileOut,width=objPlot@numWidth,height=objPlot@numHeight)
	else if(strFormat == "jpeg")
		CairoJPEG(filename = fileOut,width=objPlot@numWidth,height=objPlot@numHeight)	
	
	par(xpd=F, 
		bty=objPlot@strParBty, 
		mar=objPlot@anumParMar, 
		mgp=objPlot@anumParMgp
		)
	
	## GRAPHICAL PARAMETERS USING par():
	## mar = Margin sizes c(bottom, left, top, right) = c(5, 4, 4, 2) + 0.1 default
	##		in LINES
	## xpd = T: PLottet über Rand hinaus
	## mgp = Distance in LINES 
	##		of c(axis labels or titles from the axes; tick mark labels from the axes; tick mark symbols from the axes) 
	##		= default = c(3, 1, 0)
	## bty = Box around plot:  If âbtyâ is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting
    ## A value of "n" suppresses the box.
	
}
#fnOpenMultiPlot <- function(fileOutBody, strSuffix, numFiles) {
fnOpenMultiPlot <- function(fileOutBody, objPlot, numFiles, afileOutMultiPlot, blnOverwriteResults) {
	
	strPlotName = objPlot@strPlotName
	strFormat = objPlot@strFormat
	
	fileOut	<- paste(fileOutBody,".multi.",strPlotName,".",strFormat,sep="")
	i = 1
	while(fileOut%in%afileOutMultiPlot) {
		fileOut <- paste(fileOutBody,".multi.",strPlotName,i,".",strFormat,sep="")
		i = i + 1
	}
	if(blnOverwriteResults & file.exists(fileOut)) file.remove(fileOut)
	i = 1
	while(file.exists(fileOut)) {
		fileOut <- paste(fileOutBody,".multi.",strPlotName,i,".",strFormat,sep="")
		i = i + 1
	}
	
	numX = ceiling(sqrt(numFiles))
	numY = ceiling(numFiles/ceiling(sqrt(numFiles)))

	#png(file=fileOut,width=(250*numX),height=(250*numY))
	if(strFormat == "png")
		CairoPNG(filename=fileOut,width=(250*numX),height=(250*numY))
	else if(strFormat == "pdf")
		CairoPDF(file = fileOut, width=(3*numX),height=(3*numY))
	else if(strFormat == "tiff")
		CairoTIFF(filename=fileOut,width=(250*numX),height=(250*numY))
	else if(strFormat == "jpeg")
		CairoJPEG(filename=fileOut,width=(250*numX),height=(250*numY))
		
	par(mfrow=c(numY,numX)) 
	par(mar=c(3,3,3,1)) 
	par(mgp=c(2,0.7,0))
	
	return(fileOut)
}
fnClosePlot <- function() {	
	dev.off()
}
fnAddPlot <- function(iMultiPlot) {
	dev.set(dev.list()[iMultiPlot])
}
