setClass("CALCPDIFFDIFF",
	representation = representation(
						strEqcCommand				=	"character",
						acolBETA1s					=	"character",
						acolSE1s					=	"character",
						acolBETA2s					=	"character",
						acolSE2s					=	"character",
						# acolZSCOREs				=	"character",
						# acolNs					=	"character",
						# blnCovCorrection			=	"logical",
						# bln2sided					=	"logical",
						# rcdTestDirection			=	"character",
						colOutPdiffdiff				=	"character"
						# strMethod					=	"character"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						acolBETA1s					=	"",
						acolSE1s						=	"",
						acolBETA2s					=	"",
						acolSE2s						=	"",
						# acolZSCOREs					=	"",
						# acolNs						=	"",
						# blnCovCorrection			=	FALSE,
						# bln2sided					=	TRUE,
						# rcdTestDirection			=	"",
						colOutPdiffdiff					=	"pdiffdiff"
						# strMethod					=	""
						)
)


setGeneric("setCALCPDIFFDIFF", function(object) standardGeneric("setCALCPDIFFDIFF"))
setMethod("setCALCPDIFFDIFF", signature = (object = "CALCPDIFFDIFF"), function(object) {
	
	#aEqcSlotNamesIn = c("colBeta1", "colSe1", "colBeta2" , "colSe2", "blnCovCorrection","bln2sided","rcdTestDirection","colOutPdiffdiff")
	aEqcSlotNamesIn = c("acolBETA1s" , "acolSE1s","acolBETA2s","acolSE2s","colOutPdiffdiff")
	#aEcfSlotNamesIn = c("arcdAddCol", "astrAddColNames")

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
validCALCPDIFFDIFF <- function(objCP) {
	
	#if(any(objCP@acolBETAs != "") | any(objCP@acolSEs != "")) strMethod = "BETA"
	#else if(any(objCP@acolZSCOREs != "")) strMethod = "ZSCORE"
	#else stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please either set (--acolBETAs AND --acolSEs) OR (--acolZSCOREs AND --acolNs) !!!", sep=""))
	
	
	# if(strMethod == "BETA") {
		if(length(objCP@acolBETA1s)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 beta columns for acolBETA1s (e.g. --acolBETA1s beta11;beta12) !!!", sep=""))
		if(length(objCP@acolSE1s)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 se columns for acolSE1s (e.g. --acolSE1s se11;se12) !!!", sep=""))
			
		if(any(objCP@acolBETA1s == ""))
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect estimate columns acolBETA1s defined.\n Please set acolBETA1s.", sep=""))
		if(any(objCP@acolSE1s == ""))
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect estimate columns acolSE1s defined.\n Please set acolSE1s.", sep=""))
			
		if(length(objCP@acolBETA2s)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 beta columns for acolBETA2s (e.g. --acolBETA2s beta21;beta22) !!!", sep=""))
		if(length(objCP@acolSE2s)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 se columns for acolSE2s (e.g. --acolSE2s se21;se22) !!!", sep=""))
			
		if(any(objCP@acolBETA2s == ""))
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect estimate columns acolBETA2s defined.\n Please set acolBETA2s.", sep=""))
		if(any(objCP@acolSE2s == ""))
			stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect estimate columns acolSE2s defined.\n Please set acolSE2s.", sep=""))
			
			
	# } else {
		# if(length(objCP@acolZSCOREs)!=2) 
			# stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 Z score columns for acolZSCOREs (e.g. --acolZSCOREs z1;z2) !!!", sep=""))
		# if(length(objCP@acolNs)!=2) 
			# stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please set exactly 2 sample size columns for acolNs (e.g. --acolNs n1;n2) !!!", sep=""))
			
		# if(any(objCP@acolZSCOREs == ""))
			# stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect Z score columns acolZSCOREs defined.\n Please set --acolZSCOREs.", sep=""))
		# if(any(objCP@acolNs == ""))
			# stop(paste(" EASY ERROR:CALCPDIFFDIFF\n No Effect sample size columns acolNs defined.\n Please set --acolNs.", sep=""))
	
	# }
	# if(!bln2sided & rcdTestDirection =="" ) 
		# stop(paste(" EASY ERROR:CALCPDIFFDIFF\n Please define a direction rcdTestDirection (1 or 2) that will be used to calculate the 1-sided P-Value!!!", sep=""))
	
	# objCP@strMethod <- strMethod
	
	return(objCP)
}

#############################################################################################################################
CALCPDIFFDIFF.run <- function(objCP, objGWA, objREPORT) {
	
	#if(objCP@strMethod == "BETA") {
		iBeta11 = match(objCP@acolBETA1s[1], objGWA@aHeader)
		iBeta12 = match(objCP@acolBETA1s[2], objGWA@aHeader)
		iSe11 = match(objCP@acolSE1s[1], objGWA@aHeader)
		iSe12 = match(objCP@acolSE1s[2], objGWA@aHeader)
		iBeta21 = match(objCP@acolBETA2s[1], objGWA@aHeader)
		iBeta22 = match(objCP@acolBETA2s[2], objGWA@aHeader)
		iSe21 = match(objCP@acolSE2s[1], objGWA@aHeader)
		iSe22 = match(objCP@acolSE2s[2], objGWA@aHeader)
		
		if(is.na(iBeta11)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA1s[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iBeta12)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA1s[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iSe11)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE1s[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iSe12)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE1s[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iBeta21)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA2s[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iBeta22)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA2s[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iSe21)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE2s[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(is.na(iSe22)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE2s[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		aBeta11	<- objGWA@tblGWA[,iBeta11]
		aBeta12 <- objGWA@tblGWA[,iBeta12]
		aSe11 	<- objGWA@tblGWA[,iSe11]
		aSe12 	<- objGWA@tblGWA[,iSe12]
		aBeta21 <- objGWA@tblGWA[,iBeta21]
		aBeta22 <- objGWA@tblGWA[,iBeta22]
		aSe21 	<- objGWA@tblGWA[,iSe21]
		aSe22 	<- objGWA@tblGWA[,iSe22]
		
		if(class(aBeta11) != "numeric" & class(aBeta11) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA1s[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aBeta12) != "numeric" & class(aBeta12) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA1s[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe11) != "numeric" & class(aSe11) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE1s[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe12) != "numeric" & class(aSe12) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE1s[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aBeta21) != "numeric" & class(aBeta21) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA2s[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aBeta22) != "numeric" & class(aBeta22) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolBETA2s[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe21) != "numeric" & class(aSe21) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE2s[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe22) != "numeric" & class(aSe22) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolSE2s[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		
		#### Two-sided test
		# if(objCP@blnCovCorrection) {
			# cor.factor<-cor.test(aBeta1,aBeta2,method="spearman",exact=FALSE)
			# numCorFactor = cor.factor$estimate
			# pdiffdiff<-2*(pnorm(abs((aBeta1-aBeta2)/sqrt((aSe1)^2+(aSe2)^2-2*numCorFactor*aSe1*aSe2)),lower.tail=FALSE))
			# objREPORT <- REPORT.addval(objREPORT,paste("numCor",objCP@acolBETAs[1],objCP@acolBETAs[2],sep="."),numCorFactor)
		# }
		# else {
		
			tdiffdiff <- ((aBeta11-aBeta12)-(aBeta21-aBeta22))/sqrt((aSe11)^2+(aSe12)^2+(aSe21)^2+(aSe22)^2)
			pdiffdiff <- 2*pnorm(abs(tdiffdiff), lower.tail=FALSE)	
	
		# }
	# } else {
		# ### diff from z scores and N
		# iZ1 = match(objCP@acolZSCOREs[1], objGWA@aHeader)
		# iZ2 = match(objCP@acolZSCOREs[2], objGWA@aHeader)
		# iN1 = match(objCP@acolNs[1], objGWA@aHeader)
		# iN2 = match(objCP@acolNs[2], objGWA@aHeader)
		
		# if(is.na(iZ1)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolZSCOREs[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(is.na(iZ2)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolZSCOREs[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(is.na(iN1)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolNs[1],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(is.na(iN2)) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolNs[2],"\n is not available in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		# aZ1 	<- objGWA@tblGWA[,iZ1]
		# aZ2 	<- objGWA@tblGWA[,iZ2]
		# aN1 	<- objGWA@tblGWA[,iN1]
		# aN2 	<- objGWA@tblGWA[,iN2]
		
		# if(class(aZ1) != "numeric" & class(aZ1) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolZSCOREs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(class(aZ2) != "numeric" & class(aZ2) != "double" ) stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolZSCOREs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(class(aN1) != "numeric" & class(aN1) != "double" & class(aN1) != "integer") stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolNs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		# if(class(aN2) != "numeric" & class(aN2) != "double" & class(aN2) != "integer") stop(paste("EASY ERROR:CALCPDIFFDIFF\nColumn \n",objCP@acolNs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		
		# #### Two-sided test
		# if(objCP@blnCovCorrection) {
			# cor.factor<-cor.test(aZ1/sqrt(aN1),aZ2/sqrt(aN2),method="spearman",exact=FALSE)
			# numCorFactor = cor.factor$estimate
			# pdiffdiff<-2*(pnorm(abs((aZ1/sqrt(aN1)-aZ2/sqrt(aN2))/sqrt(1/aN1+1/aN2-2*numCorFactor*sqrt(1/aN1)*sqrt(1/aN1))), lower.tail=FALSE))
			# objREPORT <- REPORT.addval(objREPORT,paste("numCor",objCP@acolZSCOREs[1],"overSqrt",objCP@acolNs[1],objCP@acolZSCOREs[2],"overSqrt",objCP@acolNs[2],sep="."),numCorFactor)
		# }
		# else {
			# pdiffdiff<-2*(pnorm(abs((aZ1/sqrt(aN1)-aZ2/sqrt(aN2))/sqrt(1/aN1+1/aN2)), lower.tail=FALSE))
		# }
	
	# }
	
	objGWA <- GWADATA.cbind(objGWA, pdiffdiff, objCP@colOutPdiffdiff)
	
	lsOut <- list(objGWA, objREPORT)
	return(lsOut)
}

CALCPDIFFDIFF <- function(strEqcCommand){ 
	## Wrapper for class definition
	CALCPDIFFDIFFout <- setCALCPDIFFDIFF(new("CALCPDIFFDIFF", strEqcCommand = strEqcCommand))
	CALCPDIFFDIFFout <- validCALCPDIFFDIFF(CALCPDIFFDIFFout)
	return(CALCPDIFFDIFFout)

}
