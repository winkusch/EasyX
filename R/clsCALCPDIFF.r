setClass("CALCPDIFF",
	representation = representation(
						strEqcCommand				=	"character",
						acolBETAs					=	"character",
						acolSEs						=	"character",
						acolZSCOREs					=	"character",
						acolNs						=	"character",
						blnCovCorrection			=	"logical",
						# bln2sided					=	"logical",
						# rcdTestDirection			=	"character",
						colOutPdiff					=	"character",
						strMethod					=	"character",
						strTag						=	"character"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						acolBETAs					=	"",
						acolSEs						=	"",
						acolZSCOREs					=	"",
						acolNs						=	"",
						blnCovCorrection			=	FALSE,
						# bln2sided					=	TRUE,
						# rcdTestDirection			=	"",
						colOutPdiff					=	"pdiff",
						strMethod					=	"",
						strTag						=	""
						)
)


setGeneric("setCALCPDIFF", function(object) standardGeneric("setCALCPDIFF"))
setMethod("setCALCPDIFF", signature = (object = "CALCPDIFF"), function(object) {
	
	#aEqcSlotNamesIn = c("colBeta1", "colSe1", "colBeta2" , "colSe2", "blnCovCorrection","bln2sided","rcdTestDirection","colOutPdiff")
	aEqcSlotNamesIn = c("acolBETAs" , "acolSEs","acolZSCOREs" , "acolNs", "blnCovCorrection","colOutPdiff","strTag")
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
validCALCPDIFF <- function(objCP) {
	
	if(any(objCP@acolBETAs != "") | any(objCP@acolSEs != "")) strMethod = "BETA"
	else if(any(objCP@acolZSCOREs != "")) strMethod = "ZSCORE"
	else stop(paste(" EASY ERROR:CALCPDIFF\n Please either set (--acolBETAs AND --acolSEs) OR (--acolZSCOREs AND --acolNs) !!!", sep=""))
	
	
	if(strMethod == "BETA") {
		if(length(objCP@acolBETAs)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFF\n Please set exactly 2 beta columns for acolBETAs (e.g. --acolBETAs beta1;beta2) !!!", sep=""))
		if(length(objCP@acolSEs)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFF\n Please set exactly 2 se columns for acolSEs (e.g. --acolSEs se1;se2) !!!", sep=""))
			
		if(any(objCP@acolBETAs == ""))
			stop(paste(" EASY ERROR:CALCPDIFF\n No Effect estimate columns acolBETAs defined.\n Please set acolBETAs.", sep=""))
		if(any(objCP@acolSEs == ""))
			stop(paste(" EASY ERROR:CALCPDIFF\n No Effect estimate columns acolSEs defined.\n Please set acolSEs.", sep=""))
	} else {
		if(length(objCP@acolZSCOREs)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFF\n Please set exactly 2 Z score columns for acolZSCOREs (e.g. --acolZSCOREs z1;z2) !!!", sep=""))
		if(length(objCP@acolNs)!=2) 
			stop(paste(" EASY ERROR:CALCPDIFF\n Please set exactly 2 sample size columns for acolNs (e.g. --acolNs n1;n2) !!!", sep=""))
			
		if(any(objCP@acolZSCOREs == ""))
			stop(paste(" EASY ERROR:CALCPDIFF\n No Effect Z score columns acolZSCOREs defined.\n Please set --acolZSCOREs.", sep=""))
		if(any(objCP@acolNs == ""))
			stop(paste(" EASY ERROR:CALCPDIFF\n No Effect sample size columns acolNs defined.\n Please set --acolNs.", sep=""))
	
	}
	# if(!bln2sided & rcdTestDirection =="" ) 
		# stop(paste(" EASY ERROR:CALCPDIFF\n Please define a direction rcdTestDirection (1 or 2) that will be used to calculate the 1-sided P-Value!!!", sep=""))
	
	objCP@strMethod <- strMethod
	
	return(objCP)
}

#############################################################################################################################
CALCPDIFF.run <- function(objCP, objGWA, objREPORT) {
	
	strTag	<- objCP@strTag
	if(nchar(strTag)>0) strTag = paste(strTag,".",sep="") 
	
	if(objCP@strMethod == "BETA") {
		
		aBeta1 	<- GWADATA.getcol(objGWA, objCP@acolBETAs[1])
		aBeta2 	<- GWADATA.getcol(objGWA, objCP@acolBETAs[2])
		aSe1 	<- GWADATA.getcol(objGWA, objCP@acolSEs[1])
		aSe2 	<- GWADATA.getcol(objGWA, objCP@acolSEs[2])
		
		if(class(aBeta1) != "numeric" & class(aBeta1) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolBETAs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aBeta2) != "numeric" & class(aBeta2) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolBETAs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe1) != "numeric" & class(aSe1) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolSEs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aSe2) != "numeric" & class(aSe2) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolSEs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		#### Two-sided test
		if(objCP@blnCovCorrection) {
			cor.factor<-cor.test(aBeta1,aBeta2,method="spearman",exact=FALSE)
			numCorFactor = cor.factor$estimate
			pdiff<-2*(pnorm(abs((aBeta1-aBeta2)/sqrt((aSe1)^2+(aSe2)^2-2*numCorFactor*aSe1*aSe2)),lower.tail=FALSE))
			
			objREPORT <- REPORT.addval(objREPORT,paste(strTag,"numCor.",objCP@acolBETAs[1],".",objCP@acolBETAs[2],sep=""),numCorFactor)
		}
		else {
			pdiff<-2*(pnorm(abs((aBeta1-aBeta2)/sqrt((aSe1)^2+(aSe2)^2)), lower.tail=FALSE))
		}
	} else {
		### diff from z scores and N
		aZ1 	<- GWADATA.getcol(objGWA, objCP@acolZSCOREs[1])
		aZ2 	<- GWADATA.getcol(objGWA, objCP@acolZSCOREs[2])
		aN1 	<- GWADATA.getcol(objGWA, objCP@acolNs[1])
		aN2 	<- GWADATA.getcol(objGWA, objCP@acolNs[2])
		
		if(class(aZ1) != "numeric" & class(aZ1) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolZSCOREs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aZ2) != "numeric" & class(aZ2) != "double" ) stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolZSCOREs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aN1) != "numeric" & class(aN1) != "double" & class(aN1) != "integer") stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolNs[1],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		if(class(aN2) != "numeric" & class(aN2) != "double" & class(aN2) != "integer") stop(paste("EASY ERROR:CALCPDIFF\nColumn \n",objCP@acolNs[2],"\n is not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
		
		
		#### Two-sided test
		if(objCP@blnCovCorrection) {
			cor.factor<-cor.test(aZ1/sqrt(aN1),aZ2/sqrt(aN2),method="spearman",exact=FALSE)
			numCorFactor = cor.factor$estimate
			pdiff<-2*(pnorm(abs((aZ1/sqrt(aN1)-aZ2/sqrt(aN2))/sqrt(1/aN1+1/aN2-2*numCorFactor*sqrt(1/aN1)*sqrt(1/aN1))), lower.tail=FALSE))
			objREPORT <- REPORT.addval(objREPORT,paste(strTag, "numCor.",objCP@acolZSCOREs[1],"overSqrt",objCP@acolNs[1],".",objCP@acolZSCOREs[2],"overSqrt",objCP@acolNs[2],sep=""),numCorFactor)
		}
		else {
			pdiff<-2*(pnorm(abs((aZ1/sqrt(aN1)-aZ2/sqrt(aN2))/sqrt(1/aN1+1/aN2)), lower.tail=FALSE))
		}
	
	}
	
	objGWA <- GWADATA.cbind(objGWA, pdiff, objCP@colOutPdiff)
	
	lsOut <- list(objGWA, objREPORT)
	return(lsOut)
}

CALCPDIFF <- function(strEqcCommand){ 
	## Wrapper for class definition
	CALCPDIFFout <- setCALCPDIFF(new("CALCPDIFF", strEqcCommand = strEqcCommand))
	CALCPDIFFout <- validCALCPDIFF(CALCPDIFFout)
	return(CALCPDIFFout)

}
