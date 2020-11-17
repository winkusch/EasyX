setClass("WEIGHTEDHYPOTHESIS",
	representation = representation(
						strEqcCommand			=	"character",
						rcdCriterion			=	"character",
						colPvalOverall			=	"character",
						colPvalDiff				=	"character",
						numBinsize				=	"numeric",
						colOut					=	"character",
						strTag					=	"character"
						),
	prototype = prototype(
						strEqcCommand			=	"",
						rcdCriterion			=	"",
						colPvalOverall			=	"",
						colPvalDiff				=	"",
						numBinsize				=	5,
						colOut					=	"",
						strTag					=	"WH"
						)
)


setGeneric("setWEIGHTEDHYPOTHESIS", function(object) standardGeneric("setWEIGHTEDHYPOTHESIS"))
setMethod("setWEIGHTEDHYPOTHESIS", signature = (object = "WEIGHTEDHYPOTHESIS"), function(object) {
	
	#aEqcSlotNamesIn = c("colBeta1", "colSe1", "colBeta2" , "colSe2", "blnUseGoncalo","bln2sided","rcdTestDirection","strPdiffName")
	aEqcSlotNamesIn = c("rcdCriterion","colPvalOverall","colPvalDiff", "numBinsize","colOut","strTag")
	
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
validWEIGHTEDHYPOTHESIS <- function(objWH) {
	
	if(objWH@colPvalOverall == "")
		stop(paste(" EASY ERROR:WEIGHTEDHYPOTHESIS\n No Overall/Marginal P-Value column defined.\n Please set colPval.", sep=""))
	
	if(objWH@colPvalDiff == "")
		stop(paste(" EASY ERROR:WEIGHTEDHYPOTHESIS\n No Difference/Interaction P-Value column defined.\n Please set colPval.", sep=""))
		
	return(TRUE)
}
WEIGHTEDHYPOTHESIS.GWADATA.valid <- function(objWH, objGWA) {
	
	isAv <- objWH@colPvalOverall %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:WEIGHTEDHYPOTHESIS\n Defined column colPvalOverall \n",objWH@colPvalOverall, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
		
	isAv <- objWH@colPvalDiff %in% objGWA@aHeader
	if(!isAv)
		stop(paste(" EASY ERROR:WEIGHTEDHYPOTHESIS\n Defined column colPvalDiff \n",objWH@colPvalDiff, "\n is not available in GWA data-set \n",objGWA@fileIn,"\n PLease specify correct column name.", sep=""))
		
	return(TRUE)
	
}
#############################################################################################################################
WEIGHTEDHYPOTHESIS.run <- function(objWH, objGWA, objREPORT) {
	
	rcdCriterion 		<- objWH@rcdCriterion
	colPvalOverall		<- objWH@colPvalOverall
	colPvalDiff			<- objWH@colPvalDiff
	numBinsize			<- objWH@numBinsize
	colOut				<- objWH@colOut
	strTag				<- objWH@strTag
	
	if(rcdCriterion != "") {
		objRCD 	<- RCD(rcdCriterion)
		out 	<- RCD.eval(objRCD, objGWA)
		numIndepCrit = length(which(out))
		objGWA.wh <- GWADATA.getrows(objGWA, which(out))
	} else {
		numIndepCrit <- nrow(objGWA@tblGWA)
		objGWA.wh <- objGWA
	}
	
	
	objGWA.wh = GWADATA.sort(objGWA.wh,colPvalOverall,FALSE)
	
	aPov <- GWADATA.getcol(objGWA.wh, colPvalOverall)
	aPdiff <- GWADATA.getcol(objGWA.wh, colPvalDiff)
	
	nsnps = length(aPov)
	pdiffwh <- alphadiff <- rep(NA,nsnps)
	binsize = numBinsize
	varidx = 1
	icount = 1
	
	while(varidx<nsnps) {
		binstart = varidx
		binstop = binstart + binsize - 1
		if(binstop>nsnps) binstop = nsnps
		alphadiff[binstart:binstop] = 0.05/2/icount/binsize
		pdiffwh[binstart:binstop] = aPdiff[binstart:binstop]*2*icount*binsize
		binsize = 2*binsize
		varidx = binstop + 1
		icount = icount + 1
	}
	pdiffwh[pdiffwh>1]<-1
	isSignif = ifelse(aPdiff<alphadiff,1,0)
	
	strNewColName <- ifelse(colOut == "", paste(colPvalDiff,".",strTag,sep=""), colOut)
		
	objGWA.wh <- GWADATA.cbind(objGWA.wh, pdiffwh, strNewColName)
	objGWA.wh <- GWADATA.cbind(objGWA.wh, isSignif, paste(strNewColName,".isSignif",sep=""))
	
	### merges by intersect(names) -> strNewColName and paste(strNewColName,".isSignif",sep="") will be added 
	objGWA <- GWADATA.merge(objGWA,objGWA.wh, strSuffix.In = "", strSuffix.Add = "", blnAll.In = TRUE, blnAll.Add = TRUE, strBy.In = NA, strBy.Add = NA)
	objGWA.wh.signif <- GWADATA.getrows(objGWA.wh, which(as.logical(isSignif)))
	objREPORT <- REPORT.addval(objREPORT,paste(strTag,".numSignif",sep=""),sum(isSignif,na.rm=TRUE))
	
	return(list(objGWA,objGWA.wh.signif,objREPORT))
}

WEIGHTEDHYPOTHESIS <- function(strEqcCommand){ 
	## Wrapper for class definition
	WEIGHTEDHYPOTHESISout <- setWEIGHTEDHYPOTHESIS(new("WEIGHTEDHYPOTHESIS", strEqcCommand = strEqcCommand))
	validWEIGHTEDHYPOTHESIS(WEIGHTEDHYPOTHESISout)
	return(WEIGHTEDHYPOTHESISout)

}
