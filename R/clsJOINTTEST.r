setClass("JOINTTEST",
	representation = representation(
						strEqcCommand				=	"character",
						acolBETAs					=	"character",
						acolSEs						=	"character",
						acolZSCOREs					=	"character",
						colOutPjoint				=	"character",
						strMethod					=	"character",
						colBetaMain					=	"character",
						colBetaInt					=	"character",
						colSeMain					=	"character",
						colSeInt					=	"character",
						colCovar					=	"character"
						),
	prototype = prototype(
						strEqcCommand				=	"",
						acolBETAs					=	"",
						acolSEs						=	"",
						acolZSCOREs					=	"",
						colOutPjoint				=	"pjoint",
						strMethod					=	"",
						colBetaMain					=	"",
						colBetaInt					=	"",
						colSeMain					=	"",
						colSeInt					=	"",
						colCovar					=	""
						)
)


setGeneric("setJOINTTEST", function(object) standardGeneric("setJOINTTEST"))
setMethod("setJOINTTEST", signature = (object = "JOINTTEST"), function(object) {
	
	#aEqcSlotNamesIn = c("colBeta1", "colSe1", "colBeta2" , "colSe2", "blnUseGoncalo","bln2sided","rcdTestDirection","strPdiffName")
	aEqcSlotNamesIn = c("acolBETAs", "acolSEs", "acolZSCOREs" , "colOutPjoint",
						"colBetaMain", "colBetaInt", "colSeMain", "colSeInt", "colCovar")
	
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
validJOINTTEST <- function(objJT) {
	
	if(any(objJT@acolBETAs != "")) strMethod = "Aschard.BETA"
	else if(any(objJT@acolZSCOREs != "")) strMethod = "Aschard.ZSCORE"
	else if(objJT@colBetaMain != "") strMethod = "Kraft.BETA"
	else stop(paste(" EASY ERROR:JOINTTEST\n Please either set (--acolBETAs AND --acolSEs) OR (--acolZSCOREs AND --acolNs) OR (--colBetaMain, --colBetaInt, --colSeMain, --colSeInt AND --colCovar)!!!", sep=""))
	
	if(strMethod == "Aschard.BETA") {
		if(length(objJT@acolBETAs)<2)
			stop(paste(" EASY ERROR:JOINTTEST\n Please set at least 2 beta columns for acolBETAs (e.g. --acolBETAs beta1;beta2) !!!", sep=""))
		if(length(objJT@acolSEs)<2) 
			stop(paste(" EASY ERROR:JOINTTEST\n Please set at least 2 se columns for acolSEs (e.g. --acolSEs se1;se2) !!!", sep=""))
		if(length(objJT@acolBETAs) != length(objJT@acolSEs))
			stop(paste(" EASY ERROR:JOINTTEST\n Number of defined Beta columns acolBETAs does not match number of defined Se columns acolSEs. \n PLease check!!!", sep=""))
		if(any(objJT@acolBETAs == ""))
			stop(paste(" EASY ERROR:JOINTTEST\n No Effect estimate columns acolBETAs defined.\n Please set acolBETAs.", sep=""))
		if(any(objJT@acolSEs == ""))
			stop(paste(" EASY ERROR:JOINTTEST\n No Effect estimate columns acolSEs defined.\n Please set acolSEs.", sep=""))
	} else if(strMethod == "Aschard.ZSCORE"){
		if(length(objJT@acolZSCOREs)<2) 
			stop(paste(" EASY ERROR:JOINTTEST\n Please set at least 2 Z score columns for acolZSCOREs (e.g. --acolZSCOREs z1;z2) !!!", sep=""))
		if(any(objJT@acolZSCOREs == ""))
			stop(paste(" EASY ERROR:JOINTTEST\n No Z score columns acolZSCOREs defined.\n Please set acolZSCOREs.", sep=""))
	} else {
		## Kraft.BETA
		if(objJT@colBetaMain == "")
			stop(paste(" EASY ERROR:JOINTTEST\n No Main effect estimate column --colBetaMain defined.\n Please set colBetaMain.", sep=""))
		if(objJT@colBetaInt == "")
			stop(paste(" EASY ERROR:JOINTTEST\n No Interaction effect estimate column --colBetaInt defined.\n Please set colBetaInt.", sep=""))
		if(objJT@colSeMain == "")
			stop(paste(" EASY ERROR:JOINTTEST\n No Main effect standard error column --colSeMain defined.\n Please set colSeMain.", sep=""))
		if(objJT@colSeInt == "")
			stop(paste(" EASY ERROR:JOINTTEST\n No Interaction effect standard error column --colSeInt defined.\n Please set colSeInt.", sep=""))
		if(objJT@colCovar == "")
			stop(paste(" EASY ERROR:JOINTTEST\n No covariance column --colCovar defined.\n Please set colCovar.", sep=""))
	}
	
	objJT@strMethod <- strMethod
	
	return(objJT)
}

#############################################################################################################################
JOINTTEST.run <- function(objJT, objGWA) {

	if(objJT@strMethod == "Aschard.BETA") {
		nDf = length(objJT@acolBETAs)
		
		joint.chisq = 0 
		
		for(i in 1:nDf) {
			
			aBeta 	<- GWADATA.getcol(objGWA,objJT@acolBETAs[i])
			aSe 	<- GWADATA.getcol(objGWA,objJT@acolSEs[i])
			
			if(class(aBeta) != "numeric" & class(aBeta) != "double" ) stop(paste("EASY ERROR:JOINTTEST\nColumn \n",objJT@acolBETAs[i],"\nis not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
			if(class(aSe) != "numeric" & class(aSe) != "double" ) stop(paste("EASY ERROR:JOINTTEST\nColumn \n",objJT@acolSEs[i],"\nis not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
			
			joint.chisq = joint.chisq + ((aBeta/aSe)^2)
		}
		
		pjoint = pchisq(joint.chisq, df = nDf, lower.tail = FALSE)
		
		objGWA <- GWADATA.cbind(objGWA, pjoint, objJT@colOutPjoint)
	} else if(objJT@strMethod == "Aschard.ZSCORE") {
		nDf = length(objJT@acolZSCOREs)
		
		joint.chisq = 0 
		
		for(i in 1:nDf) {
			
			aZ 	<- GWADATA.getcol(objGWA,objJT@acolZSCOREs[i])
			
			if(class(aZ) != "numeric" & class(aZ) != "double" ) stop(paste("EASY ERROR:JOINTTEST\nColumn \n",objJT@aZ[i],"\nis not a numeric variable in file \n",objGWA@fileIn,"\n!!!" ,sep="" ))
			
			joint.chisq = joint.chisq + aZ^2
		}
		
		pjoint = pchisq(joint.chisq, df = nDf, lower.tail = FALSE)
		
		objGWA <- GWADATA.cbind(objGWA, pjoint, objJT@colOutPjoint)
		
	} else {

		## Kraft.BETA
		aBetaMain 	<- GWADATA.getcol(objGWA,objJT@colBetaMain)
		aBetaInt 	<- GWADATA.getcol(objGWA,objJT@colBetaInt)
		aSeMain 	<- GWADATA.getcol(objGWA,objJT@colSeMain)
		aSeInt 		<- GWADATA.getcol(objGWA,objJT@colSeInt)
		aCovar 		<- GWADATA.getcol(objGWA,objJT@colCovar)
		
		pjoint <- rep(NA,length(aBetaMain))
		
		for(i in 1:length(aBetaMain)) {
			chi2tmp <- t(c(aBetaMain[i],aBetaInt[i]))%*%solve(matrix(c(aSeMain[i]^2,aCovar[i],aCovar[i],aSeInt[i]^2),2,2))%*%c(aBetaMain[i],aBetaInt[i])
			pjoint[i] <- pchisq(chi2tmp,df=2,lower.tail=FALSE)
		}
		
		objGWA <- GWADATA.cbind(objGWA, pjoint, objJT@colOutPjoint)
		
	}
	
	return(objGWA)
}

JOINTTEST <- function(strEqcCommand){ 
	## Wrapper for class definition
	JOINTTESTout <- setJOINTTEST(new("JOINTTEST", strEqcCommand = strEqcCommand))
	JOINTTESTout<-validJOINTTEST(JOINTTESTout)
	return(JOINTTESTout)

}
