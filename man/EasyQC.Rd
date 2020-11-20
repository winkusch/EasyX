\name{EasyQC}
\alias{EasyQC}
\title{
Function to run EasyQC (g-v1.0.1).
}
\description{
The Function takes the an EasyQC config/script (ECF-) file and performs all steps defined in the ECF-file.
}
\usage{
EasyQC(fileECF,blnValidityCheckOnly,blnReturnGwadata,blnReturnReport,aFileIn)
}
\arguments{
\item{fileECF}{
fileECF is an EasyQC config/script (ECF) file.\cr
Please see 'EasyQC_9.0_Commands_140918.pdf' for more information on ECF functions and parameters.
}
\item{blnValidityCheckOnly}{
blnValidityCheckOnly is a logical value that indicates whether the full pipeline (blnValidityCheckOnly=FALSE, default) or only the validity check (blnValidityCheckOnly=TRUE) are supposed to be run.
}
\item{blnReturnGwadata}{
blnReturnGwadata is a logical value that indicates whether the most recent GWADATA object should be returned (default blnReturnGwadata=FALSE).
}
\item{blnReturnReport}{
blnReturnReport is a logical value that indicates whether the REPORT object should be returned (default blnReturnReport=FALSE).
}
\item{aFileIn}{
aFileIn is an array that pases paths to input files to the ecf (aFileIn=c(), default).
}
}
\references{
http://www.genepi-regensburg.de/easyqc
}
\examples{
\donttest{installDir=system.file("extdata", package="EasyQC")}
# Example qc routine:
\donttest{ecfFileQc=paste(installDir,"example_qc.ecf",sep="/")}
\donttest{EasyQC(ecfFileQc)}
}
\author{
Thomas Winkler <thomas.winkler@klinik.uni-regensburg.de>
}