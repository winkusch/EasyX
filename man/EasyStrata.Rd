\name{EasyStrata}
\alias{EasyStrata}
\title{
Function to run EasyStrata (g-v1.0.1).
}
\description{
The Function takes the an EasyStrata config/script (ECF-) file and performs all steps defined in the ECF-file.
}
\usage{
EasyStrata(fileECF,blnValidityCheckOnly,blnReturnGwadata,blnReturnReport,aFileIn)
}
\arguments{
\item{fileECF}{
fileECF is an EasyStrata config/script (ECF) file.\cr
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
http://www.genepi-regensburg.de/easystrata
}
\examples{
\donttest{installDir=system.file("extdata", package="EasyStrata")}
## Example Pipeline:
\donttest{ecfPipe=paste(extDataDir,"example_pipeline.ecf",sep="/")}
\donttest{EasyStrata(ecfPipe)}
}
\author{
Thomas Winkler <thomas.winkler@klinik.uni-regensburg.de>
}