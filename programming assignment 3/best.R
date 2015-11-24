best <- function (state, outcome) {
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	stateVector <- unique(data$State)
	state <- toupper(state)
	if(state %in% stateVector) {
		newdata <- subset(data,data$State==state)
	}
	else {
		stop("invalid state")
	}

	outcome <- strsplit(outcome," ")[[1]]
	outcome <- paste(toupper(substring(outcome,1,1)), substring(outcome,2), sep="", collapse=".")
	outcomeVector <- c("Pneumonia", "Heart.Attack", "Heart.Failure")
	if(outcome %in% outcomeVector) {
		col <- which(colnames(newdata)==paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep=""))
		v <- as.numeric(newdata[,col])
		minVal <- min(v, na.rm=TRUE)
		row <- newdata[as.numeric(newdata[,col])== minVal,]
	
		return(na.omit(row$Hospital.Name)[1])
	} 
	else { 
		stop("invalid outcome")
	}

}
