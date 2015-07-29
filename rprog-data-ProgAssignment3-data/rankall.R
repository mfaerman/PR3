hrank <- function (dat, num) {
        if (is.numeric(num))
                hospital <- dat$Name[order(dat$Rate, dat$Name)[num]]
        else 
                hospital <- switch(num,
                                   best  = dat$Name[order(dat$Rate, dat$Name)[1]],
                                   worst = dat$Name[order(dat$Rate, dat$Name)[nrow(dat)]])
        
        hospital      
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcome_types <- c("heart attack", "heart failure", "pneumonia")
        
        ## Hospital.Name column
        cname <- 2
        
        ## State column
        cst <- 7
        
        ## columns in input data containing outcome rates
        cor <- c (11, 17, 23)
        
        ## create mapping (dictionary) from outcome type to column number
        names(cor) <- outcome_types
        
        ## Check that outcome is valid
        if(!(outcome %in% outcome_types))
                stop ("invalid outcome")
        
        ## Retrieve dataset column containing selected outcome type
        cso <- cor[outcome]
        
        ## Save only columns of interest
        dat <- data[ , c(cname, cst, cso)]
        names(dat) <- c("Name", "State", "Rate")
        
        ## Convert outcome data from character vector to numeric
        dat$Rate <- as.numeric(dat$Rate)
        
        ## Remove rows with NA outcomes
        dat <- dat[!is.na(dat$Rate), ]
        
        ## For each state, find the hospital of the given rank
        dst <- split(dat, dat$State)
        
        results <- sapply(dst, rank, num)
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        data.frame(hospital = results, state = names(results))
}