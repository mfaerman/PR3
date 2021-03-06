best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcome_types <- c("heart attack", "heart failure", "pneumonia")
        
        ## Hospital.Name column
        cnm <- 2
        
        ## columns in input data containing outcome rates
        cor <- c (11, 17, 23)
        
        ## create mapping from outcome type to column number
        names(cor) <- outcome_types
        
        ## Check that state and outcome are valid
        if(!(state %in% state.abb)) 
                stop ("invalid state")
        
        if(!(outcome %in% outcome_types))
                stop ("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        data_split_by_state <- split(data, data$State)
        
        ## Work with data from selected state
        data_selected_state <- data_split_by_state[[state]]
        
        ## Retrieve dataset column containing selected outcome type
        cso <- cor[outcome]
        
        ## Convert outcome data from character vector to numeric
        data_selected_state[ , cso] <- as.numeric(data_selected_state[ , cso])
        
        ## Save only columns of interest
        dat <- data_selected_state[, c(cnm, cso)]
        names(dat) <- c("Name", "Rate")

        min(dat$Name[dat$Rate == min(dat$Rate, na.rm = TRUE)], na.rm = TRUE)
}