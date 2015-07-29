rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcome_types <- c("heart attack", "heart failure", "pneumonia")
        
        ## columns in input data containing outcome rates
        cn <- c (11, 17, 23)
        
        ## create mapping from outcome type to column number
        names(cn) <- outcome_types
        
        ## Check that state and outcome are valid
        if(!(state %in% state.abb)) 
                stop ("invalid state")
        
        if(!(outcome %in% outcome_types))
                stop ("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}

rankhospital <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        outcome_types <- c("heart attack", "heart failure", "pneumonia")
        
        ## columns in input data containing outcome rates
        cn <- c (11, 17, 23)
        
        ## create mapping from outcome type to column number
        names(cn) <- outcome_types
        
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
        cl <- cn[outcome]
        
        ## Convert outcome data from character vector to numeric
        data_selected_state[ , cl] <- as.numeric(data_selected_state[ , cl])
        
        ## Save only dataset rows where outcome values are not NA
        clean_outcomes <- data_selected_state[!is.na(data_selected_state[ , cl]), ]
        
        best_hospital_outcome <- min(clean_outcomes[, cl])  
        
        list_best_hospitals <- clean_outcomes$Hospital.Name[clean_outcomes[, cl] == best_hospital_outcome]
        
        min(list_best_hospitals)
}