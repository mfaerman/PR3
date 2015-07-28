test_best <- function () {
        source("best.R")

        test <- c(best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER")
        test <- c(test, best("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER")
        test <- c(test, best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE")
        test <- c(test, best("MD", "pneumonia") == "GREATER BALTIMORE MEDICAL CENTER")
        
        try(best("BB", "heart attack"))
        test <- c(test, "Error in best(\"BB\", \"heart attack\") : invalid state\n" == geterrmessage())
        try(best("NY", "hert attack"))
        test <- c(test, "Error in best(\"NY\", \"hert attack\") : invalid outcome\n" == geterrmessage())
        
        print(test)
        all(test)
}