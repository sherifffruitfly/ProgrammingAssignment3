## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank for 30-day death rate
rankhospital <- function(state, outcome, num = "best")
{
  # valid abbreviations obtained from https://www.infoplease.com/state-abbreviations-and-state-postal-codes
  valid_abbrevs <- c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "GU", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MH", "MA", "MI", "FM", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "MP", "OH", "OK", "OR", "PW", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "VI", "WA", "WV", "WI", "WY")
  
  if (!is.element(state, valid_abbrevs))
  {
    stop("invalid state")
  }
  
  # design decision: I allow outcomes to be uppercase since that's how the csv has column names
  if (!is.element(tolower(outcome), c("heart attack", "heart failure", "pneumonia")))
  {
    stop("invalid outcome")
  }
  
  # decide what col # we'll be aggregating
  agg_col <- switch(tolower(outcome)
                    , "heart attack" = 3
                    , "heart failure" = 4
                    , "pneumonia" = 5)
  
  
  # can write extract to file with:
  # write.csv(testdata, file = "C:\\cdjProgramming\\coursera\\r\\week4\\testdata.csv", row.names=FALSE)
  #
  # columns
  # $ Hospital.Name                                             : chr - col 2
  # $ State                                                     : chr - col 4
  # $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack : num - col 11
  # $ Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure: num - col 17
  # $ Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia    : num - col 23
  
  myfile <- "outcome-of-care-measures.csv"
  mydir <- "C:\\cdjProgramming\\coursera\\r\\week4"
  mycols <- c("NULL"
              , NA 			#hospital name (2)
              , rep("NULL", 4)
              , NA 			#state (7)
              , rep("NULL", 3)
              , NA			#heart attack (11)
              , rep("NULL", 5)
              , NA			#heart failure (17)
              , rep("NULL", 5)
              , NA			#pneumonia (23)
              , rep("NULL", 23))
  setwd(mydir)
  data <- read.csv(myfile
                   , header = TRUE
                   , colClasses = mycols
                   , na.strings=c("", ".", "NA", "Not Available")
                   , stringsAsFactors = FALSE
  )
  colnames(data) <- c("Hospital", "State", "Heart Attack", "Heart Failure", "Pneumonia")
  
  # filter state
  data <- subset(data, data$State == state)
  
  # filter NA
  data <- subset(data, !is.na(data[,agg_col]))
  
  # sort
  data <- data[order(data[, agg_col], data$Hospital),]
  
  # return the proper row
  if (length(num) > 0)
  {
    switch (class(num[1]),
            "character" = 
              {
                switch(num[1],
                  "best" = myrow <- 1,
                  "worst" = myrow <- nrow(data),
                  myrow <- NA
                )
              },
            "numeric" = 
              {
                if (num%%1 == 0 & num > 0 & num <= nrow(data))
                {
                  myrow <- num
                } else
                {
                  myrow <- NA
                }
              },
            NA
    )
  } else
  {
    NA
  }
  
  unique(data$Hospital[myrow])
}



