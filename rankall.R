## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
rankall <- function(outcome, num = "best")
{
  # validate num
  myrank <- NA
  if (length(num) > 0)
  {
    switch (class(num),
            "character" = 
            {
              switch(num[1],
                     "best" = myrank <- 1
                     , "worst" = myrank <- 0 # this is a flag for a later computation
                     , myrank <- NA
              )
            },
            "numeric" = 
            {
              if (num[1]%%1 == 0 & num[1] > 0)
              {
                myrank <- num[1]
              } else
              {
                myrank <- NA
              }
            },
            "integer" = 
            {
              if (num[1]%%1 == 0 & num[1] > 0)
              {
                myrank <- num[1]
              } else
              {
                myrank <- NA
              }
            },
            "NULL" = 
            {
              myrank <- NA
            },
            {
              myrank <- NA
            }
    )
  } else
  {
    myrank <- NA
  }
  
  
  # validate outcome
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
  
  
  # read data
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
  
  # filter NA
  data <- subset(data, !is.na(data[,agg_col])) # this example is for heart attack 
  
  # sort
  data <- data[order(data$State, data[,agg_col], data$Hospital),]
  
  # transform
  #split_data <- split(c(data[, 2], data[,1], data[,agg_col]), data[,2])
  split_data <- split(c(data$State, data$Hospital, data[,agg_col]), data$State)
  split_matrices <- lapply(split_data, function(x) matrix(x, ncol = 3))
  
  # then take the nth row hospital row from each - or NA if there is no nth row
  nth_best_by_state <- sapply(split_matrices
                              , function(x) 
                              {
                                if (!is.na(myrank) & nrow(x) >= myrank)
                                {
                                  if (myrank==0)
                                  {
                                    c(x[nrow(x), 2], x[nrow(x), 1]) 	# this is the last (worst) one
                                  } else
                                  {
                                    c(x[myrank, 2], x[myrank, 1]) 		# this is a row number
                                  }
                                } else
                                {
                                  c(NA, x[1, 1])
                                }
                              }
  )
  
  # make display match spec
  nth_best_by_state <- as.data.frame(t(nth_best_by_state))
  colnames(nth_best_by_state) <- c("hospital", "state")
  nth_best_by_state
}

