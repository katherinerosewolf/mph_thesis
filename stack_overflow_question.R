weird_data <- 
  data.frame("ID" = 1:8, 
             "API" = c("01-01", 
                       "01-02", 
                       "02-01", 
                       "02-02", 
                       "02-03", 
                       "03-01", 
                       "03-02", 
                       "03-03"),  
             "Final" = c("no", 
                          "yes", 
                          "no",
                          "no", 
                          "yes", 
                          "no", 
                          "no",
                          "yes"), 
             "Month" = c("May", 
                         NA, 
                         NA, 
                         "June", 
                         "July", 
                         "April", 
                         "June",
                         NA), 
             stringsAsFactors = FALSE
  )


desired_output <- 
  data.frame("ID" = c(2, 5, 8), 
             "API" = c("01-02", 
                       "02-03", 
                       "03-03"), 
             "Month" = c("May", 
                         "July", 
                         "June"), 
             stringsAsFactors = FALSE)

desired_output <- 
  data.frame("ID" = 1:8, 
             "API" = c("01-01", 
                       "01-02", 
                       "02-01", 
                       "02-02", 
                       "02-03", 
                       "03-01", 
                       "03-02", 
                       "03-03"),  
             "Final" = c("no", 
                         "yes", 
                         "no",
                         "no", 
                         "yes", 
                         "no", 
                         "no",
                         "yes"), 
             "Month" = c("May", 
                         "May", 
                         NA, 
                         "June", 
                         "July", 
                         "April", 
                         "June",
                         "June"), 
             stringsAsFactors = FALSE
  )
  

View(desired_output)