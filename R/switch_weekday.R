switch_weekday <- function(weekday){
  switch(weekday,
         "sunday" = 1,
         "monday" = 2,
         "tuesday" = 3,
         "wednesday" = 4,
         "thursday" = 5,
         "friday" = 6,
         "saturday" = 7,
         stop("Choose a weekday.")
  )
}
