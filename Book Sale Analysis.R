
library(tidyverse)
reviews <- book_reviews
dim(reviews)
columns <- colnames(reviews)

#learning about types of data in each column
for (c in colnames(reviews)) { print(c)
  print(typeof(reviews[[c]]))}

#learning about unique data 
for (c in colnames(reviews)) {
  print("Unique values in the column:")
  print(c)
  print(unique(reviews[[c]]))
  print("")
}

#filtered out data where reviews were blank
no_na <- reviews %>% filter(!is.na(review))

#changing state names to zip postal code names
no_na <- no_na %>% mutate(state = case_when(state == "California" ~ "CA", 
                                            state == "New York" ~ "NY",
                                            state == "Texas" ~ "TX",
                                            state == "Florida" ~ "FL",
                                            TRUE ~ state))
#creating a new column for review_num then adding a column on whether the review was high
no_na <-no_na %>% mutate(
  review_num = case_when(
    review == "Poor" ~ 1,
    review == "Fair" ~ 2,
    review == "Good" ~ 3,
    review == "Great" ~ 4,
    review == "Excellent" ~ 5
    ),
  is_high_review = if_else(review_num >= 4, TRUE, FALSE)
  )

#arranged books by most purchased. 

book_names <- no_na %>% count(book) %>% arrange(-n)



