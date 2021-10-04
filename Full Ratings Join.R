week <- CFBWeek()
ratings <- data.frame()
for(i in 1:week){
  df <- read.csv(glue("Archived Ratings/Composite/CompositeWeek{i}Ratings.csv")) %>% select(-pace) %>%
    mutate(week=i)
  ratings <- bind_rows(ratings, df)
}
write.csv(ratings, "Archived Ratings/Composite/CompositeFullRatings.csv", row.names = F)
