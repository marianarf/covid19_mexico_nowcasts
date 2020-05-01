install.packages("rio")

library(rio)
table<- import("summary_table.rds")

table <- table %>%
  dplyr::select(-c(`Doubling time (days)`))

write.csv(table,"/cloud/project/summary.csv", row.names = FALSE)