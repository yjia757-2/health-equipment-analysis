library(dplyr)

data <- read.csv("Cleaned_Data.csv", stringsAsFactors = FALSE) %>% select(-Item)
data[is.na(data)] <- 0

final_data <- data %>%  group_by(Imaging.Equipment) %>% summarise(Year.2019 = sum(Year.2019), 
                                                                  Year.2020 = sum(Year.2020), 
                                                                  Year.2021 = sum(Year.2021), 
                                                                  Year.2022 = sum(Year.2022),
                                                                  Year.2023 = sum(Year.2023),
                                                                  Year.2024 = sum(Year.2024),
                                                                  Beyond.5.years = sum(Beyond.5.years)
                                                                  )
write.csv(final_data, "Count_Helper.csv")
