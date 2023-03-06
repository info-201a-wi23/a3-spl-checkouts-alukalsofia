
#Loading Tidyverse and Readr
library(tidyverse)
library(readr)
library(scales)
library(knitr)
library(stringr)
library(plotly)
library(ggplot2)

# Reading the Checkouts CSV into 'library_df'
library_df <- read.csv("/Users/sofiaalukal/Desktop/INFO_201/Programming-Assignments/a3-spl-checkouts-alukalsofia/2017-2023-10-Checkouts-SPL-Data.csv",
                       stringsAsFactors = FALSE)
library_df <- library_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

library_df$date <- as.Date(library_df$date, format = "%Y-%m-%d")

library_df$Title <- tolower(library_df$Title)

library_df$Subjects <- tolower(library_df$Subjects)

#View(library_df)
# Summarizing the material types sums
sum_material_type <- library_df %>%
  group_by(MaterialType) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'keep') %>%
  arrange(-sum) %>%
  head(sort(sum_material_type$sum, decreasing = TRUE), n = 10)
#View(sum_material_type)

#Making a pie chart
ggplot(sum_material_type, aes(x = "", y = sum, fill = MaterialType)) + 
  geom_bar(stat = "identity", width = 0.3, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "The 10 most rented Material types from 2017 to 2023")
