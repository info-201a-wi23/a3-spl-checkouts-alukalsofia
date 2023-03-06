
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
# Print books summarization and summation
book_six_years <- library_df %>% 
  group_by(CheckoutMonth, CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(book_sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CheckoutYear)
#View(book_six_years)

#Print books plot over time
ggplot(data = book_six_years) +
  geom_point(aes(x = CheckoutMonth, y = book_sum, color = factor(CheckoutYear))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparing the Number of BOOK Checkouts From 2017-2023", 
       x = "Date (months)", 
       y = "Number of Checkouts", 
       color = "Checkout Year") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(0, 1450000, 10000))
