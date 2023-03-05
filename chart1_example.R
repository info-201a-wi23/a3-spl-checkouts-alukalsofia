#Loading Tidyverse and Readr
library(tidyverse)
library(readr)
library(scales)
library(knitr)
library(stringr)
library(plotly)

# Reading the Checkouts CSV into 'library_df'
library_df <- read.csv("Desktop/INFO/a3-spl-checkouts-alukalsofia/2017-2023-10-Checkouts-SPL-Data.csv",
                       stringsAsFactors = FALSE)
library_df <- library_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

library_df$date <- as.Date(library_df$date, format = "%Y-%m-%d")

library_df$Title <- tolower(library_df$Title)

library_df$Subjects <- tolower(library_df$Subjects)

#View(library_df)

#What is the month or year with the most/least 
#checkouts for a book that you're interested in?
# Year with the most checkouts for A court of mist and fury
library_df$Title[str_detect(library_df$Title, "a court of thorns")] <- "A Court of Thorns and Roses"
library_df$Title[str_detect(library_df$Title, "a court of mist")] <- "A Court of Mist and Fury"
library_df$Title[str_detect(library_df$Title, "a court of wings")] <- "A Court of Wings and Ruin"
library_df$Title[str_detect(library_df$Title, "a court of frost")] <- "A Court of Frost and Starlight"
library_df$Title[str_detect(library_df$Title, "a court of silver")] <- "A Court of Silver Flames"

acomaf_book_year <- library_df %>% 
  group_by(Title, CheckoutYear) %>%
  filter(Title == "A Court of Mist and Fury") %>% 
  summarize(year_sum = sum(Checkouts, na.rm = TRUE), .groups = "keep") #%>%
#View(acomaf_book_year)

#Month with the most checkouts for a court of mist and fury
acomaf_book_month <- library_df %>% 
  group_by(Title, date) %>%
  filter(Title == "A Court of Mist and Fury") %>% 
  summarize(month_sum = sum(Checkouts, na.rm = TRUE), .groups = "keep") %>% 
  arrange (-month_sum)
#View(acomaf_book_month)

#ACOMAF Year and Month s with the most checkouts and the specific number of checkouts
acomaf_year <- acomaf_book_year[[1, "CheckoutYear"]]
acomaf_checkouts_year <- acomaf_book_year[[1, "year_sum"]]
acomaf_month <- acomaf_book_year[[1, "CheckoutMonth"]]
acomaf_checkouts_month <- acomaf_book_year[[1, "month_sum"]]

# a court of thorns and roses series sum checkouts


acotar_series <- library_df %>% 
  group_by( date, Title, CheckoutMonth, CheckoutYear) %>% 
  filter(Title %in% c("A Court of Thorns and Roses", 
                      "A Court of Mist and Fury", "A Court of Wings and Ruin",
                      "A Court of Frost and Starlight", "A Court of Silver Flames")) %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = "drop")
#View(acotar_series)
#View(library_df)

#Potential plot for acotar series
acotar_plot <- ggplot(data = acotar_series) +
  geom_point(aes(x = date, y = sum, color = Title)) +
  labs(title = "Comparing the 'A Court of Thorns and Roses Series'", 
       x = "Date (months)", 
       y = "Number of Checkouts",
       color = "Book Title") + 
  scale_x_date(date_breaks = "4 month", date_labels = "%B") + 
  theme(plot.title = element_text(hjust = 0.5) , 
        axis.text.x = element_text(angle=40, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 1500, 100))
