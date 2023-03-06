
#Loading Tidyverse and Readr
library(tidyverse)
library(readr)
library(scales)
library(knitr)
library(stringr)
library(plotly)
library(ggplot2)

# Reading the Checkouts CSV into 'library_df'
library_df <- read.csv("Desktop/INFO_201/Programming-Assignments/a3-spl-checkouts-alukalsofia/2017-2023-10-Checkouts-SPL-Data.csv",
                       stringsAsFactors = FALSE)
library_df <- library_df %>% 
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))

library_df$date <- as.Date(library_df$date, format = "%Y-%m-%d")

library_df$Title <- tolower(library_df$Title)

library_df$Subjects <- tolower(library_df$Subjects)

#View(library_df)

#Selecting columns of data to process the dataframe in different ways, eg: by year, month, and book details
year_df <- library_df %>% 
  select("Checkouts", "CheckoutYear",
         "UsageClass", "MaterialType")
month_df <- library_df %>% 
  select("CheckoutMonth", "CheckoutYear", "Checkouts",
         "MaterialType", "CheckoutType", "UsageClass")
book_df <- library_df %>% 
  select("Title", "Creator", "UsageClass", "MaterialType", 
         "Checkouts", "CheckoutType", "CheckoutYear", "CheckoutMonth")

#What is the total number of checkouts for each Material Type? 

sum_material_type <- library_df %>%
  group_by(MaterialType) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'keep')
#View(sum_material_type)

material_table <- sum_material_type %>% 
  select(MaterialType, sum) %>%
  arrange(-sum())
#View(material_table)

#What is the average number of checkouts for each Checkout Type? 
mean_checkout_type <- library_df %>%
  group_by(CheckoutType) %>% 
  summarize(mean = mean(Checkouts, na.rm = TRUE))

checkout_table <- mean_checkout_type %>% 
  select(CheckoutType, mean) %>%
  arrange(-mean)
#View(checkout_table)

#What is the month or year with the most/least checkouts for e-books?
# Most ebook checkouts
most_ebook_checkouts <- library_df %>% 
  filter(MaterialType == 'EBOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)

#View(most_ebook_checkouts)
ebook_max <- most_ebook_checkouts[[1, "CheckoutYear"]]
#View(ebook_max)

# least ebook checkouts
least_ebook_checkouts <- library_df %>% 
  filter(MaterialType == 'EBOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(sum)

#View(least_ebook_checkouts)
ebook_min <- least_ebook_checkouts[[2, "CheckoutYear"]]
#View(ebook_min)

#How has the number of print book checkouts changed over time?
#Books over time by year
print_books_over_time_year <- library_df %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)
#View(print_books_over_time_year)

#Books over time by month
print_books_over_time_month <- library_df %>% 
  group_by(CheckoutMonth, CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)
#View(print_books_over_time_month)

#Least and most checkouts for print books
least_book_checkouts <- library_df %>% 
  filter(MaterialType == 'BOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(sum)

most_book_checkouts <- library_df %>% 
  filter(MaterialType == 'BOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)
#View(most_book_checkouts)

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
  group_by(date, Title, CheckoutMonth, CheckoutYear) %>% 
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
  scale_x_date(date_breaks = "6 month", date_labels = "%B-%Y") + 
  theme(plot.title = element_text(hjust = 0.5) , 
        axis.text.x = element_text(angle=40, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 1500, 100))


# All material types over 6 years
material_types <- library_df %>% 
  group_by( date, MaterialType, CheckoutMonth, CheckoutYear) %>% 
  filter(MaterialType %in% c("EBOOK", "BOOK", "MAGAZINE",
                             "VIDEODISC", "SOUNDDISK", "AUDIOBOOK",
                             "MOVIE", "MIXED", "SONG")) %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = "drop")
#View(material_types)
#View(library_df)

material_plot <- ggplot(data = material_types) +
  geom_line(aes(x = date, y = sum, color = MaterialType)) +
  labs(title = "Comparing Different Material Types Over 6 Years", 
       x = "Date (months)", 
       y = "Number of Checkouts",
       color = "Material Type") + 
  scale_x_date(date_breaks="6 month", date_labels="%B-%Y") + 
  theme(plot.title = element_text(hjust = 0.5) , 
        axis.text.x = element_text(angle=40, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 145000, 9000))

# top ten material types
sum_material_type <- library_df %>%
  group_by(MaterialType) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'keep') %>%
  arrange(-sum) %>%
  head(sort(sum_material_type$sum, decreasing = TRUE), n = 10)
#View(sum_material_type)

top_ten_plot<- ggplot(sum_material_type, aes(x = "", y = sum, fill = MaterialType)) + 
  geom_bar(stat = "identity", width = 0.3, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "The 10 most rented Material types from 2017 to 2023")

#Each material type over 6 years
#EBOOKS
ebook_six_years <- library_df %>% 
  group_by(CheckoutMonth, CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'EBOOK') %>%
  summarize(ebook_sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop')
#View(ebook_six_years)

ebook_plot <- ggplot(data = ebook_six_years) +
  geom_point(aes(x = CheckoutMonth, y = ebook_sum, color = factor(CheckoutYear))) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Comparing the Number of EBOOK Checkouts From 2017-2023", 
       x = "Date (months)", 
       y = "Number of Checkouts", 
       color = "Checkout Year") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(30000, 105000, 5000))

#Books
book_six_years <- library_df %>% 
  group_by(CheckoutMonth, CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(book_sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(CheckoutYear)
#View(book_six_years)

book_plot <- ggplot(data = book_six_years) +
  geom_point(aes(x = CheckoutMonth, y = book_sum, color = factor(CheckoutYear))) +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Comparing the Number of BOOK Checkouts From 2017-2023", 
       x = "Date (months)", 
       y = "Number of Checkouts", 
       color = "Checkout Year") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(breaks = seq(0, 1450000, 10000))
  