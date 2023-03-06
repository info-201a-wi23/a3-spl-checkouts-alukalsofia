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

# material types data processing
material_types <- library_df %>% 
  group_by( date, MaterialType, CheckoutMonth, CheckoutYear) %>% 
  filter(MaterialType %in% c("EBOOK", "BOOK", "MAGAZINE",
                             "VIDEODISC", "SOUNDDISK", "AUDIOBOOK",
                             "MOVIE", "MIXED", "SONG")) %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = "drop")
#View(material_types)
#View(library_df)

# Material types plot
ggplot(data = material_types) +
  geom_line(aes(x = date, y = sum, color = MaterialType)) +
  labs(title = "Comparing Different Material Types Over 6 Years", 
       x = "Date (months)", 
       y = "Number of Checkouts",
       color = "Material Type") + 
  scale_x_date(date_breaks="6 month", date_labels="%B-%Y") + 
  theme(plot.title = element_text(hjust = 0.5) , 
        axis.text.x = element_text(angle=40, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 145000, 9000))
