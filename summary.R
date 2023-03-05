#What is the total number of checkouts for each Material Type? 
sum_material_type <- library_df %>%
  group_by(MaterialType) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'keep')
View(sum_material_type)

material_table <- sum_material_type %>% 
  select(MaterialType, mean, sum) %>%
  arrange(-mean)
View(material_table)

#What is the month or year with the most/least checkouts for e-books?
# Most ebook checkouts
most_ebook_checkouts <- library_df %>% 
  filter(MaterialType == 'EBOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)

View(most_ebook_checkouts)
ebook_max <- most_ebook_checkouts[[1, "CheckoutYear"]]
View(ebook_max)

# least ebook checkouts
least_ebook_checkouts <- library_df %>% 
  filter(MaterialType == 'EBOOK') %>%
  group_by(CheckoutYear) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(sum)

View(least_ebook_checkouts)
ebook_min <- least_ebook_checkouts[[1, "CheckoutYear"]]
View(ebook_min)

#How has the number of print book checkouts changed over time?

#Books over time by year
print_books_over_time_year <- library_df %>% 
  group_by(CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)
View(print_books_over_time_year)

#Books over time by month
print_books_over_time_month <- library_df %>% 
  group_by(CheckoutMonth, CheckoutYear, MaterialType) %>% 
  filter(MaterialType == 'BOOK') %>%
  summarize(sum = sum(Checkouts, na.rm = TRUE), .groups = 'drop') %>%
  arrange(-sum)
View(print_books_over_time_month)

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
View(most_book_checkouts)
