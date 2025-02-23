orders <- read.csv("C:/Users/offic/Documents/GitHub/CRC-Case-Competition/orders_clean.csv")

library(dplyr)
library(tidyverse) # data manipulation
library(arulesSequences) # run the sequence mining algorithm
library(stringr)

# Remove unneeded columns
orders <- orders %>% 
  select(-c(price, total, created_at, category, quantity))

head(orders)

# Prepare data: order items by user and date, and create an item ID per customer
df1 <- orders %>% 
  group_by(user_id) %>% 
  arrange(date_only) %>% 
  mutate(item_id = row_number()) %>% 
  select(user_id, date_only, item_id, product_id) %>% 
  ungroup() %>% 
  mutate(across(c("user_id", "product_id"), as.factor))

df2 <- df1[order(df1$user_id),]

# Convert to transactions
sessions <- as(df2 %>% transmute(items = product_id), "transactions")
transactionInfo(sessions)$sequenceID <- df2$user_id
transactionInfo(sessions)$eventID <- df2$item_id
itemLabels(sessions) <- str_replace_all(itemLabels(sessions), "items=", "")

inspect(head(sessions, 10))

# Run cspade ------------------------------------------------------------------
itemsets <- cspade(sessions, parameter = list(support = 0.001), control = list(verbose = FALSE))
inspect(itemsets)

# Convert results to a tibble and count the number of items in each sequence
df3 <- as(itemsets, "data.frame") %>% as_tibble()
df3$pattern <- (str_count(df3$sequence, ",") + 1)
df3 <- df3[order(-df3$support),]  # sort by support descending

write.csv(x = df3, file = "all_results.csv", row.names = FALSE)

# -------------------------------
# Calculate lift for itemsets with >= 2 products

# Compute individual product supports (relative frequency)
item_support <- itemFrequency(sessions, type = "relative")

# For each itemset, compute lift as observed support divided by the product of individual supports
df3 <- df3 %>%
  rowwise() %>%
  mutate(
    lift = if (pattern >= 2) {
      # Remove curly braces (if any) and split the sequence string by comma
      seq_clean <- gsub("[\\{\\}]", "", sequence)
      items <- unlist(strsplit(seq_clean, ","))
      items <- trimws(items)
      expected_support <- prod(item_support[as.character(items)])
      support / expected_support
    } else {
      NA_real_
    }
  ) %>%
  ungroup()

# Filter to only include itemsets with 2 or more products and sort by lift (max to min)
df_lift <- df3 %>%
  filter(pattern >= 2) %>%
  arrange(desc(lift))

# Output the top results sorted by lift
write.csv(x = df_lift, file = "top_results_by_lift.csv", row.names = FALSE)