library(ggplot2)
library(dplyr)

filepath1 <- "D:\\Tuts\\DataScience\\Datasets\\brazilian_ecommerce_olist\\olist_customers_dataset.csv"
filepath2 <- "D:\\Tuts\\DataScience\\Datasets\\brazilian_ecommerce_olist\\olist_orders_dataset.csv"
filepath3 <- "D:\\Tuts\\DataScience\\Datasets\\brazilian_ecommerce_olist\\olist_order_payments_dataset.csv"
filepath4 <- "D:\\Tuts\\DataScience\\Datasets\\brazilian_ecommerce_olist\\olist_order_items_dataset.csv"
filepath5 <- "D:\\Tuts\\DataScience\\Datasets\\brazilian_ecommerce_olist\\olist_products_dataset.csv"

customers_df <- read.csv(filepath1)
orders_df <- read.csv(filepath2)
payments_df <- read.csv(filepath3)
order_items_df <- read.csv(filepath4)
products_df <- read.csv(filepath5)

products_df <- select(products_df, c(1,2))

#Inner join / only those customers who placed an order
olist_df <- merge(customers_df, orders_df, by = "customer_id") %>% 
            select(1:8) %>%
            merge(payments_df, by = "order_id") %>% 
            merge(order_items_df, by = "order_id") %>%
            select(c(1,2,3,5,6,7,8,10,11,12,13,14,15)) %>%
            merge(products_df, by = "product_id") 


 

            
olist_df$customer_id <- as.character(olist_df$customer_id)

freq_customers <- data.frame(table(olist_df$customer_id))




   


#olist_df[rand_cust]














