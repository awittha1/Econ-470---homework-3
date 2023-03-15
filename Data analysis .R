library(tidyverse)
library(fixest)
library(AER)
taxburden <- read_rds("data/output/TaxBurden_Data.rds")


##########################Question 1#######################


taxburden <- taxburden %>% mutate(price_cpi_2012 = cost_per_pack*(229.5939/index), 
                                  total_tax_cpi = tax_dollar * (229.5939/index))

# Get a vector of unique states
states <- unique(taxburden$state) 

states_list <- list()
# Loop over each state and calculate the tax change
for (i in states) {
  # Subset the data for the current state
  state_data <- filter(taxburden, state == i)
  # Calculate the tax change for the current state
  state_data2 <- state_data %>% mutate(tax_change = c(0,ifelse(diff(tax_state) != 0, 1, 0)))
  states_list[[i]] <- state_data2
}

taxburden2 <- do.call(rbind, states_list)

table_1 <- taxburden2%>% filter(Year %in% c(1970:1985)) %>% 
  group_by(Year)%>% 
  summarize(prop = sum(tax_change == 1)/n())

graph_1 <- ggplot(table_1, aes(Year, prop))+
  geom_bar(stat = 'identity', fill="#FF9999", colour="black" )+
  labs(title = "Proportion of States with a change in their cigarette tax", x = "Year", y = "Proportion of states")+
  ylim(0,1)+
  theme_bw()
graph_1

##########################Question 2#######################


table2 <- taxburden %>% filter(Year %in% c(1970:2018)) %>% 
         group_by(Year)%>%
          summarize(avg_price = mean(cost_per_pack), avg_tax = mean(tax_dollar))


#Add legend if possible 
graph_2 <- ggplot(table2)+
  geom_line(aes(Year, avg_price),color = "#FF9999")+
  geom_line(aes(Year, avg_tax), color = "black")+
  labs(title = "Average tax and Average Price of Cigaretts", x = "Year",y = "")+
  annotate("text", x = 2016, y = 7.2, label = "Average Price ", color = "#FF9999", size = 3)+
  annotate("text", x = 2016, y = 3.2, label = "Average Tax ", color = "black", size = 3)+
  theme_bw()
graph_2

##########################Question 3#######################

cig_data_diff <- taxburden %>%
  group_by(state) %>%
  summarise(price_diff = price_cpi_2012 [Year == mean(2018)] - price_cpi_2012 [Year == mean(1970)]) %>%
  arrange(desc(price_diff))

# Select the top 5 states with the highest increase in cigarette prices
top_5 <- cig_data_diff %>%
  top_n(5)


#use top_n 
bottom_5 <- cig_data_diff %>%
             top_n(-5)


table3<- taxburden %>% filter (state %in% top_5$state & Year %in% c(1970:2018))%>% 
  group_by(Year, state)%>% 
  summarize(avg_sales = mean(sales_per_capita))

graph3 <- ggplot(table3, aes(Year, avg_sales))+
  geom_line(aes(color = state))+
  labs(title = "Average sales per capita", x = "Year",y = "Average sales per capita")+
  theme_bw()
  
graph3

##########################Question 4#######################

table4<- taxburden %>% filter (state %in% bottom_5$state & Year %in% c(1970:2018))%>% 
  group_by(Year, state)%>% 
  summarize(avg_sales = mean(sales_per_capita))

graph4 <- ggplot(table4, aes(Year, avg_sales))+
  geom_line(aes(color = state))+
  labs(title = "Average sales per capita", x = "Year",y = "Average sales per capita")+
  theme_bw()
graph4

##########################Question 5#######################
top_states <- taxburden %>% filter (state %in% top_5$state & Year %in% c(1970:2018)) %>% mutate(avg_sales2= "Highest price increase")
bottom_states <-  taxburden %>% filter (state %in% bottom_5$state & Year %in% c(1970:2018)) %>% mutate(avg_sales2 = "Lowest price increase")

mini_df <- rbind(top_states, bottom_states)

graph5 <- mini_df %>% group_by(Year, avg_sales2)%>% summarize(avg_sales = mean(sales_per_capita))%>%
 ggplot(aes(Year, avg_sales))+
  geom_line(aes(color = avg_sales2))+
  labs(title = "Average sales for States with Biggest and Lowest Tax Increase", x = "Year",y = "Average sales per capita", color = "")+
  annotate("text", x = 2016, y = 30, label = "Top 5 States", color = "black", size = 3)+
  annotate("text", x = 2016, y = 75, label = "Bottom 5 States", color = "black", size = 3)+
  theme_bw()+
  guides(color = FALSE)
graph5

##########################Question 6#######################

filtered_data1 <- taxburden %>% filter(Year %in% c(1970:1990))

filtered_data1$log_sales <- log(filtered_data1$sales_per_capita)
filtered_data1$log_price <- log(filtered_data1$cost_per_pack)

regq6<- feols(log_price~ log_sales, data = filtered_data1)

##########################Question 7#######################

ivq7 <- feols(log_sales ~ 1 | log_price ~ tax_dollar, data = filtered_data1)
summary(ivq7)

##########################Question 7#######################
first_stage1 <- feols(log_price ~ tax_dollar, data = filtered_data1)
reduced_form1 <- feols(log_sales ~ tax_dollar, data = filtered_data1)


##########################Question 8#######################

filtered_data2 <- taxburden %>% filter(Year %in% c(1991:2015))

filtered_data2$log_sales <- log(filtered_data2$sales_per_capita)
filtered_data2$log_price <- log(filtered_data2$cost_per_pack)

regq8<- feols(log_price~ log_sales, data = filtered_data2)
ivq8 <- feols(log_sales ~ 1 | log_price ~ tax_dollar, data = filtered_data2)

first_stage2 <- feols(log_price ~ tax_dollar, data = filtered_data2)
reduced_form2 <- feols(log_sales ~ tax_dollar, data = filtered_data2)

save.image("image.Rdata")




