# Load necessary libraries
library(plotly)
library(readxl)
library(dplyr)

# Load the dataset
Adidas_data <- read_excel("C:/Users/Zenbook/Desktop/New folder/Abhishek Paudel-23140737/Adidas US Sales Datasets.xlsx")
# Data Cleaning and Preparation
Adidas_data$Year <- format(as.Date(Adidas_data$`Invoice Date`), "%Y")
Adidas_data$`Invoice Date` <- as.Date(Adidas_data$`Invoice Date`)
Adidas_data <- Adidas_data %>% filter(`Total Sales` >= 0)
Adidas_data$Region <- as.factor(Adidas_data$Region)
Adidas_data$`Sales Method` <- as.factor(Adidas_data$`Sales Method`)
Adidas_data$Product <- as.factor(Adidas_data$Product)
Adidas_data$Retailer <- as.factor(Adidas_data$Retailer)
Adidas_data$`Total Sales` <- as.numeric(gsub("[^0-9.-]", "", 
                                             Adidas_data$`Total Sales`))
Adidas_data$`Operating Profit` <- as.numeric(gsub("[^0-9.-]", 
                                                  "", Adidas_data$`Operating Profit`))

#Data visualization
#Sales Analysis
#1. Total Sales by Region (Bar Chart)
  total_sales_region <- Adidas_data %>%
  group_by(Region) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(total_sales_region, aes(x = Region, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Sales by Region", x = "Region", y = "Total Sales (Million)") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) 
  ) 

# Sales Trend Over Time
  sales_trend <- Adidas_data %>%
  group_by(`Invoice Date`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_trend, aes(x = `Invoice Date`, y = Total_Sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trend Over Time", x = "Date", y = "Total Sales (Million)") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) 
  ) 

# 4. Sales by Retailer (Stacked Bar Chart)
  sales_by_retailer <- Adidas_data %>%
  group_by(Retailer) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_by_retailer, aes(x = Retailer, y = Total_Sales, fill = Retailer)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Retailer", x = "Retailer", y = "Total Sales (Million)", fill = "Retailer") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) 
  ) 
# 5. Sales by Sales Method (Stacked Bar Chart)
  sales_by_sales_method <- Adidas_data %>%
  group_by(`Sales Method`) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) # Convert to Millions

ggplot(sales_by_sales_method, aes(x = `Sales Method`, y = Total_Sales, fill = `Sales Method`)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Sales Method", x = "Sales Method", y = "Total Sales (Million)", fill = "Sales Method") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) 
  ) 




# Quarterly Sales Trend by Region
quarterly_sales <- Adidas_data %>%
  mutate(Quarter = quarters(`Invoice Date`), Year = as.numeric(format(`Invoice Date`, "%Y"))) %>%
  group_by(Region, Year, Quarter) %>%
  summarise(Total_Sales = sum(`Total Sales`)/1e6) %>%
  arrange(Year, Quarter)

# Generate all combinations of Region, Year, and Quarter
all_combinations <- expand.grid(
  Region = unique(Adidas_data$Region),
  Year = unique(as.numeric(format(Adidas_data$`Invoice Date`, "%Y"))),
  Quarter = unique(quarters(Adidas_data$`Invoice Date`))
)

# Merge with the actual sales data to ensure all combinations are included
quarterly_sales <- merge(all_combinations, quarterly_sales, by = c("Region", "Year", "Quarter"), all.x = TRUE)
quarterly_sales[is.na(quarterly_sales)] <- 0  # Replace NAs with 0

# Facet plot
ggplot(quarterly_sales, aes(x = interaction(Year, Quarter), y = Total_Sales, fill = Region)) +
  geom_col() +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Quarterly Sales Trend by Region", x = "Quarter-Year", y = "Total Sales (Million)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10),
    legend.position = "none"
  ) 


#profit Analysis
# 1. Operating Profit by Region (Bar Chart)
profit_by_region <- Adidas_data %>%
  group_by(Region) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_region, aes(x = Region, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Total Operating Profit by Region", x = "Region", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) 
  ) 

# 2. Operating Profit by Sales Method (Bar Chart)
profit_by_sales_method <- Adidas_data %>%
  group_by(`Sales Method`) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_sales_method, aes(x = `Sales Method`, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Total Operating Profit by Sales Method", x = "Sales Method", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10)  
  )


# 3. Operating Profit by Product (Bar Chart)
profit_by_product <- Adidas_data %>%
  group_by(Product) %>%
  summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions

ggplot(profit_by_product, aes(x = Product, y = Total_Profit)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Total Operating Profit by Product", x = "Product", y = "Total Operating Profit (Million)") +
  theme(
    axis.text = element_text(size = 10),  
    plot.title = element_text(size = 20), 
    axis.title = element_text(size = 10) ,
    legend.position = "none"  
  ) 

# 4. Operating Profit by Retailer (Bar Chart)
  profit_by_retailer <- Adidas_data %>%
    group_by(Retailer) %>%
    summarise(Total_Profit = sum(`Operating Profit`)/1e6) # Convert to Millions
  
  ggplot(profit_by_retailer, aes(x = Retailer, y = Total_Profit)) +
    geom_bar(stat = "identity", fill = "purple") +
    labs(title = "Total Operating Profit by Retailer", x = "Retailer", y = "Total Operating Profit (Million)") +
    theme(
      axis.text = element_text(size = 10),  
      plot.title = element_text(size = 20), 
      axis.title = element_text(size = 10) 
    ) 
    
# 5. Operating Margin by Region (Box Plot)
    ggplot(Adidas_data, aes(x = Region, y = `Operating Margin`, fill = Region)) +
    geom_boxplot() +
    labs(title = "Operating Margin by Region", x = "Region", y = "Operating Margin") +
    theme(
      axis.text = element_text(size = 10),  
      plot.title = element_text(size = 20), 
      axis.title = element_text(size = 10)  
    ) 
# 7. Operating Margin by Product (Box Plot)
    ggplot(Adidas_data, aes(x = Product, y = `Operating Margin`, fill = Product)) +
      geom_boxplot() +
      labs(title = "Operating Margin by Product", x = "Product", y = "Operating Margin") +
      theme(
        axis.text.x = element_blank(), 
        axis.text = element_text(size = 10),  
        plot.title = element_text(size = 20), 
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14)
      )
    