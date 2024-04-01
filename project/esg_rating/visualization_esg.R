library(tidyverse)
df <- read_csv('data/SP 500 ESG Risk Ratings.csv')

# Diagnose whether there are NA values or not 
library(dlookr)
describe(df)
# We can see that there are NA values in the data. So, we are going to make 
df_clean <- df[complete.cases(df),]

# Calculate the mean of sector wise "Total ESG Risk"
sector_wise_risk <- df_clean %>% group_by(Sector) %>% 
  summarize(mean_ESG_Risk = mean(`Total ESG Risk score`)) %>% 
  arrange(mean_ESG_Risk)

# Draw the bar plot for sector wise ESG Risk 
sector_wise_risk %>% ggplot(aes(x = reorder(Sector, mean_ESG_Risk), y=mean_ESG_Risk)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sector-wise ESG Risk Analysis",
       x = "Sector",
       y = "Average Total ESG Risk Score")

# Now we are going to draw sector-wise E, S, G pillars respectively. 
# I'm going to use map function (purrr) for avoiding repeated work.

calculate_mean <- function(df, column) {
  df %>%
    group_by(Sector) %>%
    summarize(mean_score = mean(.data[[column]]), .groups = 'drop')
}
# Environment, Social, Governance Risk Score are what we have interested in. 
risk_columns <- c("Environment Risk Score","Social Risk Score","Governance Risk Score")
# Use map function for calculating mean of each sector for each category (E, S, G)
mean_ESG <- risk_columns %>% map(~calculate_mean(df_clean,.x))  
names(mean_ESG) <- risk_columns

# Now we are going to draw three plots for each E, S, G pillars. 
# Likewise, to avoid repeated work, I made a plot function
plot_fun <- function(df) {
  df %>% ggplot(aes(x = reorder(Sector, mean_score), y = mean_score)) +
    geom_bar(stat = 'identity') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Sector")
} 

plot_fun(mean_ESG$`Environment Risk Score`)
plot_fun(mean_ESG$`Social Risk Score`)
plot_fun(mean_ESG$`Governance Risk Score`)

