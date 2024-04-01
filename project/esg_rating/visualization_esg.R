library(tidyverse)
df <- read_csv('data/SP 500 ESG Risk Ratings.csv')

# Diagnose whether there are NA values or not 
library(dlookr)
describe(df)
# We can see that there are NA values in the data. So, we are going to make 
df_clean <- df[complete.cases(df),]
attach(df_clean)


# Caculate the mean of sector wise "Total ESG Risk"
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




  
