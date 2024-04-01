library(tidyverse)
library(dlookr)
df <- read_csv('SP 500 ESG Risk Ratings.csv')

library(purrr)

describe(df)

df_clean <- df[complete.cases(df),]
attach(df_clean)
sym(`Total ESG Risk score`)
map("`Total ESG Risk score`", ~ df_clean %>%  group_by(Sector) %>% summarize(mean_ESG_Risk = mean(.x)) %>% 
                                                                             arrange(mean_ESG_Risk))


sector_wise_risk <- df_clean %>% group_by(Sector) %>% 
  summarize(mean_ESG_Risk = mean(`Total ESG Risk score`)) %>% 
  arrange(mean_ESG_Risk)

df_clean

win.graph(width=100, height=100)

sector_wise_risk %>% ggplot(aes(x = Sector, y = mean_ESG_Risk)) +
  geom_bar(stat='identity')

sector_wise_risk %>% ggplot(aes(x = reorder(Sector, mean_ESG_Risk), y=mean_ESG_Risk)) +
  geom_bar(stat = 'identity')

dim(sector_wise_risk)[1]
sector_wise_risk %>% ggplot(aes(x = reorder(Sector, mean_ESG_Risk), y=mean_ESG_Risk)) +
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sector-wise ESG Risk Analysis",
       x = "Sector",
       y = "Average Total ESG Risk Score")


top_10 <- df %>% arrange(desc(`Total ESG Risk score`)) %>% head(10)
top_10 %>% ggplot(aes(x = reorder(Name, `Total ESG Risk score`), y = `Total ESG Risk score`)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label=`Total ESG Risk score`), vjust = -0.2 , color='black' ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Performers by ESG Risk Level",
       x = "Companies",
       y = "Total ESG Risk Score")



  
