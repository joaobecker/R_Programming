library("dplyr")
library("tidyr")
library("ggplot2")

# No Unsubscribed
wp_data_subscribed <- wp_data %>% filter(Unsubscribed == "FALSE", business_industry != "Other")
wp_data_subscribed <- wp_data %>% filter(plan_name != "Unknown",plan_name != "Starter" )
na.omit(wp_data_subscribed)

# A lot of people are with the starter plan - meaning they havent paid wishpond yet. However, these are all people that have high chance of upgrading
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$business_industry))) +
  facet_grid(. ~ factor(wp_data_subscribed$plan_name)) +
  xlab("Business Industry") +
  ylab("") +
  scale_fill_discrete(name = "Business Industry")        



ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$total_leads))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$total_leads))) +
  facet_grid(. ~ factor(wp_data_subscribed$plan_name)) +
  xlab("Total Leads") +
  ylab("") +
  scale_fill_discrete(name = "Total Leads")      


ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$total_live_tools))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$total_live_tools))) +
  facet_grid(. ~ factor(wp_data_subscribed$plan_name)) +
  xlab("Total Tools") +
  ylab("") +
  scale_fill_discrete(name = "Total Tools")        
