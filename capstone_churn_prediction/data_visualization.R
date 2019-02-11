library("dplyr")
library("tidyr")
library("ggplot2")

# Paying Subscribed Customers 
wp_data_subscribed <- wp_data %>% filter(Unsubscribed == "FALSE", plan_name != "Unknown",plan_name != "Starter")


# All Subscribed customers 
wp_data_subscribed_all <- wp_data %>%  filter(Unsubscribed == "FALSE")

ggplot(wp_data_subscribed_all, aes(plan_name)) +
  geom_bar(stat = "count", aes(fill = plan_name)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name") 



#------------------------   Paying subscribers and how are they using the platform

# Paying Subscribed Customer  - Total Amount of leads and their plan | Interesting - A lot of people have no leads, high chance of cancelling.
#For this grap, we start with those who have acquired at least 1 lead. If we count those who havent captured any lead, it will distort the graph, which is not the best signal as there are a lot of customers that are not capturing any lead.
# Interesting to see that there are a lot of customers on the basic plan that have acquired more leads than the maximun allowed under their current plan (500 leads), which means that they upgraded their account to run a campaign and then went back to basic plan. 
ggplot(wp_data_subscribed, aes(total_leads, fill = plan_name)) +
  geom_histogram(breaks = seq(1,20000, by = 100))  +
  labs(x = "Total Leads", y = "Count") +
  scale_fill_discrete(name = "Plan Name") 



# Paying Subscribed Customer - Total amount of tools and their plan | Interesting - A lot of people have no tools done, high chance of cancelling.

# For this graph, we start with those who have created at least 1 tool. If we count those who havent created anything, it will distort the graph, which is not the greatest as there are lot of people not using Wishpond's tool.
# The graph below shows us that after 25 tools, most of the people are on the Pro or growth plan.
ggplot(wp_data_subscribed, aes(total_live_tools, fill = plan_name)) +
  geom_histogram(breaks = seq(1,150, by = 5)) +
  labs(x = "Total Live Tools", y = "Count") +
  scale_fill_discrete(name = "Plan Name") 


# Dont need to include this.
# Paying Subscribed Customers - Customers that havent done anything on the platform | High risks of cancelling
wp_data_subscribed_nothing <- wp_data %>% filter(Unsubscribed == "FALSE", plan_name != "Unknown", plan_name != "Starter", total_live_tools == 0, total_leads ==0)
ggplot(wp_data_subscribed_nothing, aes(plan_name)) +
  geom_bar(stat = "count", aes(fill = plan_name)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name") 


# Paying Subscribed Customer --> Live Landings Pages & Live Workflows 
ggplot(wp_data_subscribed, aes(live_landings, live_workflows, color= plan_name)) + 
  geom_point() 



#------------  Sales - Sales People Or Affiliated - Who are the ones generating most sales/customers? 

# Paying Subscribed Customers - Their plan & if they came through a demo from a sales agent
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$plan_name))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$plan_name))) +
  facet_grid(. ~ factor(wp_data_subscribed$sales)) +
  xlab("Plan Name") +
  scale_fill_discrete(name = "Plan Name")     

# Paying Subscribed Customers - Their plan & if they came through a demo from an affiliated link
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$plan_name))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$plan_name))) +
  facet_grid(. ~ factor(wp_data_subscribed$affiliated)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name")     

#----------------  Who are the "best" sales agent at Wishpond (bringing the most customers by plan)? 

# Sales agents and the plan they have sold to each customer
wp_data_subscribed_sales <- wp_data %>% filter(plan_name != "Unknown",plan_name != "Starter", sales_agent != "Unknown" )
ggplot(wp_data_subscribed_sales, aes(factor(wp_data_subscribed_sales$plan_name))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed_sales$plan_name))) +
  facet_grid(. ~ factor(wp_data_subscribed_sales$sales_agent)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name")     




#------------  Blog - Are current subscribed customers engaging with Wishpond Blog (Trying to learn new ways to generate more leads)?

# Paying Subscribed Customers - Their plan & if they have read the blog 
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$plan_name))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$plan_name))) +
  facet_grid(. ~ factor(wp_data_subscribed$read_blog)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name")     


# Impact on blog in supporting people generate leads  | There might be some correlation in reading the blog and generating more leads 
ggplot(wp_data_subscribed, aes(total_leads, fill = plan_name)) +
  geom_histogram(breaks = seq(1,20000, by = 100))  +
  facet_grid(. ~ factor(wp_data_subscribed$read_blog)) +
  labs(title = "Have Read The Blog?", x = "Total Leads", y = "Count") +
  scale_fill_discrete(name = "Plan Name")

# Impact on blog in supporting people creating more wishpond tools | Actually, less people create workflow if they read the blog
ggplot(wp_data_subscribed, aes(total_live_tools, fill = plan_name)) +
  geom_histogram(breaks = seq(1,150, by = 5)) +
  facet_grid(. ~ factor(wp_data_subscribed$read_blog)) +
  labs(title = "Have Read The Blog?", x = "Total Live Tools", y = "Count") +
  scale_fill_discrete(name = "Plan Name") 



#--------  What business industry uses Wishpond the most? And, what are the business industries bringing the most revenue to Wishpond? 
# Subscribed Customer at each plan  | We will exclude "Other" business industries as it is a bundle of business industries with very few occurences.

# Starter Plan & Business Industry - No Revenue  | E-Commerce & Agency are the most relevant ones
wp_data_starter <- wp_data %>% filter(plan_name == "Starter", Unsubscribed == "FALSE", business_industry != "Other")
ggplot(wp_data_starter, aes(factor(wp_data_starter$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_starter$business_industry))) +
  facet_grid(. ~ factor(wp_data_starter$plan_name)) +
  xlab("business industry") +
  ylab("") +
  scale_fill_discrete(name = "business industry")     


# Basic Plan & Business Industry | Cheapest Plan | E-Commerce & Angencies are bringing a similar amount of customers, then education and fitness health bringing a good amount of customers
wp_data_basic <- wp_data %>% filter(plan_name == "Basic", Unsubscribed == "FALSE", business_industry != "Other")
ggplot(wp_data_basic, aes(factor(wp_data_basic$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_basic$business_industry))) +
  facet_grid(. ~ factor(wp_data_basic$plan_name)) +
  xlab("business industry") +
  ylab("") +
  scale_fill_discrete(name = "business industry")     

# Pro Plan & Business Industry | Second Most Expensive Plan | E-Commerce by far the most relevant one. Art-design and fitness health industry coming in second
wp_data_pro <- wp_data %>% filter(plan_name == "Pro", Unsubscribed == "FALSE", business_industry != "Other")
ggplot(wp_data_pro, aes(factor(wp_data_pro$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_pro$business_industry))) +
  facet_grid(. ~ factor(wp_data_pro$plan_name)) +
  xlab("business industry") +
  ylab("") +
  scale_fill_discrete(name = "business industry")     

# Growth Plan & Business | Most Expensive Plan  | E-Commerce by far again bringing the most customers to Growth plan.
wp_data_growth <- wp_data %>% filter(plan_name == "Growth Plan", Unsubscribed == "FALSE", business_industry != "Other")
ggplot(wp_data_growth, aes(factor(wp_data_growth$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_growth$business_industry))) +
  facet_grid(. ~ factor(wp_data_growth$plan_name)) +
  xlab("business industry") +
  ylab("") +
  scale_fill_discrete(name = "business industry")     



#---------------------  What are customers main reason to use Wishpond? 

# Initial Interest & Their Plans
ggplot(wp_data_subscribed_all, aes(factor(wp_data_subscribed_all$interest))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed_all$interest))) +
  facet_grid(. ~ factor(wp_data_subscribed_all$plan_name)) +
  xlab("Interest") +
  ylab("") +
  scale_fill_discrete(name = "Interest")     


# Paying customers - Size of their business & their plans  | Most of the customers work at for companies with 1 to 5 employees. However, at the pro plan 6 to 15 employess is the most relevant
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$business_size))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$business_size))) +
  facet_grid(. ~ factor(wp_data_subscribed$plan_name)) +
  xlab("Business Size") +
  ylab("") +
  scale_fill_discrete(name = "Business Size")     



#--------------- Do customer have a website before signing-up at Wishpond? | Good to know if we should promote that customers can use wishpond without a domain.

# All Paying Customers - If they have a website & their plans 
ggplot(wp_data_subscribed, aes(factor(wp_data_subscribed$plan_name))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_subscribed$plan_name))) +
  facet_grid(. ~ factor(wp_data_subscribed$website)) +
  xlab("Plan Name") +
  ylab("") +
  scale_fill_discrete(name = "Plan Name")     


#--------------  What business industries unsubscribed customers belong to?

#  Unsubscribed Customers & their business industry | Intersting to notice that B2B Business have one of the highest unsubscribe rate, while not having a high paying subscription rate
wp_data_unsubscribed <- wp_data %>% filter(Unsubscribed == "TRUE", business_industry != "Other")
ggplot(wp_data_unsubscribed, aes(factor(wp_data_unsubscribed$business_industry))) +
  geom_bar(stat = "count", aes(fill = factor(wp_data_unsubscribed$business_industry))) +
  xlab("Business Industry") +
  ylab("") +
  scale_fill_discrete(name = "Business Industry")        




