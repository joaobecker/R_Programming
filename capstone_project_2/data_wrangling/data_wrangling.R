library("dplyr")
library("tidyr")
library("ggplot2")

# 1st Step

# Joining two data frames to include the unsubscribed customers
upgraded_1 <- upgraded_11
wp_data <- wp_data1

#Transforming variables into characters to be able to use join function
wp_data[c(1:6, 12:16, 19)] <- lapply(wp_data[c(1:6, 12:16, 19)], as.character)
upgraded_1[c(1:6, 12:16, 19)] <- lapply(upgraded_1[c(1:6, 12:16, 19)], as.character)

# Here, we used anti_join to avoid having duplicates.
upgraded_1 <- anti_join(upgraded_1, wp_data, by = "pid")

#Combining Rows from different tables
wp_data <- bind_rows(wp_data, upgraded_1)

# Using left_join to maintain all the current data from "wp_data" while adding a new column, Unsubscribed, from "cust_can"
cust_can$pid <- as.character(cust_can$pid)
wp_data <- left_join(wp_data, cust_can, by = c("pid" = "pid"))

# Get rid off unwanted columns
wp_data <- wp_data[, -c(22:34)]

# Rename Column
wp_data <- rename(wp_data, Unsubscribed = Unsubscribed.x)


# 2nd Step

#Filling up blanks data points
glimpse(wp_data)


# Renamed the empty value and NAs into something easier to understand
wp_data$business_industry <- gsub("^$", "Unknown", wp_data$business_industry)
wp_data <- wp_data %>%  replace_na(list(business_industry = "Unknown"))
wp_data$selected_service <- gsub("^$", "Unknown", wp_data$selected_service)
wp_data$business_size[wp_data$business_size == "null"] <- "Unknown"
wp_data$current_cancel_contact_support <- gsub("^$", "No", wp_data$current_cancel_contact_support)
wp_data$current_cancel_feeling <- gsub("^$", "Unknown", wp_data$current_cancel_feeling)
wp_data$business_size <- gsub("^$", "Unknown", wp_data$business_size)
wp_data <- wp_data %>%  replace_na(list(has_seen_the_blog_overlay = "FALSE"))
wp_data$support_agent_name <- gsub("^$", "Unknown", wp_data$support_agent_name)
wp_data$locale <- gsub("^$", "Unknown", wp_data$locale)
wp_data$plan_name <- gsub("^$", "Unknown", wp_data$plan_name)
wp_data$sales_agent <- gsub("^$", "Unknown", wp_data$sales_agent)
wp_data$interest <- gsub("^$", "Unknown", wp_data$interest)
wp_data <- wp_data %>%  replace_na(list(sales = "FALSE"))
wp_data <- wp_data %>%  replace_na(list(affiliated = "FALSE"))
wp_data <- wp_data %>% replace_na(list(Unsubscribed = "FALSE"))


# Summarizing Levels 

# Have Website
wp_data$website[wp_data$website != ""] <- "TRUE"
wp_data$website[wp_data$website != "TRUE"] <- "FALSE"

# Analyze column levels
levels(wp_data$selected_service)
wp_data$selected_service <- gsub(".*self.*", "Self Service", wp_data$selected_service)
wp_data$selected_service <- gsub(".*fully.*", "Fully Managed", wp_data$selected_service)

# Plan Name Column
str(wp_data$plan_name)
wp_data$plan_name <- as.factor(wp_data$plan_name)
levels(wp_data$plan_name)
wp_data$plan_name <- gsub(".*Basic.*", "Basic", wp_data$plan_name)
wp_data$plan_name <- gsub(".*Growth.*", "Growth Plan", wp_data$plan_name)

# Interest Column
wp_data$interest <- gsub(".*coupon.*|.*photo.*|.*instagram*|.*referral.*|.*sweepstake.*|.*text.*|.*video.*|.*vote.*|.*pinterest.*", "Contest", wp_data$interest)
wp_data$interest <- gsub(".*forms.*|.*action*|.*pop.*", "Pop-Up", wp_data$interest)
wp_data$interest <- gsub(".*landing.*", "Landing Page", wp_data$interest)


# Sum of all the live tools on Wishpond to total_live_tools
wp_data <- wp_data %>%  mutate(total_live_tools =  live_popups + live_workflows + live_landings + live_contests)

# Discovering the top 15 business industries
# group by video ---> https://www.youtube.com/watch?v=jWjqLW-u3hc --> 24min
biz_grouped <- wp_data %>% group_by(business_industry) 
top_biz <- biz_grouped %>% count(business_industry) %>% arrange(desc(n))
top_15_biz <- head(top_biz, 15)

# Filtering the top 15 industries
top_biz_db <- wp_data %>% group_by(business_industry) %>% filter(n() >= 131, business_industry != "Unknown")
top_biz_col <- wp_data %>% group_by(business_industry) %>% filter(n() >= 131, business_industry != "Unknown") %>% select(pid, business_industry)

# Renaming businesses industry that are not part of the top 15
other_biz_col <- wp_data %>% group_by(business_industry) %>% filter(n() < 131, business_industry != "Unknown") %>% select(pid, business_industry)
other_biz_col$business_industry <-  "Other"
biz_unknown <- wp_data %>% filter(business_industry == "Unknown") %>% select(pid, business_industry)
biz_unknown$business_industry <- as.character(biz_unknown$business_industry)
top_biz_col$business_industry <- as.character(top_biz_col$business_industry)
other_biz_col$business_industry <- as.character(other_biz_col$business_industry)
all_biz <- bind_rows(top_biz_col, other_biz_col, biz_unknown)
wp_data$business_industry <- all_biz$business_industry



# 3rd Step

#Renaming column name
wp_data <- wp_data %>% rename( read_blog = has_seen_the_blog_overlay)
wp_data <- wp_data %>% rename( language = locale)
wp_data <- wp_data %>% rename( support_agent = support_agent_name)
wp_data <- wp_data %>% rename( cancel_contact_support = current_cancel_contact_support)
wp_data <- wp_data %>% rename( cancel_feeling = current_cancel_feeling)

#Rearranging the Data-base
wp_data <- wp_data %>% select(pid, business_industry, business_size, selected_service, live_workflows, live_popups, total_live_tools, live_contests, live_landings, total_leads, interest, read_blog, sales_agent, sales, affiliated, plan_name, website, support_agent, cancel_contact_support, cancel_feeling, Unsubscribed)

#download the data into CSV
write.csv(wp_data, "wp_data.csv")
save(wp_data, file 
     = 'wp_data.RData')
save(upgraded_1, file 
     = 'upgraded_1.RData')
save(cust_can, file 
     = 'cust_can.RData')
save(wp_data1, file 
     = 'wp_data1.RData')
save(upgraded_11, file 
     = 'upgraded_11.RData')
