#titanic file to titanic vector
titanic <- titanic_original 


# The code below find empty strings and replace with what we want, which is "S"
embarked <- titanic$embarked

titanic$embarked <- gsub("^$","S", embarked)



# Calculate the mean of the Age column and use that value to populate the missing values

# Get the mean while excluding NA
mean(titanic$age, na.rm= TRUE)


# Replace all the NA values with the age mean

titanic$age[is.na(titanic$age)] <- 29.8

#If NA was to be 0, this is how we would transform into the age mean.
titanic$age[titanic$age == 0] <- 29.8


#Other ways we could've computed the age mean, is wiwht median


#If there is no value under the boat column, we will make it into NONE
titanic$boat <- gsub("^$","None", titanic$boat)


#Create a new column called has_cabin_number
titanic$has_cabin_number <- 0

#Give a binary number depending if a person has or dont have a cabin
#the code below assigns 1 to all people that do not have anything under the cabin column

titanic$has_cabin_number[titanic$cabin != ""] <- 1


#GGPLOT for Titanic
posn.jd <- position_jitterdodge(0.5, 0, 0.6)
titanic %>% 
    filter(!is.na(sex) & !is.na(pclass)) %>% 
      ggplot(aes(x = pclass, y = age, col = sex)) +
        geom_point(size = 3, alpha = 0.5, position = posn.jd) +
        facet_grid(. ~survived)


# Exporting it to CSV
write.csv(titanic, file = "titanic_clean.csv",row.names=FALSE)
