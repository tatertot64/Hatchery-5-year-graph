#Let's make a graph of Coho at Nasaelle across Weeks for the last 5 years!

#Make sure to save your excel sheet as a .csv file first! This is the most important step.
#you can then select it to upload into R with the following code
Hatcherydata <- read.csv (file.choose ())

library(lubridate)
library(tidyverse) #You will need this, install.packages('tidyverse') first if you don't have it
library(RColorBrewer)
library(stringr)

#Let's prepare the dataframe that we uploaded for graphing!

colnames(Hatcherydata) <- Hatcherydata[7, ] #making the column names the names in the sheet

Hatcherydata <- Hatcherydata[-c(1:7), ] #We need to get rid of these rows cause they confuse R
Hatcherydata <- Hatcherydata[-nrow(Hatcherydata), ] #get lid of the last row so the totals don't get graphed

colnames(Hatcherydata) <- gsub(" ", ".", colnames(Hatcherydata)) #get rid of spaces in the names

Hatcherydata$Date<- as.Date(Hatcherydata$Date, format = "%m/%d/%Y") #need to turn the dates into a format that will work for making them into stat week!

Hatcherydata <- Hatcherydata %>% #Add extra columns to the data extracting the year and week from the dates
  mutate(
    year = year(Hatcherydata$Date),
    Week = isoweek(Hatcherydata$Date)
  )

Hatcherydata$Brood <- factor(Hatcherydata$Brood) #some categories aren't numbers and need to be factors
Hatcherydata$Date <- as.character(Hatcherydata$Date) 
Hatcherydata$Mark.Info <- factor(Hatcherydata$Mark.Info) 
Hatcherydata$Checked.for.Wire <- factor(Hatcherydata$Checked.for.Wire)
Hatcherydata$Wire.Present <- factor(Hatcherydata$Wire.Present) 
Hatcherydata$Wire.Collected <- factor(Hatcherydata$Wire.Collected)
Hatcherydata$Bag.Number<- factor(Hatcherydata$Bag.Number)
Hatcherydata$Adults.In<- as.numeric(Hatcherydata$Adults.In)
Hatcherydata$year<- as.factor(Hatcherydata$year)
Hatcherydata$Week<-as.factor(Hatcherydata$Week)

Hatcherydata[Hatcherydata == ""] <- NA

filtered_data <- Hatcherydata %>% 
  filter(Adults.In > 0) %>%  # Keep rows where Adults.In is greater than 0
  filter(str_detect(Brood, "^CO:NA")) %>%  # Filter rows where 'Brood' starts with "CO:NA", Change this if you want to see LA!!!
  group_by(Week, year) %>%  # Group by both Week and year
  summarize(
    Adults.In = sum(Adults.In, na.rm = TRUE)  # Sum Adults.In within each week-year group
  ) %>%
  ungroup() 

 #Make the blank cells NA!


# Create the grouped bar chart with numeric x-axis

ggplot(filtered_data, aes(x = Week, y = Adults.In, fill = year)) +
  geom_col(position = position_dodge2(preserve = 'single')) +
  xlab("Week") +
  ylab("Total Number of CO:NA adults in") +   #Change the name of the species here!!!!
  theme(text = element_text(size = 8)) + 
  ggtitle("Naselle CO 2019-Now") +   #change the name of the hatchery here!!!!!
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set2") #this changes the color if you don't like the colors I picked!!!!
  
