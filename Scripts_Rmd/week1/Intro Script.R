#####Intro to R with CAL Fire data set (2013-2019)#####
#Lexie Christopoulos 
#10/22/2020

##### Load Packages #####

library("tidyverse")
#install.packages("dplyr")
library("dplyr")
#install.packages("readxl")
library("readxl")
#install.packages("praise")
library("praise")

##### Load Datasets #####
excel_sheets("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx")

metadata <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Metadata")
View(metadata)
data <- read_excel("Input_Data/week1/2013_2019_CALFIRE_Redbook.xlsx", sheet="Data")
View(data)


##### Initial Data Exploration #####
names(data)
dim(data)
class(data)
head(data)
tail(data)
str(data)
glimpse(data)
typeof(data$Total_Acres_Burned)#single columns can be refered to using $ 
max(data$Total_Acres_Burned)
max(data$Structures_Destroyed, na.rm = TRUE)

summary(data)


#####Basic data wrangling (dplyr functions)######
df1 <- select(data, County_Unit:Controlled_Date, Total_Acres_Burned, Cause:Structures_Damaged)
unique(df1$County_Unit)

df2 <- filter(df1, County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "ORANGE", "VENTURA/SANTA BARBARA") & Total_Acres_Burned >= 500)

# |= "or"
# == means "equals"/"matches" and is the same as %in% c(), but only works for one thing/data point 

df3 <- arrange(df2, desc(Total_Acres_Burned))

df4 <- mutate_at(df3, vars("Structures_Destroyed", "Structures_Damaged"), replace_na,0)

df5 <- mutate(df4, structure_impact = Structures_Damaged + Structures_Destroyed)

#mess with time 
library(lubridate)

df6 <- mutate(df5, interv = interval( Start_Date, Controlled_Date), dur = as.duration(interv), days = as.numeric(dur, "days"))



##### Introduction to Piping ######

socal.fires <- data %>% 
  select(County_Unit:Controlled_Date, Total_Acres_Burned, Cause:Structures_Damaged)%>% 
  filter(County_Unit %in% c("SANTA BARBARA", "VENTURA", "LOS ANGELES", "ORANGE", "SAN DIEGO", "VENTURA/SANTA BARBARA") & Total_Acres_Burned >= 500) %>% 
  arrange(desc(Total_Acres_Burned)) %>% 
  mutate_at(vars("Structures_Destroyed", "Structures_Damaged"), replace_na,0) %>% 
  mutate(structure_impact = Structures_Damaged + Structures_Destroyed) %>% 
  mutate(interv = interval( Start_Date, Controlled_Date), dur = as.duration(interv), days = as.numeric(dur, "days"))

##### First Graphs in ggplot #####

#We're going to make a graph of acres burned in the South Coast from 2013-2019, with the color dependent on which count we're showing. 

#Three things you must tell R to make a graph in ggplot: 
# 1) That you're using ggplot 
# 2) What data you're using (including what should be x and what should be y)
# 3) What type of graph you want to create. 
# Everything after is extra to make it pretty. 

ggplot(socal.fires, aes(x = Start_Date, y = Total_Acres_Burned))+ 
  geom_point(aes(color = County_Unit))+
  ggtitle("CA South Coast Major Fires \n2014-2018")+
  labs(x = " ", y = "Total Acres Burned", color = "Counties")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_grid(rows = "County_Unit", scales = "free")

plot.data <- socal.fires %>% 
  rename(county = County_Unit, acres = Total_Acres_Burned, start = Start_Date, end = Controlled_Date) %>%   
  mutate(year = year(start), county = ifelse(county == "VENTURA/SANTA BARBARA", "VENTURA", county))  
 
  incidents <- plot.data %>% 
  group_by(county,year) %>% 
  tally() %>% 
  ungroup()

incidents.plot <- incidents %>% 
  ggplot(aes(x = year, y = n))+
  geom_point()+
  geom_line()+
  labs(title = "CA South Coast Major Fire Incidents \n 2013-2018", x = " ", y = "Incidents", color = "Counties")+
  theme_bw()+
  facet_grid(rows = "county", scales = "free")+
  guides(color = FALSE)


all.incidents <- plot.data %>% 
  group_by(year) %>% 
  tally() %>% 
  ungroup()

all.incidents.plot <- all.incidents %>% 
  ggplot(aes(x = year, y = n))+
  geom_point()+
  geom_line()+
  labs(title = "CA South Coast Major Fire Incidents \n 2013-2018", x = " ", y = "Incidents")+
  theme_bw()
 


##### Saving Data and Plots ##### 

saveRDS(socal.fires, file = "Output_Data/week1/socal_fires_data.rds")
write.csv(socal.fires, "Output_Data/week1/socal_fires_data.csv")
ggsave(filename = "Fire Incidents", all.incidents.plot, device = "jpeg", "Output_Data/week1/")
