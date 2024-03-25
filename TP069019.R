

#TP069019
#BENJAPORN HATTHI

install.packages("lubridate")
library(dplyr)
library(ggplot2)
library(lubridate)
house_data = read.csv("/Users/ben/Desktop/House_Rent_Dataset.csv")

house_data
names(house_data)=c("Posted On","BHK","Rent","Size","Floor","Area.type","Area.Locality",
                    "City","Furnishing_Status","Tenant.Preferred","Bathroom","Point.of.Contact")

str(house_data)
summary(house_data)
dim(house_data)

house_data <- house_data %>%
  select("Posted On","BHK","Rent","Size","Floor","Area.type","Area.Locality",
          "City","Furnishing_Status","Tenant.Preferred","Bathroom","Point.of.Contact")

# Convert 'Size' to numeric
house_data
str(house_data)
house_data = read.csv("/Users/ben/Desktop/House_Rent_Dataset.csv",encoding = "UTF-8")
print(house_data$Size)
print(house_data$SizeRange)

TotalBachelors <- house_data %>%
  mutate(Tenant.Preferred = ifelse(Tenant.Preferred %in% c("Bachelors", "Bachelors/Family"), "Total Bachelors", Tenant.Preferred)) %>%
  filter(Tenant.Preferred == "Total Bachelors")

TotalFamily <- house_data %>%
  mutate(Tenant.Preferred = ifelse(Tenant.Preferred %in% c("Family", "Bachelors/Family"), "Total Family", Tenant.Preferred)) %>%
  filter(Tenant.Preferred == "Total Family")

combined_tenant <- bind_rows(TotalFamily, TotalBachelors)
combined_tenant


# Filter the data to inclued only "Total Bachelors" and "Total Family" categories
total_tenant <- combined_tenant %>%
  filter(Tenant.Preferred %in% c("Total Bachelors","Total Family"))
total_tenant


#__________________________________________________________________________________________________________
# TP069019
# count the number from the data set before using in the bar chart
Unfurnished = nrow(house_data[house_data$Tenant.Preferred =="Family" & 
                                house_data$Furnishing_Status == "Unfurnished",])
Furnished = nrow(house_data[house_data$Tenant.Preferred =="Family" & 
                              house_data$Furnishing_Status == "Furnished",])
Semi = nrow(house_data[house_data$Tenant.Preferred =="Family" & 
                         house_data$Furnishing_Status == "Semi-Furnished",])
# set the type data into the bar chart and label it
furnishing_status <- data.frame(name = c("Unfurnished", "Furnished", "Semi-Furnished"),
                                value = c(Unfurnished, Furnished, Semi))
# inserts the value in the bar chart along give the color title x and y information
ggplot(furnishing_status, aes(x = name, y = value, fill = factor(name))) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = value), vjust = -0.3,size = 8) + 
  scale_fill_manual(values = c("purple", "pink", "skyblue")) +
  labs(x = "Furnishing Status", y = " Tenant preferred ( Family ) ", 
  title = " The amount of  the Family which prefer each type of furnishing Status ") + 
# add theme minimal to label the size of the bar graph 
  theme_minimal() + theme( text = element_text(size = 24),  
                           axis.title = element_text(size = 24),
                           plot.title = element_text(size = 28, face = "bold"))

#__________________________________________________________________________________________________________
# TP069019
# count the number from the data set before using in the chart
Family = nrow(house_data[house_data$Furnishing_Status =="Unfurnished",])
Bachelors = nrow(house_data[house_data$Furnishing_Status =="Furnished",])
any = nrow(house_data[house_data$Furnishing_Status == "Semi-Furnished",])
# set the type data into the chart and label it
furnishing_status <- data.frame(name = c("Unfurnished", "Furnished", "Semi-Furnished"), 
                                value = c(Unfurnished, Furnished, Semi))
# set the x-axis label, y-axis  label and also title
plot(1, type = "n", xlim = c(0.5, 3.5), ylim = c(0, max(furnishing_status$value) * 1.1),
     xlab = "Furnishing Status", ylab = "Bachelors/Family",
     main = " The Amount of Furnishing Status ")
# set the value to the point of the graph
points(seq_along(furnishing_status$name), furnishing_status$value, pch = 19, col = "#3a606e")
lines(seq_along(furnishing_status$name), furnishing_status$value, col = "#3a606e")
# length the graph and set the color under the graph
for (i in 1:(length(furnishing_status$name) - 1)) {
  polygon(c(i, i + 1, i + 1, i), c(0, 0, furnishing_status$value[i + 1], 
          furnishing_status$value[i]), col = "#c7c7a6", border = NA)}
# label the name using seq_along text to give the count value
text(seq_along(furnishing_status$name), furnishing_status$value, 
labels = furnishing_status$value, pos = 3)
axis(1, at = seq_along(furnishing_status$name), 
labels = furnishing_status$name, tick = TRUE, line = -0.5)


#__________________________________________________________________________________________________________

# TP069019
#install "plotrix" for make a 3D pie chart
install.packages("plotrix")
library(plotrix)

# TP069019
# count the furnishing status that only  the Bachelors side from the data set 
Unfurnished = nrow(combined_tenant %>% filter(Tenant.Preferred == "Total Bachelors"
                                               & Furnishing_Status == "Unfurnished"))
Furnished =  nrow(combined_tenant %>% filter(Tenant.Preferred == "Total Bachelors"
                                              & Furnishing_Status == "Furnished"))
Semi = nrow(combined_tenant %>% filter(Tenant.Preferred == "Total Bachelors"
                                       & Furnishing_Status == "Semi-Furnished"))
# set the type data into the data frame
furnishing_status <- data.frame(name = c("Unfurnished", "Furnished", "Semi-Furnished"),
                               value = c(Unfurnished, Furnished, Semi))
# set the value of the furnishing status and label before put in the pie chart
labels <- paste(furnishing_status$name, "\n(", furnishing_status$value, ")", sep = "")
values <- furnishing_status$value
# Create the pie chart with labeled segments and adding the detail color, title, explode,
pie3D(values, labels = labels,
      explode = 0.1, main = "The Amount of Bachelors Preferring Each Type of Furnishing Status",
      col = c("#483C32", "#6ABEA7", "#EDB458"), labelcex = 1)


#__________________________________________________________________________________________________________
# TP069019
library(ggplot2)
# Define size ranges
new_size_ranges <- cut(house_data$Size, 
                       breaks = c(0,1000,2000,3000,4000,5000,Inf),
                       labels = c("0-1000","1001-2000","2001-3000",
                                  "3001-4000","4001-5000","5001++"))
house_data$SizeRange <- new_size_ranges
# make box plot and fill the graph data with Size Range
ggplot(house_data, aes(x = Size, y = Furnishing_Status, fill = SizeRange)) +
  geom_boxplot() +
  labs(title = "The Relationship between Size Range and Furnishing Status",
       x = "Size Range",
       y = "Furnishing Status") +
  scale_fill_discrete(name = "Size Range") +
  # decorate theme graph the text position size, color and linetype 
  theme( plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0), size = 20),
         axis.title = element_text(color = "black", size = 20),
         panel.background = element_rect(fill = "#F1F4F9"),
         panel.border = element_rect(color = "lightblue", fill = NA),
         panel.grid = element_line(color = "lightblue", linetype = "twodash"))

#__________________________________________________________________________________________________________

# TP069019
library(ggplot2)
# label color to each Rental Range
custom_colors <- c("1200" = "#61f4de", "16000" = "#65cbe9", 
                   "50000" = "#68b6ef", "100000" = "#6c8dfa", "350000" = "#6e78ff")
# Define the range of each Rent
new_Rent_ranges <- cut(house_data$Rent, 
                       breaks = c(1200, 16000, 50000, 100000, 350000, Inf),
                       labels = c("1200", "16000", "50000", "100000", "350000"))
house_data$RentRange <- new_Rent_ranges
# label the x and y value into the graph
ggplot(house_data, aes(x = Furnishing_Status, y = RentRange, color = RentRange)) +
  geom_jitter(data = na.omit(house_data), alpha = 0.6, shape = 19, size = 3) +
  labs(title = "The Relationship between Rental Rate and Furnishing Status",
       x = "Furnishing Status",
       y = "Rental Rate") +
# label the rate Rental beside the graph
  scale_color_manual(name = "Rent Range", values = custom_colors,
                     labels = c("Very Low", "Low", "Medium", "High", "Very High")) +
# customize the graph with the theme to arrange the text size
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, 
        margin = margin(20, 0, 20, 0), size = 20),
        axis.title = element_text(color = "black", size = 20))


#________________________________________________________________________________________________
library(ggplot2)
library(dplyr)  


#TP09019
# Calculate bathroom categories
bathroom_Range <- cut(house_data$Bathroom, breaks = c(0, 1, 2, 3, 4, Inf),
                      labels = c("1", "2", "3", "4", "5++"))
# Add the bathroom range to the daataset
house_data$bathroom <- bathroom_Range
# Create the stacked area chart
ggplot(house_data, aes(x = bathroom_Range, fill = Furnishing_Status)) +
  geom_bar(position = "dodge") +
  labs(title = "Grouped Bar Chart of Bathroom Configurations by Furnishing Status",
       x = "Bathroom Range", y = "Count the number of furnishing status") +
  scale_fill_manual(values = c("#c0c0c0", "#464d77", "#36827f")) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, 
                    margin = margin(20, 0, 20, 0), size = 20),
       axis.title = element_text(color = "black", size = 20))



#______________________________________________________________________________________

library(ggplot2)

# TP069019
# Set the group plot data
plot_data <- house_data %>%
  group_by(BHK, Furnishing_Status) %>%
  summarise(count = n()) %>%
  arrange(BHK, Furnishing_Status)
# Create a line chart with filled area below the lines
ggplot(plot_data, aes(x = BHK, y = count, color = Furnishing_Status, group = Furnishing_Status)) +
  geom_line() +
  geom_area(aes(fill = Furnishing_Status), alpha = 1) +  # Add filled area
  labs(title = "Line Chart Area relationship of BHK and Furnishing Status",
       x = "BHK", y = "Count") +
  scale_color_manual(values = c("#9caf88", "#33658a", "#cabac8")) +
  scale_fill_manual(values = c("#9caf88", "#33658a", "#cabac8")) +  # Set fill colors
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, 
                          margin = margin(20, 0, 20, 0), size = 20),
                          axis.title = element_text(color = "black", size = 20))







