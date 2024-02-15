setwd("path/to/your/folder")
# Load the required library for reading Excel files
library(readxl)
library(ggplot2)
library(vctrs)
detach("package:vctrs", unload = TRUE)
library(dplyr)
ScreenTime_SPH <- read_excel("C:/Users/suh/Dropbox/My PC (DESKTOP-P0455LB)/Downloads/ScreenTime_SPH.xlsx")
View(ScreenTime_SPH)
#screentime <- read_excel("screentime.xlsx")
#View(screentime)
#ScreenTime_SPH<-ScreenTime_SPH %>%
#install.packages("vctrs")

 
 # Assuming 'Date' is in the format "1/16/2024"
ScreenTime_SPH$Date <- as.Date(ScreenTime_SPH$Date, format = "%m/%d/%Y")
 
#ScreenTime_SPH <- ScreenTime_SPH %>%
#   mutate(if_weekend = as.factor(ifelse(weekdays(Date) %in% c("Sun", "Sat"), "weekend", "weekday")),
#          weekday = !weekdays(Date) %in% c("Sun", "Sat"))
 
 
 # Adding in weekend for extra Visualization
ScreenTime_SPH$weekdays <- weekdays(ScreenTime_SPH$Date, abbreviate = TRUE)
ScreenTime_SPH <- ScreenTime_SPH %>%
   mutate(if_weekend = as.factor(ifelse(weekdays %in% c("Sun", "Sat"), "weekend", "weekday")),
          weekday = !weekdays %in% c("Sun", "Sat"))
 
 # Check the structure of the modified dataset
 str(ScreenTime_SPH)
 
  
 
  
  # Check summary statistics
  summary(ScreenTime_SPH)
  # Convert time strings to minutes
  time_to_minutes <- function(time_str) {
    parts <- strsplit(time_str, "h|m")
    hours <- as.numeric(sapply(parts, function(x) as.numeric(x[1])))
    minutes <- as.numeric(sapply(parts, function(x) as.numeric(x[2])))
    total_minutes <- hours * 60 + minutes
    return(total_minutes)
  }
  
  # Convert Total. ST and Social .ST to numeric
  ScreenTime_SPH$Total.ST.min <- time_to_minutes(ScreenTime_SPH$`Total. ST`)
  ScreenTime_SPH$Social.ST.min <- time_to_minutes(ScreenTime_SPH$`Social .ST`)
  
  # Check the updated structure and summary
  str(ScreenTime_SPH)
  summary(ScreenTime_SPH)
  # Create new variables
  ScreenTime_SPH$daily_proportion_social <- ScreenTime_SPH$Social.ST.min / ScreenTime_SPH$Total.ST.min
  ScreenTime_SPH$daily_duration_per_use <- ScreenTime_SPH$Total.ST.min / ScreenTime_SPH$Pickups
  
  # Check the updated structure and summary
  str(ScreenTime_SPH)
  summary(ScreenTime_SPH)
  #Problem #2 
  #a)Time Series
  # Load necessary libraries
  
  
  # Convert Date to a date object
  ScreenTime_SPH$Date <- as.Date(ScreenTime_SPH$Date, format="%m/%d/%Y")
  
   # Time series plot
  ggplot(ScreenTime_SPH,aes(x=Date,y=Total.ST.min,color=if_weekend)+geom_line(color="blue"))+geom_point()+
    labs(x="Date",y="Total ScreenTime_SPH(min)")+
    scale_y_continuous(limits = c(100,550),breaks = seq(100,550,by=100))+
    scale_color_manual(labels=c("weekdays","weekends"),values = c("black","purple"))+ 
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 60,hjust = 1),legend.title = element_blank())
  names(ScreenTime_SPH)
  class(ScreenTime_SPH$if_weekend)
  
  # Assuming that "Date" is a character column representing dates
  ScreenTime_SPH$Date <- as.Date(ScreenTime_SPH$Date, format = "%m/%d/%Y")
  
  # Create the day of the week column
  ScreenTime_SPH$if_weekend <- ifelse(weekdays(ScreenTime_SPH$Date) %in% c("Sun", "Sat"), "weekend", "weekday")
  
  # Now you can use if_weekend in your ggplot code
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(x = "Date", y = "Total ScreenTime_SPH (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekend", "weekday"), values = c("black", "green")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
 
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(x = "Date", y = "Social.ST.min (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekend", "weekday"), values = c("black", "green")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
 
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(x = "Date", y = "Pickups (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekend", "weekday"), values = c("black", "green")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
    
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(x = "Date", y = " daily_proportion_social (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekend", "weekday"), values = c("black", "green")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
 
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "blue") +
    geom_point() +
    labs(x = "Date", y = "daily_duration_per_use (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekend", "weekday"), values = c("black", "green")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
   # Assuming 'ScreenTime_SPH' is your data frame
  # Convert Date to Date format
  ScreenTime_SPH$Date <- as.Date(ScreenTime_SPH$Date, format = "%m/%d/%Y")
  
  # Check the structure of the dataset
  str(ScreenTime_SPH)
  
  # Line plot for Total Screen Time
  #ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    #geom_line() +
   # geom_point() +
    #labs(title = "Total Screen Time Over Time",
         #x = "Date",
         #y = "Total Screen Time (min)") +
    #scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    #scale_color_manual(labels = c("weekdays", "weekends"), values = c("black", "purple")) +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  
  
  # Convert if_weekend to a factor
  ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("weekday", "weekend"))
  
  
  ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("weekday", "weekend"))
  
  # Use fill instead of color for the line and point aesthetics
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +
    geom_line(color = "steelblue") +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(labels = c("weekdays", "weekends"),values = c("black","purple")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  # Convert if_weekend to a factor
  ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("weekday", "weekend"))
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "purple"), name = "Day Type") +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "purple")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "purple"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "purple"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  ##
   ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min)) +
    geom_point(aes(color=if_weekend))+geom_line()+
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y ="Pickups (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min)) +
    geom_point(aes(color=if_weekend))+geom_line()+
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y ="daily_duration_per_use (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min)) +
    geom_point(aes(color=if_weekend))+geom_line()+
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y ="daily_proportion_socia (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
   Use fill instead of color for the line and point aesthetics
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend)) +#fill = if_weekend
   geom_line(aes(color = if_weekend)) +
  geom_point(aes(color = if_weekend)) +
  labs(title = "Total Screen Time Over Time",
       x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  
  #a)       
    theme_minimal()
ScreenTime_SPH <- na.omit(ScreenTime_SPH)
  ggplot(ScreenTime_SPH, aes(x = Date)) +
    geom_line(aes(y = `Total.ST.min`, group = 1), color = "blue", linetype = "solid") +
    geom_line(aes(y = `Social.ST.min`, group = 1), color = "red", linetype = "solid") +
    geom_line(aes(y = Pickups, group = 1), color = "green", linetype = "solid") +
    geom_line(aes(y = daily_proportion_social, group = 1), color = "purple", linetype = "solid") +
    geom_line(aes(y = daily_duration_per_use, group = 1), color = "orange", linetype = "solid") +
    labs(title = "Time Series Plot",
         y = "Values",
         x = "Date") +
    theme_minimal()
  # Convert if_weekend to a factor
  ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("weekday", "weekend"))
  # Assuming you have created daily_proportion_social
  ScreenTime_SPH$daily_proportion_social <- # Your calculation here
    
  # Use fill instead of color for the line and point aesthetics
  ggplot(ScreenTime_SPH, aes(x = Date, y = Total.ST.min, color = if_weekend, fill = if_weekend)) +
    geom_line(aes(color = if_weekend)) +
    geom_point(aes(color = if_weekend)) +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "Total Screen Time (min)") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "black"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  ggplot(ScreenTime_SPH, aes(x = Date)) +
    geom_line(aes(y = Total.ST.min, color = "Total Screen Time"), linetype = "solid") +
    geom_line(aes(y = Social.ST.min, color = "Social Screen Time"), linetype = "solid") +
    geom_line(aes(y = Pickups, color = "Pickups"), linetype = "solid") +
    geom_line(aes(y = daily_proportion_social, color = "Daily Proportion Social"), linetype = "solid") +
    geom_line(aes(y = daily_duration_per_use, color = "Daily Duration Per Use"), linetype = "solid") +
    labs(title = "Time Series Plot",
         y = "Values",
         x = "Date") +
    theme_minimal()
  ggplot(ScreenTime_SPH, aes(x = Date, y = daily_duration_per_use, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "daily_duration_per_use") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "black"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  # Check for complete cases
  complete_data <- ScreenTime_SPH[complete.cases(ScreenTime_SPH[, c("Date", "daily_duration_per_use", "if_weekend")]), ]
  
  # Update ggplot code
  ggplot(complete_data, aes(x = Date, y = daily_duration_per_use, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "daily_duration_per_use") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "black"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  # Identify rows with missing values
  missing_rows <- ScreenTime_SPH[!complete.cases(ScreenTime_SPH[, c("daily_duration_per_use", "if_weekend")]), ]
  # Show rows with missing values
  print(missing_rows)
  # Impute missing values (replace with mean)
  ScreenTime_SPH$daily_duration_per_use[is.na(ScreenTime_SPH$daily_duration_per_use)] <- mean(ScreenTime_SPH$daily_duration_per_use, na.rm = TRUE)
  
  # Remove rows with missing values
  ScreenTime_SPH <- ScreenTime_SPH[complete.cases(ScreenTime_SPH[, c("daily_duration_per_use", "if_weekend")]), ]
  
  # Identify rows with missing values in any variable
  missing_rows <- ScreenTime_SPH[!complete.cases(ScreenTime_SPH), ]
  
  # Show rows with missing values
  print(missing_rows)
  ggplot(complete_data, aes(x = Date, y = daily_duration_per_use, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "daily_duration_per_use") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "black"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  # Assuming you have already removed missing values and assigned the data to complete_data
  ggplot(complete_data, aes(x = Date, y = daily_duration_per_use, color = if_weekend, fill = if_weekend)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Screen Time Over Time",
         x = "Date",
         y = "daily_duration_per_use") +
    scale_y_continuous(limits = c(100, 550), breaks = seq(100, 550, by = 100)) +
    scale_fill_manual(values = c("weekday" = "black", "weekend" = "green"), name = "Day Type") +
    scale_color_manual(values = c("weekday" = "black", "weekend" = "black"), name = "Day Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.title = element_blank())
  
  # Print the plot
  print(last_plot())
  
  #b)
  
    # Install the package
   # install.packages("GGally")
  
  # Load the package
  library(GGally)
  
       ScreenTime_SPH %>%
      ggpairs(columns = c("Total.ST.min", "Social.ST.min", "Pickups", "daily_proportion_social", "daily_duration_per_use"),
              columnLabels = c("TotalScreentime", "Social Screentime", "Total Pickups", "Daily Proportion of Social Screen Time", "Daily Duration Per Use")) +
      theme_bw()
    #ScreenTime_SPH %>%
     # ggpairs(columns = c("Total.ST.min", "Social.ST.min", "Pickups", "weekdays", "if_weekend", "weekday")) +
      #theme_bw()
    
#c) Occupation Time Curve
ggplot(ScreenTime_SPH, aes(x = Date)) +
  geom_step(aes(y = Total.ST.min), color = "blue") +
  geom_step(aes(y = Social.ST.min), color = "red") +
  geom_step(aes(y = Pickups), color = "green") +
  geom_step(aes(y = daily_proportion_social * 100), color = "purple") +
  geom_step(aes(y = daily_duration_per_use), color = "orange") +
  labs(title = "Occupation Time Curve",
       y = "Values") +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Proportion")) +
  theme_minimal()

acf(ScreenTime_SPH$Total.ST.min)

acf(ScreenTime_SPH$Social.ST.min)

acf(ScreenTime_SPH$ Pickups)
acf(ScreenTime_SPH$daily_duration_per_use)
acf(ScreenTime_SPH$daily_proportion_social)


#d) Compute autocorrelations
autocorrelation_total <- acf(ScreenTime_SPH$Total.ST.min, plot = FALSE)$acf
autocorrelation_social <- acf(ScreenTime_SPH$Social.ST.min, plot = FALSE)$acf
autocorrelation_pickups <- acf(ScreenTime_SPH$Pickups, plot = FALSE)$acf
autocorrelation_proportion <- acf(ScreenTime_SPH$daily_proportion_social, plot = FALSE)$acf
autocorrelation_duration <- acf(ScreenTime_SPH$daily_duration_per_use, plot = FALSE)$acf

# Print autocorrelations
cat("Autocorrelation for Total.ST.min:", autocorrelation_total, "\n")
cat("Autocorrelation for Social.ST.min:", autocorrelation_social, "\n")
cat("Autocorrelation for Pickups:", autocorrelation_pickups, "\n")
cat("Autocorrelation for daily_proportion_social:", autocorrelation_proportion, "\n")
cat("Autocorrelation for daily_duration_per_use:", autocorrelation_duration, "\n")

#Question 3
#a)
ScreenTime_SPH <-ScreenTime_SPH%>%
library(dplyr)
library(lubridate)

# Convert Pickup.1st to datetime
ScreenTime_SPH$Pickup.1st <- mdy_hm(ScreenTime_SPH$Pickup.1st)

# Extract hour and minute
ScreenTime_SPH <- ScreenTime_SPH %>%
  mutate(hour = hour(Pickup.1st), minute = minute(Pickup.1st))

# Calculate the angle in degrees
ScreenTime_SPH <-ScreenTime_SPH %>%
 mutate(angle = (hour * 60 + minute) / 1440 * 360)

# View the resulting data frame
print(screentime[, c("Pickup.1st", "angle")])
mutate(Pickup.1st.angular=(hour(Pickup.1st)*60)+minute(Pickup.1st)/(24*60)*360)
first_pickup_cir<-circular(ScreenTime_SPH$Pickup.1st_angular,unit="degree",template="clock24")
library(lubridate)

ScreenTime_SPH$Pickup.1st <- mdy_hm(ScreenTime_SPH$Pickup.1st)

ScreenTime_SPH <- ScreenTime_SPH %>%
  mutate(
    hour = hour(Pickup.1st),
    minute = minute(Pickup.1st),
    angle = (hour * 60 + minute) / 1440 * 360
  )
print(ScreenTime_SPH[, c("Pickup.1st", "angle")])
ScreenTime_SPH <- ScreenTime_SPH[complete.cases(ScreenTime_SPH$Pickup.1st), ]
library(lubridate)

# Assuming the date-time is in a common format, let's try automatic parsing
ScreenTime_SPH$Pickup.1st <- mdy_hm(ScreenTime_SPH$Pickup.1st)

# If automatic parsing fails, and your date-time format is like "1/3/2021 7:20", you can manually parse it
# ScreenTime_SPH$Pickup.1st <- as.POSIXct(ScreenTime_SPH$Pickup.1st, format = "%m/%d/%Y %H:%M")

# Now, proceed with calculating hour, minute, and angle
ScreenTime_SPH <- ScreenTime_SPH %>%
  mutate(
    hour = hour(Pickup.1st),
    minute = minute(Pickup.1st),
    angle = (hour * 60 + minute) / 1440 * 360
  )
plot(first_pickup_cir,stack=TRUE,bins= 30,col="blue")
library(circular)

# Assuming Pickup.1st is in a proper datetime format
ScreenTime_SPH$Pickup.1st <- mdy_hm(ScreenTime_SPH$Pickup.1st)

# Calculate hour, minute, and angle
ScreenTime_SPH <- ScreenTime_SPH %>%
  mutate(
    hour = hour(Pickup.1st),
    minute = minute(Pickup.1st),
    angle = (hour * 60 + minute) / 1440 * 360
  )

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
plot(first_pickup_cir, stack = TRUE, bins = 30, col = "blue")

# Assuming Pickup.1st is in a proper datetime format
ScreenTime_SPH$Pickup.1st <- mdy_hm(ScreenTime_SPH$Pickup.1st)

# Calculate hour, minute, and angle
ScreenTime_SPH <- ScreenTime_SPH %>%
  mutate(
    hour = hour(Pickup.1st),
    minute = minute(Pickup.1st),
    angle = (hour * 60 + minute) / 1440 * 360
  )

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
hist.circular(first_pickup_cir, bins = 30, col = "blue")

# Install and load the 'circular' package
if (!requireNamespace("circular", quietly = TRUE)) {
  install.packages("circular")
}

library(circular)

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
hist.circular(first_pickup_cir, bins = 30, col = "blue")

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
hist(first_pickup_cir, breaks = 30, col = "blue")

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
rose.diag(first_pickup_cir, bins = 30, col = "blue")
library(circular)
# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Plot the circular histogram
hist.circular(first_pickup_cir, bins = 30, col = "blue")
# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Extract the angles
angles <- as.vector(first_pickup_cir)

# Create a circular histogram using base R
hist(angles, breaks = 30, col = "blue", main = "Circular Histogram")
# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Extract the angles
angles <- as.vector(first_pickup_cir)

# Plot a circular histogram using the circular package
hist.circular(angles, bins = 30, col = "blue")
#Question 4 
#a)
#b)
model<-glm(Pickups~offset(log(Total.ST.min)),data=ScreenTime_SPH,family ="poisson")
summary(model)
##call:
##glm(formula = Pickups~offset(log(Total.ST.min)),data=ScreenTime_SPH,family ="poisson")
#c)
model<-glm(Pickups~offset(log(Total.ST.min))+if_weekend,data=ScreenTime_SPH,family ="poisson")
summary(model)

# Check the levels of if_weekend
levels(ScreenTime_SPH$if_weekend)

# If it has only one level, you need to ensure it has at least two levels
# Example: Convert if_weekend to a factor with two levels
ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("level1", "level2"))

# Check levels again
levels(ScreenTime_SPH$if_weekend)

# Now, try running the glm model again
model <- glm(Pickups ~ offset(log(Total.ST.min)) + if_weekend, data = ScreenTime_SPH, family = "poisson")
# Check the unique values in if_weekend
unique(ScreenTime_SPH$if_weekend)

# Convert if_weekend to a factor with at least two levels
ScreenTime_SPH$if_weekend <- factor(ScreenTime_SPH$if_weekend, levels = c("level1", "level2"))

# Check the levels again
levels(ScreenTime_SPH$if_weekend)

# Now, try running the glm model again
model <- glm(Pickups ~ offset(log(Total.ST.min)) + if_weekend, data = ScreenTime_SPH, family = "poisson")
# Set explicit contrasts for if_weekend
contrasts(ScreenTime_SPH$if_weekend) <- contr.treatment(2)

# Now, try running the glm model again
model <- glm(Pickups ~ offset(log(Total.ST.min)) + if_weekend, data = ScreenTime_SPH, family = "poisson")
# Check the levels of if_weekend
levels(ScreenTime_SPH$if_weekend)

# If there's only one level, explicitly set the levels
levels(ScreenTime_SPH$if_weekend) <- c("level1", "level2")

# Now, try running the glm model again
model <- glm(Pickups ~ offset(log(Total.ST.min)) + if_weekend, data = ScreenTime_SPH, family = "poisson")

#Question 5
#a)
# Load the circular package
library(circular)

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Estimate parameters using mle.vonmises
vonmises_fit <- mle.vonmises(first_pickup_cir)

# Display estimated parameters
print(vonmises_fit)

# Load the circular package
library(circular)

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Check for missing values
if (any(is.na(first_pickup_cir))) {
  warning("There are missing values in the circular data.")
  # Handle missing values if needed
}

# Check if there are observations
if (length(first_pickup_cir) == 0) {
  stop("No observations in the circular data.")
}

# Estimate parameters using mle.vonmises
vonmises_fit <- mle.vonmises(first_pickup_cir)

# Check if the fit is successful
if (is.null(vonmises_fit)) {
  warning("Failed to fit the von Mises distribution.")
} else {
  # Display estimated parameters
  print(vonmises_fit)
  
  # Assuming your data has an angle for 8:30 AM
  angle_830AM <- # Calculate the angle for 8:30 AM
    
    # Check if angle_830AM is available
    if (!is.na(angle_830AM)) {
      # Calculate the probability using pvonmises
      prob_830AM_or_later <- pvonmises(angle_830AM, mu = vonmises_fit$mu, kappa = vonmises_fit$kappa)
      
      # Display the probability
      print(prob_830AM_or_later)
    } else {
      warning("Angle for 8:30 AM is not available.")
    }
}


# Load the circular package
library(circular)

# Create circular data
first_pickup_cir <- circular(ScreenTime_SPH$angle, units = "degrees")

# Check for missing values
if (any(is.na(first_pickup_cir))) {
  warning("There are missing values in the circular data.")
  # Handle missing values if needed
}

# Check if there are observations
if (length(first_pickup_cir) == 0) {
  stop("No observations in the circular data.")
}

# Try fitting von Mises distribution
tryCatch({
  # Estimate parameters using mle.vonmises
  vonmises_fit <- mle.vonmises(first_pickup_cir)
  
  # Check if the fit is successful
  if (!is.null(vonmises_fit)) {
    # Display estimated parameters
    print(vonmises_fit)
    
    # Assuming your data has an angle for 8:30 AM
    angle_830AM <- # Calculate the angle for 8:30 AM
      
      # Check if angle_830AM is available
      if (!is.na(angle_830AM)) {
        # Calculate the probability using pvonmises
        prob_830AM_or_later <- pvonmises(angle_830AM, mu = vonmises_fit$mu, kappa = vonmises_fit$kappa)
        
        # Display the probability
        print(prob_830AM_or_later)
      } else {
        warning("Angle for 8:30 AM is not available.")
      }
  } else {
    warning("Failed to fit the von Mises distribution.")
  }
}, error = function(e) {
  warning("An error occurred while fitting the von Mises distribution.")
  print(e)
})

# Check for missing values
if (any(is.na(first_pickup_cir))) {
  warning("There are missing values in the circular data.")
  # Handle missing values if needed
} else {
  # Check if there are observations
  if (length(first_pickup_cir) > 0) {
    tryCatch({
      # Estimate parameters using mle.vonmises
      vonmises_fit <- mle.vonmises(first_pickup_cir)
      
      # Check if the fit is successful
      if (!is.null(vonmises_fit)) {
        # Display estimated parameters
        print(vonmises_fit)
        
        # Assuming your data has an angle for 8:30 AM
        angle_830AM <- # Calculate the angle for 8:30 AM
          
          # Check if angle_830AM is available
          if (!is.na(angle_830AM)) {
            # Calculate the probability using pvonmises
            prob_830AM_or_later <- pvonmises(angle_830AM, mu = vonmises_fit$mu, kappa = vonmises_fit$kappa)
            
            # Display the probability
            print(prob_830AM_or_later)
          } else {
            warning("Angle for 8:30 AM is not available.")
          }
      } else {
        warning("Failed to fit the von Mises distribution.")
      }
    }, error = function(e) {
      warning("An error occurred while fitting the von Mises distribution.")
      print(e)
    })
  } else {
    warning("No valid observations in the circular data.")
  }
}

# Remove missing values
first_pickup_cir <- na.omit(first_pickup_cir)
# Remove missing values
first_pickup_cir <- na.omit(first_pickup_cir)

# Check if there are observations after removing missing values
if (length(first_pickup_cir) > 0) {
  # Proceed with the analysis
  vonmises_fit <- mle.vonmises(first_pickup_cir)
  # ... (rest of the analysis)
} else {
  warning("No valid observations in the circular data after removing missing values.")
}
est_cir=mle.vonmises(first_pickup_cir-180)
est_cir_mu=est_cir$mu
est_cir_kappa=est_cir$kappa
first_pickup_rad=circular((first_pickup_cir)*pi/180-pi, units="radians")
est_rad=mle.vonmises(first_pickup_rad)
est_rad_mu=est_rad$mu
est_rad_kappa=est_rad$kappa

# Remove missing values
first_pickup_cir <- na.omit(first_pickup_cir)

# Check if there are observations after removing missing values
if (length(first_pickup_cir) > 0) {
  # Center the circular data around 0 by subtracting 180
  centered_cir <- first_pickup_cir - 180
  
  # Fit the von Mises distribution
  vonmises_fit <- mle.vonmises(centered_cir)
  
  # Display estimated parameters
  print(vonmises_fit)
  
  # ... (rest of the analysis)
} else {
  warning("No valid observations in the circular data after removing missing values.")
}

# Assuming your data has an angle for 8:30 AM
angle_830AM <- # Calculate the angle for 8:30 AM
  
  # Calculate the probability using pvonmises
  prob_830AM_or_later <- pvonmises(angle_830AM, mu = vonmises_fit$mu, kappa = vonmises_fit$kappa)

# Display the probability
print(prob_830AM_or_later)
#b)
nine_ang=(9*60)/(24*60)*360
nine_rad=(-nine_ang*(pi/180))-pi
1-pvonmises(circular(nine_rad),mu=est_rad_mu,kappa=est_rad_kappa)

# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa

# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- (-nine_ang * (pi/180)) - pi

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(circular(nine_rad), mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)

# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa
# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- (-nine_ang * (pi/180)) - pi

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(nine_rad, mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)
# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa

# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- -nine_ang * (pi / 180)

# Create a circular object
circ_nine_rad <- circular(nine_rad)

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(circ_nine_rad, mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)
# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa

# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- -nine_ang * (pi / 180)

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(nine_rad, mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)
# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa

# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- -nine_ang * (pi / 180)

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(nine_rad, mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)
# Assuming you have estimates for mu and kappa
est_mu <- vonmises_fit$mu
est_kappa <- vonmises_fit$kappa

# Calculate the circular representation of 8:30 AM
nine_ang <- (9 * 60) / (24 * 60) * 360
nine_rad <- -nine_ang * (pi / 180)

# Calculate the probability
prob_8_30_or_later <- 1 - pvonmises(nine_rad, mu = est_mu, kappa = est_kappa)

# Display the result
print(prob_8_30_or_later)

# Setting angular frequency
omega <- 0.1

# Generating time values from 1 to 100 with a step size of 0.1
time <- seq(1, 100, 0.1)

# Calculating sine values
sine_values <- sin(omega * time)

# Plotting the sine wave
plot(time, sine_values, type = "l", col = "blue", xlab = "Time", ylab = "Sine Value", main = "Sine Wave Function")
# Calculating linear frequency
linear_frequency <- omega / (2 * pi)

# Calculating period
period <- 1 / linear_frequency

# Displaying results
cat("Linear Frequency:", linear_frequency, "\n")
cat("Period:", period, "\n")

fulfillment