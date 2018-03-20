# EB5101      : Foundation of Business Analytics
# Assignment  : Data Preparation
# Team Name   : AbracaDATA
# Team Members: Bhabesh Senapati
#               Dibyajyoti Panda
#               Diksha Kumari Jha
#               Gopesh Dwivedi
#               Sravani Satpathy

# Using pacman to install and load required libraries in current session
install.packages("pacman")
pacman::p_load('ggfortify','rAverage','chron','grid','scales','caTools','reshape2', 'zoo', 'readxl','XLConnect','magrittr','dplyr','plyr','plotly', 'tidyverse','data.table','lattice','lubridate', 'TTR', 'imputeTS')


# setting Working directory to current source file path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############### - Data Preparation Begins - #######################################

# reading assigment dataset files, assuming them to be in current working directory
march.data <- read.csv("Assignment_Data.csv")
april.data <- read.csv("Assignment_Data2.csv")

# taking backup of original dataset for comparison
bkp.march <- march.data  # backup for march
bkp.april <- april.data  # backup for april

# Structure, summary and head of imported datasets
str(march.data)
summary(march.data)
head(march.data)
str(april.data)
summary(april.data)
head(april.data)

# correcting data type of "date_time" from factors to data-time and rounding off seconds to 0
#march.data$date_time <- round(as.POSIXct(march.data$date_time), units = "mins")
#april.data$date_time <- round(as.POSIXct(april.data$date_time), units = "mins")

# Combining data from both months for Missing Value Treatment
full.data <- rbind(march.data,april.data)
str(full.data)
summary(full.data)
rownames(full.data) <- NULL


# Inserting Missing Timestamps in the Full data set
full.data$date_time <- as.POSIXct(full.data$date_time, format="%Y-%m-%d %H:%M") # correcting data type of data_time
full.zoo <- zoo(full.data[, -1],full.data[, 1])   #set date_time to Index
final.data <- merge(full.zoo, zoo(, seq(start(full.zoo), end(full.zoo), by = "min")), all = TRUE) # merge complete zoo object with original dataset
final.data <- fortify.zoo(final.data) # convert zoo back to dataframe
colnames(final.data)[1] <- "date_time"
cols = c(3, 4, 5, 6, 7, 8)
final.data[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
str(final.data)
summary(final.data)

# Ploting Box Plots
boxplot(final.data$Temperature)
boxplot(final.data$Noise)
boxplot(final.data$Light)
boxplot(final.data$Co2)
boxplot(final.data$VOC)
boxplot(final.data$Humidity)

# Replace outliers with missing values with Z Score Method
outliersZ <- function(data, zCutOff = 1.96, replace = NA, values = FALSE, digits = 2) {
  #compute standard deviation (sample version n = n [not n-1])
  stdev <- sqrt(sum((data - mean(data, na.rm = T))^2, na.rm = T) / sum(!is.na(data)))
  #compute absolute z values for each value
  absZ <- abs(data - mean(data, na.rm = T)) / stdev
  #subset data that has absZ greater than the zCutOff and replace them with replace
  #can also replace with other values (such as max/mean of data)
  data[absZ > zCutOff] <- replace 
  
  if (values == TRUE) {
    return(round(absZ, digits)) #if values == TRUE, return z score for each value
  } else {
    return(round(data, digits)) #otherwise, return values with outliers replaced
  }
}

summary(final.data)
final.data$Temperature <- outliersZ(final.data$Temperature)
final.data$Noise <- outliersZ(final.data$Noise)
final.data$Light <- outliersZ(final.data$Light)
final.data$Co2 <- outliersZ(final.data$Co2)
final.data$VOC <- outliersZ(final.data$VOC)
final.data$Humidity <- outliersZ(final.data$Humidity)
summary(final.data)


# Checking missing values
missing <- sapply(final.data, function(y) sum(length(which(is.na(y)))))
missing.data <- final.data[rowSums(is.na(final.data)) > 0,]

s1 <- subset(missing.data,unitid == "SS0029")
s2 <- subset(missing.data,unitid == "SS0031")
s3 <- subset(missing.data,unitid == "SS0036")
s4 <- subset(missing.data,unitid == "SS0050")

summary(s1)
summary(s2)
summary(s3)
summary(s4)


# Recoding unitid for further analysis
final.data$unitid <- as.character(final.data$unitid)
final.data$unitid[final.data$unitid == "SS0029"] <- 1
final.data$unitid[final.data$unitid == "SS0031"] <- 2
final.data$unitid[final.data$unitid == "SS0036"] <- 3
final.data$unitid[final.data$unitid == "SS0050"] <- 4
final.data$unitid <- as.numeric(final.data$unitid)
summary(final.data)

# Missing Value Imputation for various Attributes by interpolation using na.approx from zoo package
temp.zoo <- zoo(final.data)  # creating separate copy 
temp.zoo$unitid <- na.approx(temp.zoo$unitid)
temp.zoo$Temperature <- na.approx(temp.zoo$Temperature)
temp.zoo$Noise <- na.approx(temp.zoo$Noise)
temp.zoo$Light <- na.approx(temp.zoo$Light)
temp.zoo$Co2 <- na.approx(temp.zoo$Co2)
temp.zoo$VOC <- na.approx(temp.zoo$VOC)
temp.zoo$Humidity <- na.approx(temp.zoo$Humidity)
temp.zoo <- fortify.zoo(temp.zoo)
cols = c(3, 4, 5, 6, 7, 8, 9)
temp.zoo[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
imputed.data <- temp.zoo[,c(2,3,4,5,6,7,8,9)]
imputed.data$unitid <- ceiling(as.numeric(imputed.data$unitid))

# Converting unitid back to factors
imputed.data$unitid = as.character(imputed.data$unitid)
imputed.data$unitid[imputed.data$unitid == "1"] <- "SS0029"
imputed.data$unitid[imputed.data$unitid == "2"] <- "SS0031"
imputed.data$unitid[imputed.data$unitid == "3"] <- "SS0036"
imputed.data$unitid[imputed.data$unitid == "4"] <- "SS0050"
imputed.data$unitid = as.factor(imputed.data$unitid)
summary(imputed.data)

# Ploting TS Data just for analysing pattern(s) and noise in the data 
imputed.ts <- ts(imputed.data)
plot.ts(imputed.ts)

# Smoothing the data to remove abnormality and irregular components using Simple Moving Averages
sm.imputed <- imputed.data
sm.imputed$Temperature <- SMA(imputed.data$Temperature, n=400)
sm.imputed$Noise <- SMA(imputed.data$Noise, n=400)
sm.imputed$Light <- SMA(imputed.data$Light, n=200)
sm.imputed$Co2 <- SMA(imputed.data$Co2, n=400)
sm.imputed$VOC <- SMA(imputed.data$VOC, n=400)
sm.imputed$Humidity <- SMA(imputed.data$Humidity, n=400)
plot.ts(sm.imputed)
sm.imputed <- sm.imputed[complete.cases(sm.imputed),] # Removing First 400 NA due to Smoothing
summary(sm.imputed)

# Checking for Duplicates
anyDuplicated(sm.imputed)

################## - Data Preparation End - ############################

################## - Data Exploration Begins - #########################

# Applying Transformation and Extracting New Features

sm.imputed$hour <- format(as.POSIXct(strptime(sm.imputed$date_time,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M")
sm.imputed$date <- format(as.POSIXct(strptime(sm.imputed$date_time,"%Y-%m-%d %H:%M",tz="")) ,format = "%Y-%m-%d")
sm.imputed$month <- month(sm.imputed$date_time)
sm.imputed$weekday <- wday(sm.imputed$date_time)
summary(sm.imputed)

d <- sm.imputed

#Conditional Levels for EAQ
d$EAQ[(d$Temperature>=20 & d$Temperature<=25) |(d$Noise == 60) | (d$Light>=250 & d$Light<=300)| (d$Co2>=350 & d$Co2<=1000) | (d$Humidity>=45 & d$Humidity<=55)] ='Ideal'
d$EAQ[(d$Temperature>=15 & d$Temperature<20) | (d$Temperature>25 & d$Temperature<=30) | (d$Noise>=55 & d$Noise<60) | (d$Noise>60 & d$Noise<=80) |
            (d$Light>300 & d$Light<=500) | (d$Co2>1000 & d$Co2<=2000) | (d$Humidity>=30 & d$Humidity<45) | (d$Humidity>55 & d$Humidity<=65)] ='Good'
d$EAQ[(d$Temperature>30) | (d$Noise>80) | (d$Co2>2000)| (d$Humidity>65)] ="Actionable"

sm.imputed <- d
summary(sm.imputed)
str(sm.imputed)

d_plot<- na.omit(sm.imputed)

d_plot%>%
  group_by(EAQ) %>%
  summarise(count_EAQ =length(EAQ)) %>%
  ggplot(aes(x=EAQ, y=count_EAQ)) +
  geom_bar(stat='identity', color="orchid2", fill="orchid")+
  labs(x="EAQ", y="Count of EAQs")+ geom_text(aes(label=(count_EAQ), vjust=2))

# Spliting Imputed Data into march and april data
march <- subset(sm.imputed, month=="March")
april <- subset(sm.imputed, month=="April")

# checking Correlation
corr <- cor(imputed.data[sapply(imputed.data, is.numeric)])

# Checking Spectral Density and Lag Plot
spectrum(imputed.ts)
lag.plot(imputed.ts)

write.csv(sm.imputed,"final_imputed.csv")

# PCA on the correlated data

num.data <- imputed.data[c(3:8)]
summary(num.data)
data.pca <- prcomp(num.data,
                 center = TRUE,
                 scale. = TRUE) 
summary(data.pca)
print(data.pca)
biplot(data.pca, scale=0, loadings.label = TRUE)
autoplot(data.pca, label = TRUE, label.size=3, loadings = TRUE, loadings.label = TRUE)
# Visualizations and Trend Graphs

# Light Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Light))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")
plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=Light))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

# Temperature Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Temperature))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")

plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=Temperature))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

# Noise Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Noise))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")

plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=Noise))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

# VOC Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=VOC))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")

plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=VOC))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

# CO2 Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Co2))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")

plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=Co2))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)

# Humidity Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Humidity))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")

plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-04-01"):as.POSIXct("2017-04-29"))),
                  aes(x=as.POSIXct(date_time), y=Humidity))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-04-01 00:00:04"),
                                 as.POSIXct("2017-04-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-04-01 00:00:00"),
                      as.POSIXct("2017-04-29 00:00:00")
                    )) +
  labs(x="April"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)


# Light and Temperature Trends
plot1_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-30"))) ,
                  aes(x=as.POSIXct(date_time), y=Temperature))+
  geom_line(color='steelblue')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:02"),
                                 as.POSIXct("2017-03-30 23:59:00 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-30 00:00:00")
                    ))+
  labs(x="March")
plot2_L <- ggplot(data=subset(sm.imputed, date_time=c(as.POSIXct("2017-03-01"):as.POSIXct("2017-03-29"))),
                  aes(x=as.POSIXct(date_time), y=Light))+
  geom_line(color='darkgoldenrod1')+
  theme(text=element_text(size=8),
        legend.text=element_text(size=8), 
        axis.text=element_text(size=8, colour="black"),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.y = element_text(size=8, vjust = 0.3), 
        axis.title.x = element_text(size=8, vjust = 0),
        legend.key=element_rect(fill="white")) +
  scale_x_datetime( breaks = seq(as.POSIXct("2017-03-01 00:00:04"),
                                 as.POSIXct("2017-03-29 23:59:08 "), "24 hours"), 
                    labels = date_format("%a-%d\n%m\n%H:%M"), 
                    expand = c(0,0),
                    limits = c(
                      as.POSIXct("2017-03-01 00:00:00"),
                      as.POSIXct("2017-03-29 00:00:00")
                    )) +
  labs(x="March"); 

gb1 <- ggplot_build(plot1_L)
gb2 <- ggplot_build(plot2_L)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
grid.newpage()
grid.draw(g)