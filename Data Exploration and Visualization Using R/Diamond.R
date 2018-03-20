##****Diamond****##
pacman::p_load('caTools','readxl','XLConnect','magrittr','dplyr','plotly', 'tidyverse','data.table')

d = read.csv(file.choose(), stringsAsFactors = FALSE)



############################
#1 Which is the best color for a Diamond?
head(levels(d$color),1)                                      


############################
##2 Which cut of Diamond is expensive?
d <- as.data.frame(d)
str(d)
#sliced dataset as per cut
slcd_d <- as.data.table(d[c("cut","price")])

#plot price as per cut
slcd_d %>%
  group_by(cut) %>%
  summarise(sum_price=sum(price), count_cut=length(cut),
            avg_price = (sum_price/count_cut)) %>%
  ggplot(aes(x=cut, y=avg_price))+
  geom_bar(stat='identity',color="royalblue4", fill="steelblue3")+
  geom_text(aes(label=round(avg_price,2)), vjust=2)+
  theme_minimal()+
  xlab("Cut")+ylab("Avg Price in $")                     
  

###############################
##3 How many diamonds cost less than $1200 ? show as per distribution of price.

count_less_12k <- d %>%
  filter(price<1200) %>%
  summarise(count_less_12k=n()); count_less_12k


#histogram plot view
d_price <-d$price
str(d)
names(d_price) <- c("price")
counts_less<- as.data.frame(d_price< 1200)

hst<-
    ggplot(d, aes(price, fill=counts_less)) + 
  geom_histogram(color = "thistle1", binwidth = 342) + 
    scale_x_continuous(breaks=seq(0,19000,1200))+
  scale_fill_manual(values = c("mediumorchid1", "orchid4")) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill="# of Diamonds priced less than $1200")+
  xlab("Price in $") + ylab("Count of Diamonds"); hst



############################
#4 Analyze the price per carat of diamonds for different colors
d %>%
  ggplot(aes(x = color, y = (price/carat), fill = color)) +
  geom_boxplot() +
  coord_cartesian(ylim=c(300, 19000)) +
  scale_y_continuous(breaks=seq(300,19000,1000)) + 
  xlab("Color") + ylab("Price per Carat")


############################
#5 How many each category of Premium and Ideal diamonds has the best clarity


ideal=d %>%
  filter(cut=="Ideal") %>%
  summarise(clarity_count=length(clarity==head(levels(clarity),1)))

premium=d %>%
  filter(cut=="Premium") %>%
  summarise(clarity_count=length(clarity==head(levels(clarity),1)))

cat(paste("No. of ideal diamonds with best clarity: ", ideal, '\n',"No. of Premium diamonds with best clarity: " ,premium) )



############################
#6 # Show various cut Diamonds of all the clarity levels from chaepest to the most expensive


ggplot(d, aes(x = clarity, y = price, color = cut)) + 
  geom_boxplot() + 
  facet_grid(color~., margins = TRUE) 


############################
#7 Find the Largest diamond and its corresponding Price. Which cut does this Diamond belong to?
str(d)
d$sizes <- with(d, x * y * z)

ggplot(data=d, aes(x=price, y=sizes)) +
  geom_point(aes(size=sizes, color=cut )) +
  scale_y_continuous(breaks=seq(0,4000,300))+
  scale_x_continuous(breaks=seq(0,19000,1225))+
  theme(axis.text.x = element_text(angle = 90)) +
  scale_size_continuous(range=c(1,5))

############################