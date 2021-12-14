
library(ggplot2)
library(xtable)
library(dplyr)
library(tidyverse)
library(car)
library(pROC)

car <- read.csv("C:/MIDS/Program/IDS702/vehicles.csv", header = T,
                colClasses=c("numeric","factor","factor","factor","numeric","numeric","factor","factor",
                             "factor","factor","factor","numeric","factor","factor","factor","factor",
                             "factor","factor","factor","factor","factor","factor","factor","numeric",
                             "numeric","factor"))

saveRDS(car, "C:/MIDS/Program/IDS702/car_craiglist")

car$url <- NULL
car$lat <- NULL
car$long <- NULL
car$region <- NULL
car$region_url <- NULL
car$image_url <- NULL
# there is no data in county
car$county <- NULL
car$description <- NULL
car$posting_date <- NULL

saveRDS(car, "C:/MIDS/Program/IDS702/car_remove_columns")
car <- readRDS("C:/MIDS/Program/IDS702/car_remove_columns")

car <- car[!duplicated(car$VIN),]

str(car)
dim(car)
summary(car)


############# Data Preparation
car$id <- NULL
car$model <- NULL
car$drive <- NULL
car$size <- NULL
car$VIN <- NULL
car$title_status <- NULL

car$price <- ifelse(car$price < 5000, NA, car$price)
car$price <- ifelse(car$price > 35000, NA, car$price)
car$year <- ifelse(car$year < 1990, NA, car$year)
car$odometer <- ifelse(car$odometer < 25000, NA, car$odometer)
car$odometer <- ifelse(car$odometer > 300000, NA, car$odometer)
#car$VIN <- ifelse(car$VIN == "", NA, as.character(car$VIN))
#car$VIN <- factor(car$VIN) 
#car$model <- ifelse(car$model == "", NA, as.character(car$model)) 
car$condition <- ifelse(car$condition == "", NA, as.character(car$condition)) 
car$condition <- factor(car$condition) 
car$cylinders <- ifelse(car$cylinders == "", NA, as.character(car$cylinders)) 
car$cylinders <- factor(car$cylinders) 
car$fuel <- ifelse(car$fuel == "", NA, as.character(car$fuel)) 
car$fuel <- factor(car$fuel) 
#car$title_status <- ifelse(car$title_status == "", NA, as.character(car$title_status))  
#car$title_status <- factor(car$title_status) 
car$transmission <- ifelse(car$transmission == "", NA, as.character(car$transmission)) 
car$transmission <- factor(car$transmission) 
#car$drive <- ifelse(car$drive == "", NA, as.character(car$drive)) 
#car$size <- ifelse(car$size == "", NA, as.character(car$size)) 
car$type <- ifelse(car$type == "", NA, as.character(car$type))  
car$type <- factor(car$type) 
car$paint_color <- ifelse(car$paint_color == "", NA, as.character(car$paint_color))  
car$paint_color <- factor(car$paint_color) 

table(is.na(car))
table(is.na(car$price))
table(is.na(car$year))

car_filtered <- na.omit(car)
summary(car_filtered)

write.csv(car_filtered, file="C:/MIDS/Program/IDS702/vehicles_modified.csv", row.names = FALSE)


############# EDA
# Is the distribution of the response variable normal?
hist(car_filtered$price)
ggplot(car_filtered, aes(x=price)) +
  geom_histogram(aes(y = ..density..), color = "black", linetype = "dashed", fill = "lightblue",bins=20) +
  geom_density(alpha = .25, fill = "lightblue") + scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of used car price", x="price") + 
  theme_classic() + theme(legend.position="none")
ggplot(car_filtered, aes(x=log(price))) +
  geom_histogram(aes(y = ..density..), color = "black", linetype = "dashed", fill = "lightblue",bins=20) +
  geom_density(alpha = .25, fill = "lightblue") + scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of used car price", x="log Price") + 
  theme_classic() + theme(legend.position="none")


# continuous variables
# year
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha= .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year",x="Year",y="Log Price")

# odometer
ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha= .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer",x="Odometer",y="Log Price")


# categorical variables
# manufacturer
ggplot(car_filtered, aes(x=manufacturer, y=log(price), fill=manufacturer)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Manufacturer",x="Manufacturer",y="Log Price") + theme_classic() + theme(legend.position="none")

# condition
ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition",x="Condition",y="Log Price") + theme_classic() + theme(legend.position="none")

# cylinders
ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders",x="Cylinders",y="Log Price") + theme_classic() + theme(legend.position="none")

# fuel
ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Fuel",x="Fuel",y="Log Price") + theme_classic() + theme(legend.position="none")

# status
#ggplot(car_filtered, aes(x=title_status, y=log(price), fill=title_status)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Status",x="Status",y="Log Price") + theme_classic() + theme(legend.position="none")

# transmission
ggplot(car_filtered, aes(x=transmission, y=log(price), fill=transmission)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Transmission",x="Transmission",y="Log Price") + theme_classic() + theme(legend.position="none")

# type
ggplot(car_filtered, aes(x=type, y=log(price), fill=type)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Type",x="Type",y="Log Price") + theme_classic() + theme(legend.position="none")

# color
ggplot(car_filtered, aes(x=paint_color, y=log(price), fill=paint_color)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Color",x="Color",y="Log Price") + theme_classic() + theme(legend.position="none")

# state
ggplot(car_filtered, aes(x=state, y=log(price), fill=state)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs State",x="State",y="Log Price") + theme_classic() + theme(legend.position="none")


# Interactions
# Use the log(price) going forward
# Manufacturer and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Manufacturer",x="Year",y="Log Price") + facet_wrap( ~ manufacturer, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Manufacturer",x="Odometer",y="Log Price") + facet_wrap( ~ manufacturer, ncol=4)

# Condition and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Condition",x="Year",y="Log Price") + facet_wrap( ~ condition, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Condition",x="Odometer",y="Log Price") + facet_wrap( ~ condition, ncol=4)

# Cylinders and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Cylinders",x="Year",y="Log Price") + facet_wrap( ~ cylinders, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Cylinders",x="Odometer",y="Log Price") + facet_wrap( ~ cylinders, ncol=4)

# Fuel and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Fuel",x="Year",y="Log Price") + facet_wrap( ~ fuel, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Fuel",x="Odometer",y="Log Price") + facet_wrap( ~ fuel, ncol=4)

# Status and discrete/continuous predictors
#ggplot(car_filtered, aes(x=year, y=log(price))) +
#  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
#  labs(title="Log Price vs Year by Status",x="Year",y="Log Price") + facet_wrap( ~ title_status, ncol=4)

#ggplot(car_filtered, aes(x=odometer, y=log(price))) +
#  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
#  labs(title="Log Price vs Odometer by Status",x="Odometer",y="Log Price") + facet_wrap( ~ title_status, ncol=4)

# Transmission and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Transmission",x="Year",y="Log Price") + facet_wrap( ~ transmission, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Transmission",x="Odometer",y="Log Price") + facet_wrap( ~ transmission, ncol=4)

# Type and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Type",x="Year",y="Log Price") + facet_wrap( ~ type, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Type",x="Odometer",y="Log Price") + facet_wrap( ~ type, ncol=4)

# Color and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by Color",x="Year",y="Log Price") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by Color",x="Odometer",y="Log Price") + facet_wrap( ~ paint_color, ncol=4)

# State and discrete/continuous predictors
ggplot(car_filtered, aes(x=year, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Year by State",x="Year",y="Log Price") + facet_wrap( ~ state, ncol=4)

ggplot(car_filtered, aes(x=odometer, y=log(price))) +
  geom_point(alpha = .5,colour="blue4") + geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="Log Price vs Odometer by State",x="Odometer",y="Log Price") + facet_wrap( ~ state, ncol=4)


# Interactions between categorical variables
# Condition 
ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by Cylinders",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ cylinders, ncol=4)

ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by Fuel",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ fuel, ncol=4)

#ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Condition by Status",x="Condition",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ title_status, ncol=4)

ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by Transmission",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ transmission, ncol=4)

ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by Type",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ type, ncol=4)

ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by Color",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=condition, y=log(price), fill=condition)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Condition by State",x="Condition",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Cylinders 
ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders by Fuel",x="Cylinders",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ fuel, ncol=4)

#ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Cylinders by Status",x="Cylinders",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ title_status, ncol=4)

ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders by Transmission",x="Cylinders",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ transmission, ncol=4)

ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders by Type",x="Cylinders",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ type, ncol=4)

ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders by Color",x="Cylinders",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=cylinders, y=log(price), fill=cylinders)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Cylinders by State",x="Cylinders",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Fuel 
#ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Fuel by Status",x="Fuel",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ title_status, ncol=4)

ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Fuel by Transmission",x="Fuel",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ transmission, ncol=4)

ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Fuel by Type",x="Fuel",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ type, ncol=4)

ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Fuel by Color",x="Fuel",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=fuel, y=log(price), fill=fuel)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Fuel by State",x="Fuel",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Status 
#ggplot(car_filtered, aes(x=title_status, y=log(price), fill=title_status)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Status by Transmission",x="Status",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ transmission, ncol=4)

#ggplot(car_filtered, aes(x=title_status, y=log(price), fill=title_status)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Status by Type",x="Status",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ type, ncol=4)

#ggplot(car_filtered, aes(x=title_status, y=log(price), fill=title_status)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Status by Color",x="Status",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

#ggplot(car_filtered, aes(x=title_status, y=log(price), fill=title_status)) +
#  geom_boxplot() + scale_fill_brewer(palette="Blues") +
#  labs(title="Log Price vs Status by State",x="Status",y="Log Price") +
#  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Transmission 
ggplot(car_filtered, aes(x=transmission, y=log(price), fill=transmission)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Transmission by Type",x="Transmission",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ type, ncol=4)

ggplot(car_filtered, aes(x=transmission, y=log(price), fill=transmission)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Transmission by Color",x="Transmission",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=transmission, y=log(price), fill=transmission)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Transmission by State",x="Transmission",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Type
ggplot(car_filtered, aes(x=type, y=log(price), fill=type)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Type by Color",x="Type",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ paint_color, ncol=4)

ggplot(car_filtered, aes(x=type, y=log(price), fill=type)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Type by State",x="Type",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)

# Color
ggplot(car_filtered, aes(x=paint_color, y=log(price), fill=paint_color)) +
  geom_boxplot() + scale_fill_brewer(palette="Blues") +
  labs(title="Log Price vs Color by State",x="Type",y="Log Price") +
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ state, ncol=4)


############# Modeling
# Mean center continuous variables
car_filtered$yearc <- car_filtered$year - mean(car_filtered$year)
car_filtered$odometerc <- car_filtered$odometer - mean(car_filtered$odometer)

# Fit first model with all predictors and interactions
model1 <- lm(log(price) ~ yearc + manufacturer + condition + cylinders + fuel + odometerc #+ title_status
             + transmission + type + paint_color + state + yearc*manufacturer #+ yearc*type 
             + odometerc*fuel + odometerc*cylinders + odometerc*type, data = car_filtered)
summary(model1)

# Model Assessment
plot(model1)
ggplot(car_filtered, aes(x = yearc, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_filtered, aes(x = odometerc, y = model1$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

# Fit first model with all predictors
model0 <- lm(log(price) ~ yearc + manufacturer + condition + cylinders + fuel + odometerc #+ title_status
             + transmission + type + paint_color + state, data = car_filtered)
summary(model0)

# Model Assessment
plot(model0)
ggplot(car_filtered, aes(x = yearc, y = model0$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_filtered, aes(x = odometerc, y = model0$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

AIC(model0, model1)

# Add square term for year
car_filtered$year2 <- car_filtered$yearc^2
model2 <-lm(log(price) ~ yearc + year2 + manufacturer + condition + cylinders + fuel + odometerc #+ title_status
            + transmission + type + paint_color + state + yearc*manufacturer #+ yearc*type 
            + odometerc*fuel + odometerc*cylinders + odometerc*type, data = car_filtered)
summary(model2)

# Model Assessment
plot(model2)
ggplot(car_filtered, aes(x = yearc, y = model2$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_filtered, aes(x = odometerc, y = model2$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

AIC(model1, model2)

# Add quad term for year
car_filtered$year3 <- car_filtered$yearc^3
model3 <-lm(log(price) ~ yearc + year2 + year3 + manufacturer + condition + cylinders + fuel + odometerc 
            #+ title_status 
            + + transmission + type + paint_color + state + yearc*manufacturer #+ yearc*type 
            + odometerc*fuel + odometerc*cylinders + odometerc*type, data = car_filtered)
summary(model3)

# Model Assessment
plot(model3)
ggplot(car_filtered, aes(x = yearc, y = model3$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_filtered, aes(x = odometerc, y = model3$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

AIC(model2, model3)

#out = c(8073, 8667, 26144) no trinket
#out = c(3890, 5043, 5279, 7507) trinket to 50000
#out = c(3273, 4282, 4491, 4794, 6100, 6466) trinket to 35000
out = c(5784, 180721) #trinket 5000~35000
car_removed = car_filtered[-out, ]

model4 <-lm(log(price) ~ yearc + year2 
            + year3 
            + manufacturer + condition + cylinders + fuel + odometerc 
            #+ title_status 
            + transmission + type + paint_color + state + yearc*manufacturer #+ yearc*type 
            + odometerc*fuel + odometerc*cylinders + odometerc*type, data = car_removed)
summary(model4)

# Model Assessment
plot(model4)
ggplot(car_removed, aes(x = yearc, y = model4$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_removed, aes(x = odometerc, y = model4$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

AIC(model3, model4)

model5 <-lm(log(price) ~ yearc + year2 + year3 + condition + cylinders + fuel + odometerc 
            #+ title_status 
            + transmission + paint_color + state + yearc*manufacturer #+ yearc*type 
            + odometerc*fuel + odometerc*cylinders + odometerc*type, data = car_removed)

AIC(model4, model5)

is.na(model4)

nullmodel <- lm(log(price)~yearc, data = car_removed)
n <- nrow(car_removed)

aic_step <- step(nullmodel, scope = list(upper = model4, lower = nullmodel), direction = "both", trace = 0)
aic_for <- step(nullmodel, scope = list(upper = model4, lower = nullmodel), direction = "forward", trace = 0)
aic_back <- step(model4, scope = list(upper = model4, lower = nullmodel), direction = "both", trace = 0)
bic_step <- step(nullmodel, scope = list(upper = model4, lower = nullmodel), direction = "both", trace = 0, k = log(n))
bic_for <- step(nullmodel, scope = list(upper = model4, lower = nullmodel), direction = "forward", trace = 0, k = log(n))
bic_back <- step(model4, scope = list(upper = model4, lower = nullmodel), direction = "both", trace = 0, k = log(n))

saveRDS(aic_step, "C:/MIDS/Program/IDS702/aic_step1")
saveRDS(aic_for, "C:/MIDS/Program/IDS702/aic_for1")
saveRDS(aic_back, "C:/MIDS/Program/IDS702/aic_back1")
saveRDS(bic_step, "C:/MIDS/Program/IDS702/bic_step1")
saveRDS(bic_for, "C:/MIDS/Program/IDS702/bic_for1")
saveRDS(bic_back, "C:/MIDS/Program/IDS702/bic_back1")

aic_step <- readRDS("C:/MIDS/Program/IDS702/aic_step1")
aic_for <- readRDS("C:/MIDS/Program/IDS702/aic_for1")
aic_back <- readRDS("C:/MIDS/Program/IDS702/aic_back1")
bic_step <- readRDS("C:/MIDS/Program/IDS702/bic_step1")
bic_for <- readRDS("C:/MIDS/Program/IDS702/bic_for1")
bic_back <- readRDS("C:/MIDS/Program/IDS702/bic_back1")

anova(aic_back, aic_for)
AIC(aic_back, aic_for)
AIC(aic_back, aic_step)
AIC(aic_back, bic_back)
AIC(aic_back, bic_for)
AIC(aic_back, bic_step)
anova(aic_back, bic_step)

AIC(aic_step, bic_step)

plot(aic_step)
ggplot(car_removed, aes(x = yearc, y = aic_step$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red") + theme_classic() + 
  labs(title="Residuals vs Year",x="Year",y="Residuals")

ggplot(car_removed, aes(x = odometerc, y = aic_step$residuals)) + geom_point(alpha = .7) + 
  geom_hline(yintercept=0,col="red3") + theme_classic() + 
  labs(title="Residuals vs Odometer",x="Odometer",y="Residuals")

summary(aic_step)
#vif(aic_step)














