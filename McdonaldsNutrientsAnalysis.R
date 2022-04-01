#packages Required
library(tidyverse)
library(dplyr)
library(plotly)


#1
data.csv<-read.csv("C:\\Users\\rockm\\Downloads\menu.csv", header = TRUE)
breakfast_temp<-data.frame()



#finding the contribution % of breakfast in total calories
breakfast_temp<-data.frame()
breakfast_temp = data.csv %>% filter(Category == "Breakfast")
bf = sum(breakfast_temp$Calories)
total = sum(data.csv$Calories)
percent_bf = (bf) * 100 / (total)
percent_bf

#finding the contribution % of Beverages in total calories
beverages_temp<-data.frame()
beverages_temp = data.csv %>% filter(Category == "Beverages")
bev = sum(beverages_temp$Calories)
percent_bev = (bev) * 100 / (total)

#finding the contribution % of Beef and Pork in total calories
beef_pork_temp<-data.frame()
beef_pork_temp = data.csv %>% filter(Category == "Beef & Pork")
bp = sum(beef_pork_temp$Calories)
percent_bp = (bp) * 100 / (total)

#finding the contribution % of Coffee and Tea in total calories
coffee_temp<-data.frame()
coffee_temp = data.csv %>% filter(Category == "Coffee & Tea")
cof = sum(coffee_temp$Calories)
percent_coffee = (cof) * 100 / (total)


#finding the contribution % of Smoothies and shakes in total calories
smoothies_temp<-data.frame()
smoothies_temp = data.csv %>% filter(Category == "Smoothies % Shakes")
smo = sum(smoothies_temp$Calories)
percent_smoothies = (smo) * 100 / (total)



#finding the contribution % of Salads in total calories
salads_temp <-data.frame()
salads_temp = data.csv %>% filter(Category == "Salads")
salad = sum(salads_temp$Calories)
percent_salads = (salad) * 100 / (total)


#finding the contribution % of Snacks and Sides in total calories
snacks_temp<-data.frame()
snacks_temp = data.csv %>% filter(Category == "Snacks & Sides")
snack_sides = sum(snacks_temp$Calories)
percent_snack_sides = (snack_sides) * 100 / (total)


#finding the contribution % of Chicken and fish in total calories
chicken_fish_temp<-data.frame()
chicken_fish_temp = data.csv %>% filter(Category == "Chicken & Fish")
cf = sum(chicken_fish_temp$Calories)
percent_cf = (cf) * 100 / (total)


#Plotting a pie chart for the contribution of each categories in the field of calories
x<-c(bf, bev,  bp, cof, smo,salad, snack_sides, cf)
labels<-c("Breakfast", "Beverages", "Beef & Pork", "Coffee", "Smoothie", "Salads", "Snacks&Sides", "Chicken&Fish")
color<-c("Blue", "Yellow", "Red", "White", "Green", "Black", "Orange", "Brown")
par(mfrow = c(1,1))
pie(x, labels, main = "Contribution of All Categories Of Food", col = color)



#Finding the Average calorie value for breakfast and the total Mcdonalds Menu
breakfast_temp = data.csv %>% filter(Category == "Breakfast")
View(breakfast_temp)
cat("Average calorie value of breakfast: ", mean(breakfast_temp$Calories))
cat("Average calorie value of McDonalds menu item: ", mean(data.csv$Calories))

#Plotting all the calorie values

plot_csv = head(data.csv, 110)
ggp <- ggplot(plot_csv,aes(x=Item,y=Calories)) + 
  geom_bar(stat="identity",fill="Red") +
  coord_flip() +
  ylab("Calories") + xlab("Item name") + 
  geom_text(aes(x = Category,   y = Calories+30, label = Calories))
ggp %>% ggplotly
#2

#Filtering the data which contains Category as Beverages, Coffee & Tea, Smoothies & shakes
beverage_temp<-data.frame()
beverage_temp = data.csv %>% filter(Category == "Beverages" | Category == "Coffee & Tea" | Category == "Smoothies & Shakes")


#Finding the Contribution of Beverages like Soda and tea to the overall caloric intake
only_beverages = sum(beverage_temp$Calories)
new_pie = total - only_beverages
percent_beverage = (only_beverages) * 100 / (total)
cat("The percent contributed by beverage: ", percent_beverage)

#Plotting a pie chart to represent the contribution of beverages to the overall caloric intake 
slice<-c(only_beverages, new_pie)
labels<-c("Beverages", "Others")
color<-c("Blue", "Yellow")
pie(slice, labels, main = "Contribution of Beverages", col = color)
legend("topleft", c("Beverages", "Others"), fill = color, cex = 0.5)


#3
par(mfrow = c(1, 1))

#Creating a data frame from storing Grilled and Crispy sandwich Separately
grilled_burger_temp<-data.frame()
crispy_burger_temp<-data.frame()


#Filtering the data which contains Grilled sandwich and storing it in a separate data frame
grilled_temp = data.csv[grep("Grilled", data.csv$Item), ]
grilled_burger_temp = grilled_temp[grep("Sandwich", grilled_temp$Item), ]


##Filtering the data which contains Crispy sandwich and storing it in a separate data frame
crispy_temp = data.csv[grep("Crispy", data.csv$Item), ]
crispy_burger_temp = crispy_temp[grep("Sandwich", crispy_temp$Item), ]


View(grilled_burger_temp)
View(crispy_burger_temp)


#Plotting a Bar plot to study the Nutritional value for Crispy and Grilled chicken by comparing
#its  Calories, Proteins and Fat
g_low = c(mean(grilled_burger_temp$Calories), mean(grilled_burger_temp$Protein), mean(grilled_burger_temp$Total.Fat))
c_low =  c(mean(crispy_burger_temp$Calories), mean(crispy_burger_temp$Protein), mean(crispy_burger_temp$Total.Fat))
matri = matrix(c(g_low, c_low), nrow = 2, ncol = 3, byrow = TRUE)
label = c("Calories", "Protein", "Total Fat")
barplot(matri,
        main = "Grilled vs Crispy",
        xlab = "Categories",
        names.arg = label,
        ylab = "Nutritional Value",
        col = c("red","green"),
        beside = TRUE
)
legend("topright", c("Grilled", "Crispy"), fill = c("Red", "Green"), cex = 0.45)


#Conclusion
cat("In terms of calories and total fat, crispy has more value than grilled. In terms of protein grilled wins")


#4
par(mfrow = c(1, 1))

#Creating a data frame from storing Egg Whites and Whole Egg and Crispy sandwich Separately
egg_data<-data.frame()
eggwhites_data<-data.frame()


#Filtering the data which contains Egg Whites and storing it in a separate data frame
egg_data = data.csv[grep("Egg", data.csv$Item), ]
eggwhites_data = egg_data[grep("Whites", egg_data$Item),]


#Filtering the data which contains Whole Egg and storing it in a separate data frame
egg_data = egg_data[!grepl("Whites", egg_data$Item),]
View(egg_data)
View(eggwhites_data)


#Plotting a Bar plot to study the Nutritional value for Whole Egg and Egg Whites by comparing
#its  Calories, Proteins, Carbohydrates and Fat
eggwhite_low = c(mean(eggwhites_data$Calories), mean(eggwhites_data$Protein), mean(eggwhites_data$Carbohydrates), mean(eggwhites_data$Total.Fat))
egg_low =  c(mean(egg_data$Calories), mean(egg_data$Protein), mean(egg_data$Carbohydrates), mean(egg_data$Total.Fat))
egg_matrix_low = matrix(c(eggwhite_low, egg_low), nrow = 2, ncol = 4, byrow = TRUE)
label = c("Calories", "Protein", "Carbohydrates", "Total Fat")
barplot(egg_matrix_low,
        main = "Egg Whites vs Whole Eggs",
        xlab = "Category",
        names.arg = label,
        ylab = "Nutritional Value",
        col = c("red","green"),
        beside = TRUE
)
legend("topright", c("Egg Whites", "Egg"), fill = c("Red", "Green"), cex = 0.50)


#Conclusion
cat("Egg Whites have more calories, protein, carbohydrates and fat")



#5

#Sorting the Data in Descending order based on Calories
sort_data = data.csv[with(data.csv, order(-Calories)),]
final_analysis = head(sort_data, 2)
View(final_analysis)
print("Looks like we can get the 2,000 calories from only two items, let's analyze those items in terms of the the %DV we will get w.r.t the remaining nutrients", quote = FALSE)

#Finding the Total sum of Nutritional Columns of the 2 Rows 
sum_total_fat = sum(final_analysis$Total.Fat....Daily.Value.)
sum_saturated_fat = sum(final_analysis$Saturated.Fat....Daily.Value.)
sumcholesterol = sum(final_analysis$Cholesterol....Daily.Value.)
sum_sodium = sum(final_analysis$Sodium....Daily.Value.)
sum_Carb = sum(final_analysis$Carbohydrates....Daily.Value.)
sum_dietary_fiber = sum(final_analysis$Dietary.Fiber....Daily.Value.)
sum_vitamin_A = sum(final_analysis$Vitamin.A....Daily.Value.)
sum_vitamin_C = sum(final_analysis$Vitamin.C....Daily.Value.)
sum_calcium = sum(final_analysis$Calcium....Daily.Value.)
sum_Iron = sum(final_analysis$Iron....Daily.Value.)


#Creating a data frame to store the Nutritional value Sum
valuess = c(sum_total_fat, sum_saturated_fat, sumcholesterol,sum_sodium, sum_Carb, sum_dietary_fiber, sum_vitamin_A, sum_vitamin_C, sum_calcium,sum_Iron)
valuess
last_frame <-data.frame(nutrional_values = c("sum_total_fat", "sum_saturated_fat", "sumcholesterol", "sum_sodium", "sum_Carb", "sum_dietary_fiber", "sum_vitamin_A", "sum_vitamin_C", "sum_calcium", "sum_Iron"), values = valuess)
View(last_frame)

#Conclusion
print("From here we can conclude that even we have met the 2,000 from those two items, those items can affect our health badly as they will provide us with double the % DV of the nutrients we want to get less of like Total Fat - Saturated Fat - Cholesterol - Sodium, and will not give us the 100% DV we need from the nutrients we want to get more of like carb - vitamins - fibers and so on.", quote = FALSE)