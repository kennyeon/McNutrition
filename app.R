library(shiny)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)

menu <- read.csv('data/menu.csv')
menu$Cholesterol <- menu$Cholesterol/100
menu$Sodium <- menu$Sodium/100

menu_long_dv <- menu %>%
  select(-c(Category, Serving.Size, Calories, Calories.from.Fat, Total.Fat, Saturated.Fat,
            Trans.Fat, Cholesterol, Sodium, Carbohydrates, Dietary.Fiber, Sugars, Protein)) %>%
  pivot_longer(cols = -Item, names_to = "Nutrition", values_to = "Value")
menu_long_dv <- pivot_wider(menu_long_dv, names_from = "Item", values_from = "Value")

menu_nutrition <- menu %>%
  select(c(Item, Total.Fat, Saturated.Fat,
            Trans.Fat, Cholesterol, Sodium, Carbohydrates, Dietary.Fiber, Sugars, Protein)) %>%
  pivot_longer(cols = -Item, names_to = "Nutrition", values_to = "Value")
menu_nutrition <- pivot_wider(menu_nutrition, names_from = "Item", values_from = "Value")

# User interface ----
ui <- fluidPage(
  titlePanel(h1(tags$b("McDONALD'S MENU"), style = "color: #FFC72C;", h4("Explore McDonald's complete menu and its nutritional information"), h6("BY: KENNETH YEON"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose which item you would like to see"),
      
      selectInput("category_var", 
                  label = "Choose a menu category",
                  choices = c("Breakfast", "Beef & Pork", "Chicken & Fish", "Salads", "Snacks & Sides", "Desserts", "Beverages", "Coffee & Tea", "Smoothies & Shakes"),
                  selected = "Breakfast"),
      
      selectInput("food_var",
                  label = "Choose a menu item to display",
                  choices = c('Egg McMuffin'),
                  selected = "Egg McMuffin"),
      
      radioButtons("nutrition",
                  label = "Check daily value or nutritional content?",
                  choices = c('Daily Value', 'Nutritional Content'),
                  selected = "Daily Value"),
      
      img(src = "McDonald's_Golden_Arches.svg.webp", width = "100%")
    ),
    
    mainPanel(
      h3(textOutput("selected_food"), style = "font-size: 24px;"),
      plotOutput("dailyValuePlot"),
      br(),
      p("To examine the data set, visit ",
        a("Kaggle", 
          href = "https://www.kaggle.com/datasets/mcdonalds/nutrition-facts"))
    )
  )
)

# server 
server <- function(input, output, session) {
  
  observeEvent(input$category_var, {
    if (input$category_var == "Breakfast") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Beef & Pork") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Chicken & Fish") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Salads") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Snacks & Sides") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Desserts") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Beverages") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Coffee & Tea") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    } else if (input$category_var == "Smoothies & Shakes") {
      food_choices <- subset(menu, Category == input$category_var)$Item
    }
    
    updateSelectInput(session, "food_var", choices = food_choices)
  })
  
  output$selected_food <- renderText({
    input$food_var
  })
  
  output$dailyValuePlot <- renderPlot({
    
    menu_wide_dv <- switch(input$nutrition,
                           "Daily Value" = menu_long_dv,
                           "Nutritional Content" = menu_nutrition)
    
    yLabels <- switch(input$nutrition,
                           "Daily Value" = "Daily Value",
                           "Nutritional Content" = "Amount")
    
    xLabels <- switch(input$nutrition,
                           "Daily Value" = c("Calcium", "Carbohydrates", "Cholesterol", "Dietary Fiber", "Iron", "Saturated Fat", "Sodium", "Total Fat", "Vitamin A", "Vitamin C"),
                           "Nutritional Content" = c("Carbohydrates (g)", "Cholesterol (100 mg)", "Dietary Fiber (g)", "Protein (g)", "Saturated Fat (g)", "Sodium (100 mg)", "Sugars (g)", "Total Fat (g)", "Trans Fat (g)"))
    
    food_item <- switch(input$food_var,
                        "Egg McMuffin" = menu_wide_dv$"Egg McMuffin",
                        "Egg White Delight" = menu_wide_dv$"Egg White Delight",
                        "Sausage McMuffin" = menu_wide_dv$"Sausage McMuffin",
                        "Sausage McMuffin with Egg" = menu_wide_dv$"Sausage McMuffin with Egg",
                        "Sausage McMuffin with Egg Whites" = menu_wide_dv$"Sausage McMuffin with Egg Whites",
                        "Steak & Egg McMuffin" = menu_wide_dv$"Steak & Egg McMuffin",
                        "Bacon, Egg & Cheese Biscuit (Regular Biscuit)" = menu_wide_dv$"Bacon, Egg & Cheese Biscuit (Regular Biscuit)",
                        "Bacon, Egg & Cheese Biscuit (Large Biscuit)" = menu_wide_dv$"Bacon, Egg & Cheese Biscuit (Large Biscuit)",
                        "Bacon, Egg & Cheese Biscuit with Egg Whites (Regular Biscuit)" = menu_wide_dv$"Bacon, Egg & Cheese Biscuit with Egg Whites (Regular Biscuit)",
                        "Bacon, Egg & Cheese Biscuit with Egg Whites (Large Biscuit)" = menu_wide_dv$"Bacon, Egg & Cheese Biscuit with Egg Whites (Large Biscuit)",
                        "Sausage Biscuit (Regular Biscuit)" = menu_wide_dv$"Sausage Biscuit (Regular Biscuit)",
                        "Sausage Biscuit (Large Biscuit)" = menu_wide_dv$"Sausage Biscuit (Large Biscuit)",
                        "Sausage Biscuit with Egg (Regular Biscuit)" = menu_wide_dv$"Sausage Biscuit with Egg (Regular Biscuit)",
                        "Sausage Biscuit with Egg (Large Biscuit)" = menu_wide_dv$"Sausage Biscuit with Egg (Large Biscuit)",
                        "Sausage Biscuit with Egg Whites (Regular Biscuit)" = menu_wide_dv$"Sausage Biscuit with Egg Whites (Regular Biscuit)",
                        "Sausage Biscuit with Egg Whites (Large Biscuit)" = menu_wide_dv$"Sausage Biscuit with Egg Whites (Large Biscuit)",
                        "Southern Style Chicken Biscuit (Regular Biscuit)" = menu_wide_dv$"Southern Style Chicken Biscuit (Regular Biscuit)",
                        "Southern Style Chicken Biscuit (Large Biscuit)" = menu_wide_dv$"Southern Style Chicken Biscuit (Large Biscuit)",
                        "Steak & Egg Biscuit (Regular Biscuit)" = menu_wide_dv$"Steak & Egg Biscuit (Regular Biscuit)",
                        "Bacon, Egg & Cheese McGriddles" = menu_wide_dv$"Bacon, Egg & Cheese McGriddles",
                        "Bacon, Egg & Cheese McGriddles with Egg Whites" = menu_wide_dv$"Bacon, Egg & Cheese McGriddles with Egg Whites",
                        "Sausage McGriddles" = menu_wide_dv$"Sausage McGriddles",
                        "Sausage, Egg & Cheese McGriddles" = menu_wide_dv$"Sausage, Egg & Cheese McGriddles",
                        "Sausage, Egg & Cheese McGriddles with Egg Whites" = menu_wide_dv$"Sausage, Egg & Cheese McGriddles with Egg Whites",
                        "Bacon, Egg & Cheese Bagel" = menu_wide_dv$"Bacon, Egg & Cheese Bagel",
                        "Bacon, Egg & Cheese Bagel with Egg Whites" = menu_wide_dv$"Bacon, Egg & Cheese Bagel with Egg Whites",
                        "Steak, Egg & Cheese Bagel" = menu_wide_dv$"Steak, Egg & Cheese Bagel",
                        "Big Breakfast (Regular Biscuit)" = menu_wide_dv$"Big Breakfast (Regular Biscuit)",
                        "Big Breakfast (Large Biscuit)" = menu_wide_dv$"Big Breakfast (Large Biscuit)",
                        "Big Breakfast with Egg Whites (Regular Biscuit)" = menu_wide_dv$"Big Breakfast with Egg Whites (Regular Biscuit)",
                        "Big Breakfast with Egg Whites (Large Biscuit)" = menu_wide_dv$"Big Breakfast with Egg Whites (Large Biscuit)",
                        "Big Breakfast with Hotcakes (Regular Biscuit)" = menu_wide_dv$"Big Breakfast with Hotcakes (Regular Biscuit)",
                        "Big Breakfast with Hotcakes (Large Biscuit)" = menu_wide_dv$"Big Breakfast with Hotcakes (Large Biscuit)",
                        "Big Breakfast with Hotcakes and Egg Whites (Regular Biscuit)" = menu_wide_dv$"Big Breakfast with Hotcakes and Egg Whites (Regular Biscuit)",
                        "Big Breakfast with Hotcakes and Egg Whites (Large Biscuit)" = menu_wide_dv$"Big Breakfast with Hotcakes and Egg Whites (Large Biscuit)",
                        "Hotcakes" = menu_wide_dv$"Hotcakes",
                        "Hotcakes and Sausage" = menu_wide_dv$"Hotcakes and Sausage",
                        "Sausage Burrito" = menu_wide_dv$"Sausage Burrito",
                        "Hash Brown" = menu_wide_dv$"Hash Brown",
                        "Cinnamon Melts" = menu_wide_dv$"Cinnamon Melts",
                        "Fruit & Maple Oatmeal" = menu_wide_dv$"Fruit & Maple Oatmeal",
                        "Fruit & Maple Oatmeal without Brown Sugar" = menu_wide_dv$"Fruit & Maple Oatmeal without Brown Sugar",
                        "Big Mac" = menu_wide_dv$"Big Mac",
                        "Quarter Pounder with Cheese" = menu_wide_dv$"Quarter Pounder with Cheese",
                        "Quarter Pounder with Bacon & Cheese" = menu_wide_dv$"Quarter Pounder with Bacon & Cheese",
                        "Quarter Pounder with Bacon Habanero Ranch" = menu_wide_dv$"Quarter Pounder with Bacon Habanero Ranch",
                        "Quarter Pounder Deluxe" = menu_wide_dv$"Quarter Pounder Deluxe",
                        "Double Quarter Pounder with Cheese" = menu_wide_dv$"Double Quarter Pounder with Cheese",
                        "Hamburger" = menu_wide_dv$"Hamburger",
                        "Cheeseburger" = menu_wide_dv$"Cheeseburger",
                        "Double Cheeseburger" = menu_wide_dv$"Double Cheeseburger",
                        "Bacon Clubhouse Burger" = menu_wide_dv$"Bacon Clubhouse Burger",
                        "McDouble" = menu_wide_dv$"McDouble",
                        "Bacon McDouble" = menu_wide_dv$"Bacon McDouble",
                        "Daily Double" = menu_wide_dv$"Daily Double",
                        "Jalapeño Double" = menu_wide_dv$"Jalapeño Double",
                        "McRib" = menu_wide_dv$"McRib",
                        "Premium Crispy Chicken Classic Sandwich" = menu_wide_dv$"Premium Crispy Chicken Classic Sandwich",
                        "Premium Grilled Chicken Classic Sandwich" = menu_wide_dv$"Premium Grilled Chicken Classic Sandwich",
                        "Premium Crispy Chicken Club Sandwich" = menu_wide_dv$"Premium Crispy Chicken Club Sandwich",
                        "Premium Grilled Chicken Club Sandwich" = menu_wide_dv$"Premium Grilled Chicken Club Sandwich",
                        "Premium Crispy Chicken Ranch BLT Sandwich" = menu_wide_dv$"Premium Crispy Chicken Ranch BLT Sandwich",
                        "Premium Grilled Chicken Ranch BLT Sandwich" = menu_wide_dv$"Premium Grilled Chicken Ranch BLT Sandwich",
                        "Bacon Clubhouse Crispy Chicken Sandwich" = menu_wide_dv$"Bacon Clubhouse Crispy Chicken Sandwich",
                        "Bacon Clubhouse Grilled Chicken Sandwich" = menu_wide_dv$"Bacon Clubhouse Grilled Chicken Sandwich",
                        "Southern Style Crispy Chicken Sandwich" = menu_wide_dv$"Southern Style Crispy Chicken Sandwich",
                        "McChicken" = menu_wide_dv$"McChicken",
                        "Bacon Cheddar McChicken" = menu_wide_dv$"Bacon Cheddar McChicken",
                        "Bacon Buffalo Ranch McChicken" = menu_wide_dv$"Bacon Buffalo Ranch McChicken",
                        "Buffalo Ranch McChicken" = menu_wide_dv$"Buffalo Ranch McChicken",
                        "Premium McWrap Chicken & Bacon (Crispy Chicken)" = menu_wide_dv$"Premium McWrap Chicken & Bacon (Crispy Chicken)",
                        "Premium McWrap Chicken & Bacon (Grilled Chicken)" = menu_wide_dv$"Premium McWrap Chicken & Bacon (Grilled Chicken)",
                        "Premium McWrap Chicken & Ranch (Crispy Chicken)" = menu_wide_dv$"Premium McWrap Chicken & Ranch (Crispy Chicken)",
                        "Premium McWrap Chicken & Ranch (Grilled Chicken)" = menu_wide_dv$"Premium McWrap Chicken & Ranch (Grilled Chicken)",
                        "Premium McWrap Southwest Chicken (Crispy Chicken)" = menu_wide_dv$"Premium McWrap Southwest Chicken (Crispy Chicken)",
                        "Premium McWrap Southwest Chicken (Grilled Chicken)" = menu_wide_dv$"Premium McWrap Southwest Chicken (Grilled Chicken)",
                        "Premium McWrap Chicken Sweet Chili (Crispy Chicken)" = menu_wide_dv$"Premium McWrap Chicken Sweet Chili (Crispy Chicken)",
                        "Premium McWrap Chicken Sweet Chili (Grilled Chicken)" = menu_wide_dv$"Premium McWrap Chicken Sweet Chili (Grilled Chicken)",
                        "Chicken McNuggets (4 piece)" = menu_wide_dv$"Chicken McNuggets (4 piece)",
                        "Chicken McNuggets (6 piece)" = menu_wide_dv$"Chicken McNuggets (6 piece)",
                        "Chicken McNuggets (10 piece)" = menu_wide_dv$"Chicken McNuggets (10 piece)",
                        "Chicken McNuggets (20 piece)" = menu_wide_dv$"Chicken McNuggets (20 piece)",
                        "Chicken McNuggets (40 piece)" = menu_wide_dv$"Chicken McNuggets (40 piece)",
                        "Filet-O-Fish" = menu_wide_dv$"Filet-O-Fish",
                        "Premium Bacon Ranch Salad (without Chicken)" = menu_wide_dv$"Premium Bacon Ranch Salad (without Chicken)",
                        "Premium Bacon Ranch Salad with Crispy Chicken" = menu_wide_dv$"Premium Bacon Ranch Salad with Crispy Chicken",
                        "Premium Bacon Ranch Salad with Grilled Chicken" = menu_wide_dv$"Premium Bacon Ranch Salad with Grilled Chicken",
                        "Premium Southwest Salad (without Chicken)" = menu_wide_dv$"Premium Southwest Salad (without Chicken)",
                        "Premium Southwest Salad with Crispy Chicken" = menu_wide_dv$"Premium Southwest Salad with Crispy Chicken",
                        "Premium Southwest Salad with Grilled Chicken" = menu_wide_dv$"Premium Southwest Salad with Grilled Chicken",
                        "Chipotle BBQ Snack Wrap (Crispy Chicken)" = menu_wide_dv$"Chipotle BBQ Snack Wrap (Crispy Chicken)",
                        "Chipotle BBQ Snack Wrap (Grilled Chicken)" = menu_wide_dv$"Chipotle BBQ Snack Wrap (Grilled Chicken)",
                        "Honey Mustard Snack Wrap (Crispy Chicken)" = menu_wide_dv$"Honey Mustard Snack Wrap (Crispy Chicken)",
                        "Honey Mustard Snack Wrap (Grilled Chicken)" = menu_wide_dv$"Honey Mustard Snack Wrap (Grilled Chicken)",
                        "Ranch Snack Wrap (Crispy Chicken)" = menu_wide_dv$"Ranch Snack Wrap (Crispy Chicken)",
                        "Ranch Snack Wrap (Grilled Chicken)" = menu_wide_dv$"Ranch Snack Wrap (Grilled Chicken)",
                        "Small French Fries" = menu_wide_dv$"Small French Fries",
                        "Medium French Fries" = menu_wide_dv$"Medium French Fries",
                        "Large French Fries" = menu_wide_dv$"Large French Fries",
                        "Kids French Fries" = menu_wide_dv$"Kids French Fries",
                        "Side Salad" = menu_wide_dv$"Side Salad",
                        "Apple Slices" = menu_wide_dv$"Apple Slices",
                        "Fruit 'n Yogurt Parfait" = menu_wide_dv$"Fruit 'n Yogurt Parfait",
                        "Baked Apple Pie" = menu_wide_dv$"Baked Apple Pie",
                        "Chocolate Chip Cookie" = menu_wide_dv$"Chocolate Chip Cookie",
                        "Oatmeal Raisin Cookie" = menu_wide_dv$"Oatmeal Raisin Cookie",
                        "Kids Ice Cream Cone" = menu_wide_dv$"Kids Ice Cream Cone",
                        "Hot Fudge Sundae" = menu_wide_dv$"Hot Fudge Sundae",
                        "Hot Caramel Sundae" = menu_wide_dv$"Hot Caramel Sundae",
                        "Strawberry Sundae" = menu_wide_dv$"Strawberry Sundae",
                        "Coca-Cola Classic (Small)" = menu_wide_dv$"Coca-Cola Classic (Small)",
                        "Coca-Cola Classic (Medium)" = menu_wide_dv$"Coca-Cola Classic (Medium)",
                        "Coca-Cola Classic (Large)" = menu_wide_dv$"Coca-Cola Classic (Large)",
                        "Coca-Cola Classic (Child)" = menu_wide_dv$"Coca-Cola Classic (Child)",
                        "Diet Coke (Small)" = menu_wide_dv$"Diet Coke (Small)",
                        "Diet Coke (Medium)" = menu_wide_dv$"Diet Coke (Medium)",
                        "Diet Coke (Large)" = menu_wide_dv$"Diet Coke (Large)",
                        "Diet Coke (Child)" = menu_wide_dv$"Diet Coke (Child)",
                        "Dr Pepper (Small)" = menu_wide_dv$"Dr Pepper (Small)",
                        "Dr Pepper (Medium)" = menu_wide_dv$"Dr Pepper (Medium)",
                        "Dr Pepper (Large)" = menu_wide_dv$"Dr Pepper (Large)",
                        "Dr Pepper (Child)" = menu_wide_dv$"Dr Pepper (Child)",
                        "Diet Dr Pepper (Small)" = menu_wide_dv$"Diet Dr Pepper (Small)",
                        "Diet Dr Pepper (Medium)" = menu_wide_dv$"Diet Dr Pepper (Medium)",
                        "Diet Dr Pepper (Large)" = menu_wide_dv$"Diet Dr Pepper (Large)",
                        "Diet Dr Pepper (Child)" = menu_wide_dv$"Diet Dr Pepper (Child)",
                        "Sprite (Small)" = menu_wide_dv$"Sprite (Small)",
                        "Sprite (Medium)" = menu_wide_dv$"Sprite (Medium)",
                        "Sprite (Large)" = menu_wide_dv$"Sprite (Large)",
                        "Sprite (Child)" = menu_wide_dv$"Sprite (Child)",
                        "1% Low Fat Milk Jug" = menu_wide_dv$"1% Low Fat Milk Jug",
                        "Fat Free Chocolate Milk Jug" = menu_wide_dv$"Fat Free Chocolate Milk Jug",
                        "Minute Maid 100% Apple Juice Box" = menu_wide_dv$"Minute Maid 100% Apple Juice Box",
                        "Minute Maid Orange Juice (Small)" = menu_wide_dv$"Minute Maid Orange Juice (Small)",
                        "Minute Maid Orange Juice (Medium)" = menu_wide_dv$"Minute Maid Orange Juice (Medium)",
                        "Minute Maid Orange Juice (Large)" = menu_wide_dv$"Minute Maid Orange Juice (Large)",
                        "Dasani Water Bottle" = menu_wide_dv$"Dasani Water Bottle",
                        "Iced Tea (Small)" = menu_wide_dv$"Iced Tea (Small)",
                        "Iced Tea (Medium)" = menu_wide_dv$"Iced Tea (Medium)",
                        "Iced Tea (Large)" = menu_wide_dv$"Iced Tea (Large)",
                        "Iced Tea (Child)" = menu_wide_dv$"Iced Tea (Child)",
                        "Sweet Tea (Small)" = menu_wide_dv$"Sweet Tea (Small)",
                        "Sweet Tea (Medium)" = menu_wide_dv$"Sweet Tea (Medium)",
                        "Sweet Tea (Large)" = menu_wide_dv$"Sweet Tea (Large)",
                        "Sweet Tea (Child)" = menu_wide_dv$"Sweet Tea (Child)",
                        "Coffee (Small)" = menu_wide_dv$"Coffee (Small)",
                        "Coffee (Medium)" = menu_wide_dv$"Coffee (Medium)",
                        "Coffee (Large)" = menu_wide_dv$"Coffee (Large)",
                        "Latte (Small)" = menu_wide_dv$"Latte (Small)",
                        "Latte (Medium)" = menu_wide_dv$"Latte (Medium)",
                        "Latte (Large)" = menu_wide_dv$"Latte (Large)",
                        "Caramel Latte (Small)" = menu_wide_dv$"Caramel Latte (Small)",
                        "Caramel Latte (Medium)" = menu_wide_dv$"Caramel Latte (Medium)",
                        "Caramel Latte (Large)" = menu_wide_dv$"Caramel Latte (Large)",
                        "Hazelnut Latte (Small)" = menu_wide_dv$"Hazelnut Latte (Small)",
                        "Hazelnut Latte (Medium)" = menu_wide_dv$"Hazelnut Latte (Medium)",
                        "Hazelnut Latte (Large)" = menu_wide_dv$"Hazelnut Latte (Large)",
                        "French Vanilla Latte (Small)" = menu_wide_dv$"French Vanilla Latte (Small)",
                        "French Vanilla Latte (Medium)" = menu_wide_dv$"French Vanilla Latte (Medium)",
                        "French Vanilla Latte (Large)" = menu_wide_dv$"French Vanilla Latte (Large)",
                        "Latte with Sugar Free French Vanilla Syrup (Small)" = menu_wide_dv$"Latte with Sugar Free French Vanilla Syrup (Small)",
                        "Latte with Sugar Free French Vanilla Syrup (Medium)" = menu_wide_dv$"Latte with Sugar Free French Vanilla Syrup (Medium)",
                        "Latte with Sugar Free French Vanilla Syrup (Large)" = menu_wide_dv$"Latte with Sugar Free French Vanilla Syrup (Large)",
                        "Nonfat Latte (Small)" = menu_wide_dv$"Nonfat Latte (Small)",
                        "Nonfat Latte (Medium)" = menu_wide_dv$"Nonfat Latte (Medium)",
                        "Nonfat Latte (Large)" = menu_wide_dv$"Nonfat Latte (Large)",
                        "Nonfat Caramel Latte (Small)" = menu_wide_dv$"Nonfat Caramel Latte (Small)",
                        "Nonfat Caramel Latte (Medium)" = menu_wide_dv$"Nonfat Caramel Latte (Medium)",
                        "Nonfat Caramel Latte (Large)" = menu_wide_dv$"Nonfat Caramel Latte (Large)",
                        "Nonfat Hazelnut Latte (Small)" = menu_wide_dv$"Nonfat Hazelnut Latte (Small)",
                        "Nonfat Hazelnut Latte (Medium)" = menu_wide_dv$"Nonfat Hazelnut Latte (Medium)",
                        "Nonfat Hazelnut Latte (Large)" = menu_wide_dv$"Nonfat Hazelnut Latte (Large)",
                        "Nonfat French Vanilla Latte (Small)" = menu_wide_dv$"Nonfat French Vanilla Latte (Small)",
                        "Nonfat French Vanilla Latte (Medium)" = menu_wide_dv$"Nonfat French Vanilla Latte (Medium)",
                        "Nonfat French Vanilla Latte (Large)" = menu_wide_dv$"Nonfat French Vanilla Latte (Large)",
                        "Nonfat Latte with Sugar Free French Vanilla Syrup (Small)" = menu_wide_dv$"Nonfat Latte with Sugar Free French Vanilla Syrup (Small)",
                        "Nonfat Latte with Sugar Free French Vanilla Syrup (Medium)" = menu_wide_dv$"Nonfat Latte with Sugar Free French Vanilla Syrup (Medium)",
                        "Nonfat Latte with Sugar Free French Vanilla Syrup (Large)" = menu_wide_dv$"Nonfat Latte with Sugar Free French Vanilla Syrup (Large)",
                        "Mocha (Small)" = menu_wide_dv$"Mocha (Small)",
                        "Mocha (Medium)" = menu_wide_dv$"Mocha (Medium)",
                        "Mocha (Large)" = menu_wide_dv$"Mocha (Large)",
                        "Mocha with Nonfat Milk (Small)" = menu_wide_dv$"Mocha with Nonfat Milk (Small)",
                        "Mocha with Nonfat Milk (Medium)" = menu_wide_dv$"Mocha with Nonfat Milk (Medium)",
                        "Mocha with Nonfat Milk (Large)" = menu_wide_dv$"Mocha with Nonfat Milk (Large)",
                        "Caramel Mocha (Small)" = menu_wide_dv$"Caramel Mocha (Small)",
                        "Caramel Mocha (Medium)" = menu_wide_dv$"Caramel Mocha (Medium)",
                        "Caramel Mocha (Large)" = menu_wide_dv$"Caramel Mocha (Large)",
                        "Nonfat Caramel Mocha (Small)" = menu_wide_dv$"Nonfat Caramel Mocha (Small)",
                        "Nonfat Caramel Mocha (Medium)" = menu_wide_dv$"Nonfat Caramel Mocha (Medium)",
                        "Nonfat Caramel Mocha (Large)" = menu_wide_dv$"Nonfat Caramel Mocha (Large)",
                        "Hot Chocolate (Small)" = menu_wide_dv$"Hot Chocolate (Small)",
                        "Hot Chocolate (Medium)" = menu_wide_dv$"Hot Chocolate (Medium)",
                        "Hot Chocolate (Large)" = menu_wide_dv$"Hot Chocolate (Large)",
                        "Hot Chocolate with Nonfat Milk (Small)" = menu_wide_dv$"Hot Chocolate with Nonfat Milk (Small)",
                        "Hot Chocolate with Nonfat Milk (Medium)" = menu_wide_dv$"Hot Chocolate with Nonfat Milk (Medium)",
                        "Hot Chocolate with Nonfat Milk (Large)" = menu_wide_dv$"Hot Chocolate with Nonfat Milk (Large)",
                        "Regular Iced Coffee (Small)" = menu_wide_dv$"Regular Iced Coffee (Small)",
                        "Regular Iced Coffee (Medium)" = menu_wide_dv$"Regular Iced Coffee (Medium)",
                        "Regular Iced Coffee (Large)" = menu_wide_dv$"Regular Iced Coffee (Large)",
                        "Caramel Iced Coffee (Small)" = menu_wide_dv$"Caramel Iced Coffee (Small)",
                        "Caramel Iced Coffee (Medium)" = menu_wide_dv$"Caramel Iced Coffee (Medium)",
                        "Caramel Iced Coffee (Large)" = menu_wide_dv$"Caramel Iced Coffee (Large)",
                        "Hazelnut Iced Coffee (Small)" = menu_wide_dv$"Hazelnut Iced Coffee (Small)",
                        "Hazelnut Iced Coffee (Medium)" = menu_wide_dv$"Hazelnut Iced Coffee (Medium)",
                        "Hazelnut Iced Coffee (Large)" = menu_wide_dv$"Hazelnut Iced Coffee (Large)",
                        "French Vanilla Iced Coffee (Small)" = menu_wide_dv$"French Vanilla Iced Coffee (Small)",
                        "French Vanilla Iced Coffee (Medium)" = menu_wide_dv$"French Vanilla Iced Coffee (Medium)",
                        "French Vanilla Iced Coffee (Large)" = menu_wide_dv$"French Vanilla Iced Coffee (Large)",
                        "Iced Coffee with Sugar Free French Vanilla Syrup (Small)" = menu_wide_dv$"Iced Coffee with Sugar Free French Vanilla Syrup (Small)",
                        "Iced Coffee with Sugar Free French Vanilla Syrup (Medium)" = menu_wide_dv$"Iced Coffee with Sugar Free French Vanilla Syrup (Medium)",
                        "Iced Coffee with Sugar Free French Vanilla Syrup (Large)" = menu_wide_dv$"Iced Coffee with Sugar Free French Vanilla Syrup (Large)",
                        "Iced Mocha (Small)" = menu_wide_dv$"Iced Mocha (Small)",
                        "Iced Mocha (Medium)" = menu_wide_dv$"Iced Mocha (Medium)",
                        "Iced Mocha (Large)" = menu_wide_dv$"Iced Mocha (Large)",
                        "Iced Mocha with Nonfat Milk (Small)" = menu_wide_dv$"Iced Mocha with Nonfat Milk (Small)",
                        "Iced Mocha with Nonfat Milk (Medium)" = menu_wide_dv$"Iced Mocha with Nonfat Milk (Medium)",
                        "Iced Mocha with Nonfat Milk (Large)" = menu_wide_dv$"Iced Mocha with Nonfat Milk (Large)",
                        "Iced Caramel Mocha (Small)" = menu_wide_dv$"Iced Caramel Mocha (Small)",
                        "Iced Caramel Mocha (Medium)" = menu_wide_dv$"Iced Caramel Mocha (Medium)",
                        "Iced Caramel Mocha (Large)" = menu_wide_dv$"Iced Caramel Mocha (Large)",
                        "Iced Nonfat Caramel Mocha (Small)" = menu_wide_dv$"Iced Nonfat Caramel Mocha (Small)",
                        "Iced Nonfat Caramel Mocha (Medium)" = menu_wide_dv$"Iced Nonfat Caramel Mocha (Medium)",
                        "Iced Nonfat Caramel Mocha (Large)" = menu_wide_dv$"Iced Nonfat Caramel Mocha (Large)",
                        "Frappé Mocha (Small)" = menu_wide_dv$"Frappé Mocha (Small)",
                        "Frappé Mocha (Medium)" = menu_wide_dv$"Frappé Mocha (Medium)",
                        "Frappé Mocha (Large)" = menu_wide_dv$"Frappé Mocha (Large)",
                        "Frappé Caramel (Small)" = menu_wide_dv$"Frappé Caramel (Small)",
                        "Frappé Caramel (Medium)" = menu_wide_dv$"Frappé Caramel (Medium)",
                        "Frappé Caramel (Large)" = menu_wide_dv$"Frappé Caramel (Large)",
                        "Frappé Chocolate Chip (Small)" = menu_wide_dv$"Frappé Chocolate Chip (Small)",
                        "Frappé Chocolate Chip (Medium)" = menu_wide_dv$"Frappé Chocolate Chip (Medium)",
                        "Frappé Chocolate Chip (Large)" = menu_wide_dv$"Frappé Chocolate Chip (Large)",
                        "Blueberry Pomegranate Smoothie (Small)" = menu_wide_dv$"Blueberry Pomegranate Smoothie (Small)",
                        "Blueberry Pomegranate Smoothie (Medium)" = menu_wide_dv$"Blueberry Pomegranate Smoothie (Medium)",
                        "Blueberry Pomegranate Smoothie (Large)" = menu_wide_dv$"Blueberry Pomegranate Smoothie (Large)",
                        "Strawberry Banana Smoothie (Small)" = menu_wide_dv$"Strawberry Banana Smoothie (Small)",
                        "Strawberry Banana Smoothie (Medium)" = menu_wide_dv$"Strawberry Banana Smoothie (Medium)",
                        "Strawberry Banana Smoothie (Large)" = menu_wide_dv$"Strawberry Banana Smoothie (Large)",
                        "Mango Pineapple Smoothie (Small)" = menu_wide_dv$"Mango Pineapple Smoothie (Small)",
                        "Mango Pineapple Smoothie (Medium)" = menu_wide_dv$"Mango Pineapple Smoothie (Medium)",
                        "Mango Pineapple Smoothie (Large)" = menu_wide_dv$"Mango Pineapple Smoothie (Large)",
                        "Vanilla Shake (Small)" = menu_wide_dv$"Vanilla Shake (Small)",
                        "Vanilla Shake (Medium)" = menu_wide_dv$"Vanilla Shake (Medium)",
                        "Vanilla Shake (Large)" = menu_wide_dv$"Vanilla Shake (Large)",
                        "Strawberry Shake (Small)" = menu_wide_dv$"Strawberry Shake (Small)",
                        "Strawberry Shake (Medium)" = menu_wide_dv$"Strawberry Shake (Medium)",
                        "Strawberry Shake (Large)" = menu_wide_dv$"Strawberry Shake (Large)",
                        "Chocolate Shake (Small)" = menu_wide_dv$"Chocolate Shake (Small)",
                        "Chocolate Shake (Medium)" = menu_wide_dv$"Chocolate Shake (Medium)",
                        "Chocolate Shake (Large)" = menu_wide_dv$"Chocolate Shake (Large)",
                        "Shamrock Shake (Medium)" = menu_wide_dv$"Shamrock Shake (Medium)",
                        "Shamrock Shake (Large)" = menu_wide_dv$"Shamrock Shake (Large)",
                        "McFlurry with M&M’s Candies (Small)" = menu_wide_dv$"McFlurry with M&M’s Candies (Small)",
                        "McFlurry with M&M’s Candies (Medium)" = menu_wide_dv$"McFlurry with M&M’s Candies (Medium)",
                        "McFlurry with M&M’s Candies (Snack)" = menu_wide_dv$"McFlurry with M&M’s Candies (Snack)",
                        "McFlurry with Oreo Cookies (Small)" = menu_wide_dv$"McFlurry with Oreo Cookies (Small)",
                        "McFlurry with Oreo Cookies (Medium)" = menu_wide_dv$"McFlurry with Oreo Cookies (Medium)",
                        "McFlurry with Oreo Cookies (Snack)" = menu_wide_dv$"McFlurry with Oreo Cookies (Snack)",
                        "McFlurry with Reese's Peanut Butter Cups (Medium)" = menu_wide_dv$"McFlurry with Reese's Peanut Butter Cups (Medium)",
                        "McFlurry with Reese's Peanut Butter Cups (Snack)" = menu_wide_dv$"McFlurry with Reese's Peanut Butter Cups (Snack)")
    
    textLab <- switch(input$nutrition,
                      "Daily Value" = paste0(food_item, "%"),
                      "Nutritional Content" = food_item)
    
    ylimits <- switch(input$nutrition,
                      "Daily Value" = c(0, 200),
                      "Nutritional Content" = c(0,100))
    
    mcColor <- switch(input$nutrition,
                      "Daily Value" = "#DA291C",
                      "Nutritional Content" = "#FFC72C")
    
    ggplot(menu_wide_dv, aes(x = Nutrition, y = food_item)) +
      geom_bar(stat = "identity", fill = mcColor) +
      geom_text(aes(label = textLab), vjust = -0.5) +
      scale_x_discrete(labels = xLabels) +
      ylab(yLabels) +
      scale_y_continuous(limits = ylimits, breaks = seq(0, 200, 25)) +
      theme_classic() +
      theme(panel.background = element_rect(fill=alpha('#FFC72C', 0.2)))
    
  })
    
}

shinyApp(ui, server)
