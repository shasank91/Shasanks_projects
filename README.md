# Shasanks_projects
Projects done in Northeastern 
library(RSQLite)
library(sqldf)
library(ggplot2)
library(grid)

setwd("~/OneDrive/school/collected storing/project")
#Loading the data into dataframe 'food'
food<- read.csv("USMainDB2016_project.csv") 

#Columnn "food.Description" is split and only the string value before the comma is put into the dataframe. 
#This would be the Food_Type column
type <- as.data.frame(sapply(strsplit(as.character(food$Food.Description), 
         split=", "),"[",1))
colnames(type)<- c("Ftype")

#In this step the 'Short.Food.Description' column is split and the string value after the first comma are only considered
tick <- sapply(strsplit(as.character(food$Short.Food.Description), 
                        split=", "),"[",1)

tick2 <- sapply(strsplit(as.character(food$Short.Food.Description), 
                         split=", "),"[",2)

tick3 <- sapply(strsplit(as.character(food$Short.Food.Description), 
                         split=", "),"[",3)

tick4 <- sapply(strsplit(as.character(food$Short.Food.Description), 
                         split=", "),"[",4)

tick5 <- sapply(strsplit(as.character(food$Short.Food.Description), 
                         split=", "),"[",5)

#All the terms 'Baby food' in 'short.Food.Description' is turned to NA.
tick[tick== 'Baby food']= NA 
der<- data.frame(cbind(tick,tick2,tick3, tick4, tick5)) 

der<- sapply(der, as.character)
#Now turning all the NA's to blank
der[is.na(der)] <- " " 

jam<- data.frame(der)

#The 'finaltype' is going to be the 'food_description' column in the data frame 'food'.
jam$finaltype<- paste(jam$tick,"",jam$tick2,"", jam$tick3, " ", jam$tick4," ", jam$tick5) 

#The columns ftype and finaltype columns in the data frame food
food<-cbind(food, type$Ftype)
colnames(food)[50] <- "food_type"

food<- cbind(food, jam$finaltype)
colnames(food)[51]<- "food_description"


#New Dataframe nutri has been created to show the columns we are considering, but for the rest of code dataframe food will be used.
nutri<- food[c(1,50,51,4,6:8,10:14,17:19,21:23,27,28,36,37,45,46)]
colnames(nutri)<-c("Food_ID", "Food_Type", "Food_Description","Portion_size_amount","Porton_size_description", "Portion_size_weight",
                 "Food_Group","k_calories", "Total_protein", "Total_Fat", "Total_carbohydrate", "Total_fiber", "Total_calcium",
                 "Total_iron", "Total_magnesium", "Total_potassium", "Total_sodium", "Total_zinc", "Vitamin_C", "Vitamin_B",
                 "Vitamin_A", "Vitamin_D", "Total_cholestrol", "Total_sugar")
View(nutri) 

#Storing
#Creating Database to store the dataframe
#Connecting to the database SQLite
trial <- dbConnect(SQLite(), dbname="foodtrial.sqlite")

dbWriteTable(conn = trial, name = "foodtrial", value = food,row.names = FALSE, header = TRUE)
dbListFields(trial,"foodtrial")
dbReadTable(trial, "foodtrial")


#Table 1 (food details)
fdetail<-cbind.data.frame(as.character(food$Food.ID), as.character(food$food_type) ,as.character(food$food_description), 
                          as.character(food$NCC.Food.Group))

colnames(fdetail)<- c("Food_ID","Food_Type", "Food_Description", "Food_Group")

dbWriteTable(conn = trial, name = "Foodesc", value = fdetail,row.names = FALSE) 

dbListFields(trial,"Foodesc")
dbReadTable(trial, "Foodesc")


#Table 2 (food portion)
fportion<- cbind.data.frame(as.character(food$Common.Portion.Size.Amount), as.character(food$Common.Portion.Size.Description), 
                            as.numeric(food$Common.Portion.Size.Gram.Weight), as.character(food$Food.ID))

colnames(fportion)<- c("Portion_size_amount","Porton_size_description", "Portion_size_weight", "Food_ID")

dbWriteTable(conn = trial, name = "Foodport", value = fportion,row.names = FALSE) 

dbListFields(trial,"Foodport")
dbReadTable(trial, "Foodport")

#Table 3 (food nutrition)

fnutrition<- cbind.data.frame(as.numeric(food$Energy..kcal.), as.numeric(food$Total.Protein..g.), as.numeric(food$Total.Fat..g.), 
                              as.numeric(food$Total.Carbohydrate..g.), as.numeric(food$Total.Dietary.Fiber..g.), 
                              as.numeric(food$Total.Sugars..g.), as.numeric(food$Cholesterol..mg.),as.character(food$Food.ID))

colnames(fnutrition)<- c("k_calories", "Total_protein", "Total_Fat", "Total_carbohydrate", "Total_fiber","Total_sugar", "Total_cholestrol", "Food_ID")

#Generating unique numbers to make it a primary key
fnutrition$NutritionID<-1:nrow(fnutrition) 

dbWriteTable(conn = trial, name = "Foodnut", value = fnutrition,row.names = FALSE) 

dbListFields(trial,"Foodnut")
dbReadTable(trial, "Foodnut")

#Table 4 (Food Minerals)
fminerals<- cbind.data.frame(fnutrition$NutritionID, as.numeric(food$Iron..mg.), as.numeric(food$Sodium..mg.), as.numeric(food$Calcium..mg.),
                             as.numeric(food$Magnesium..mg.), as.numeric(food$Potassium..mg.), as.numeric(food$Zinc..mg.))

colnames(fminerals)<- c("NutritionID", "Total_iron", "Total_sodium", "Total_calcium", "Total_magnesium", "Total_potassium", "Total_zinc")

dbWriteTable(conn = trial, name = "Foodmin", value = fminerals,row.names = FALSE) 

dbListFields(trial,"Foodmin")
dbReadTable(trial, "Foodmin")

# Table 5 (Food Vitamins)
fvitamins<- cbind.data.frame(fnutrition$NutritionID, as.numeric(food$Thiamin..vitamin.B1...mg.), 
                             as.numeric(food$Total.Vitamin.A.Activity..International.Units...IU.), as.numeric(food$Vitamin.D..calciferol...mcg.), 
                             as.numeric(food$Vitamin.E..Total.Alpha.Tocopherol...mg.), as.numeric(food$Vitamin.C..ascorbic.acid...mg.))

colnames(fvitamins)<- c("NutritionID", "Vitamin_B", "Vitamin_A", "Vitamin_D", "Vitamin_E","Vitamin_C")

dbWriteTable(conn = trial, name = "Foodvit", value = fvitamins,row.names = FALSE) 

dbListFields(trial,"Foodvit")
dbReadTable(trial, "foodvit")

#Retreiving 
#Funtions and plotting to test the database

#This function will give the maximum sugar content for a particular food type
#It will also display a graph comparing the sugar content of all food types
sugarcontent<- function()
{
  con<- dbGetQuery(trial, "SELECT Foodnut.Food_ID, Foodesc.Food_Description, max(Total_sugar) 
                   FROM Foodesc JOIN Foodnut on Foodesc.Food_ID= Foodnut.Food_ID WHERE Foodesc.Food_Type= 'baby food'")
  
  #Displaying the graph to compare the sugar content in 'baby food' as compared to all other kinds of foods
  lad <-  ggplot(food, aes(x = food$food_type, y = food$Total.Sugars..g., col = food$Common.Portion.Size.Amount))+ geom_point() + labs(colour = "Portion Size", x = "Food Type", y = "Sugar Content", title = "Sugar Content-Food Types", subtitle = "for different portion sizes") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10))
  
  return(list(lad, con))
  
}

sugarcontent()

#This function will give the list of all the carbohydrate in descending order 
#Also displays a graph to compare the carbohydrates content for all the food types
carbs<- function()
{  
  sod<- dbGetQuery(trial, "SELECT Foodesc.Food_ID, Foodesc.Food_description, Foodnut.Total_carbohydrate 
                   FROM Foodesc JOIN Foodnut on Foodesc.Food_ID= Foodnut.Food_ID ORDER BY Total_carbohydrate DESC")
  
  prod <- ggplot(food, aes(x = food$food_type, y = food$Total.Carbohydrate..g., col = food$Common.Portion.Size.Amount)) + geom_point(alpha = 0.4) + labs(colour = "Portion Size", x = "Food Type", y = "Carb Content", title = "Carbohydrate content for Food Types", subtitle = "Portion Sizes") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10))
  
  return(list(sod, prod))
  
}

carbs()

#Displays the vitamin A content for cookies in descending order
#Also displays graphs to compare the vitamins for all food types
vit<- function()
{
  vol<- dbGetQuery(trial, "SELECT Foodesc.Food_ID, Foodesc.Food_Description, Foodesc.Food_Group, Foodvit.Vitamin_A 
                   FROM Foodesc JOIN Foodnut ON Foodesc.Food_ID= Foodnut.Food_ID JOIN Foodvit ON 
                   Foodnut.NutritionID = Foodvit.NutritionID WHERE Foodesc.Food_Group= 'Cookies' ORDER BY Foodvit.Vitamin_A DESC")
  
  bol1 <- ggplot(food, aes(x = food$food_type, y = food$Total.Vitamin.A.Activity..International.Units...IU.)) + geom_col() + labs(x = "Food Type", y = "Vit A", title = "Vit A v/s Food Type") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10)) 
  
  bol2 <- ggplot(food, aes(x = food$food_type, y = food$Thiamin..vitamin.B1...mg.)) + geom_col() + labs(x = "Food Type", y = "Vit B", title = "Vit B v/s Food Type") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10))
  
  bol3 <- ggplot(food, aes(x = food$food_type, y = food$Vitamin.C..ascorbic.acid...mg.)) + geom_col() + labs(x = "Food Type", y = "Vit C", title = "Vit C v/s Food Type") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10))
  
  bol4 <- ggplot(food, aes(x = food$food_type, y = food$Vitamin.D..calciferol...mcg.)) + geom_col() + labs(x = "Food Type", y = "Vit D", title = "Vit D v/s Food Type") + theme(axis.title.x = element_text(face = "bold", colour = "#990000", size = 10), axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6),axis.title.y = element_text(face = "bold", colour = "#990000", size = 10))
  
  #-----------
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
  {
    
    #Make a list from the ... arguments and plotlist
    plots <- c(list(bol1, bol2, bol3, bol4), plotlist)
    
    numPlots = length(plots)
    
    #If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      #Make the panel
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) 
    {
      print(plots[[1]])
    }
    
    else 
    {
      #Setting up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      #Make each plot, in the correct location
      for (i in 1:numPlots) {
        #Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  
  dol <- multiplot(bol1, bol2, bol3, bol4, cols = 2)
  
  #Plotting the vitamin A, B, C and D values for all food types to show how the Vitamin values for Cookies compare with other food types
  return(list(dol, vol))
}

vit()

#Function calculating average nutrition for different food types, just need to input the food types
avg<- function(avgtype)
{
  lego<- avgtype
  dev= data.frame(dbGetQuery(trial, paste( "SELECT Foodesc.Food_Type, avg(Foodnut.k_calories), 
                  avg(Total_protein), avg(Total_Fat), avg(Total_carbohydrate), avg(Total_fiber), 
                  avg(Total_sugar), avg(Total_cholestrol) FROM Foodnut JOIN Foodesc ON 
                  Foodnut.Food_ID= Foodesc.Food_ID AND Foodesc.Food_Type= '",lego,"'",sep="")))
  return(dev)
  
}
avg("baby food")


################################################################


