# PREDICTIVE ANALYSIS OF DATA USING R.
# TECHNIQUES USED ARE REGRESSION,NEURAL NETWORKS,DECISION TREES AND RANDOM FOREST TECHNIQUE. 

l <- 0


while( l < 1)
{  
print(" ")
print(" ")
print(" ")
print("Enter your choice")
print(" ")
print("1.General Trend between Rating and other variables")
print("2.Regression")
print("3.Neural Networks")
print("4.Decision trees")
print("5.Random Forest Technique")
print("6.exit")
ch <- readline()
 
#Used for data visualization.i.e to find the general trend between rating and other variable
if(ch == 1)
{
  
  print(" ")
  print("enter your choice")
  print(" ")
  print("1.Rating VS Complaints")
  print("2.Rating VS Privileges")
  print("3.Rating VS Learning")
  print("4.Rating VS Raises")
  print("5.Rating VS Critical")
  print("6.Rating VS Advance")
  ch1 <- readline()
  
  #use of package and library "ggplot" to visualise the data
  library(ggplot2)
  
  
    if(ch1 == 1){
    
          #A very irregular graph is made and the genral trend is difficult to find     
          trend <-qplot(complaints,rating,data = attitude,geom = "line",main = "General Trend")
          print(trend)
          pause <- readline("press enter")
          
          #A very smooth graph is made and general trend is easily seen in the graph
          trend <- qplot(complaints,rating,data = attitude,geom = "smooth",main = "General Trend")
          print(trend)
          pause <- readline("press enter")
      }
  
    if(ch1 == 2){
      
      trend <-qplot(privileges,rating,data = attitude,geom = "line",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      trend <- qplot(privileges,rating,data = attitude,geom = "smooth",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
    
    
    
    }
  
    if(ch1 == 3){
    
      trend <-qplot(learning,rating,data = attitude,geom = "line",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      trend <- qplot(learning,rating,data = attitude,geom = "smooth",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
    
        }
      
    if(ch1 == 4){
      
      trend <-qplot(raises,rating,data = attitude,geom = "line",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      trend <- qplot(raises,rating,data = attitude,geom = "smooth",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      
    }
    
    if(ch1 == 5){
      
      trend <-qplot(critical,rating,data = attitude,geom = "line",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      trend <- qplot(critical,rating,data = attitude,geom = "smooth",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      
    }
  
    if(ch1 == 6){
      
      trend <-qplot(advance,rating,data = attitude,geom = "line",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      trend <- qplot(advance,rating,data = attitude,geom = "smooth",main = "General Trend")
      print(trend)
      pause <- readline("press enter")
      
    }
  
  }

if(ch == 2)
{
 #Use of regression for machine learning
  
  data <-attitude
  print("Actual data is :-")
  print(data)
  
  index <- sample(1:nrow(data),0.75 * nrow(data))
  
  # I have divided the original data set into two parts :- test and train
  # train data set is used to fit the regression model 
  # test data set is used to test the regression model and perform the predictive analysis 
  
  train <- data[index,]
  test <-data[-index,]
  
  print("Fitting of regression model on the data")
  lm_fit<- lm(rating~.,data = train)
  plot(lm_fit)
  
  pr_lm <- as.data.frame(predict(lm_fit,test))
  
  finalOutput <- cbind(pr_lm,test$rating)
  colnames(finalOutput) <- c("predicted values","actual values")
  View(finalOutput)
  
  plot.default(pr_lm,type = "b",col = "blue",ylim = c(0,100),ylab = "Ratings",main = "predicted value (blue) vs actual value (black)")
  lines(test$rating,type = "b")
  
  mape <- (mean(abs(test$rating - finalOutput$`predicted values`)/test$rating) * 100)
  print("mean absolute percentage error is:- ")
  print(mape)
  
  
}

if(ch == 3)
{
  #Use of neural network for Machine Learning
  
  df <- attitude
  index <- sample(1:nrow(df),round(0.75*nrow(df)))
  
  #standardization of data is done here
  maxs <- apply(df,2,max)
  mins <- apply(df,2,min)
  
  scaled <- as.data.frame(scale(df,center = mins,scale = maxs - mins))
  
  train <- scaled[index,]
  test <-  scaled[-index,]
  
  library("neuralnet")
  
  myFormula <- rating ~ complaints + privileges + learning + raises + critical + advance 
  #fitting of the neural network
  nn <- neuralnet(formula = myFormula ,data = train,hidden = c(5,3))
  plot(nn)
  
  pause <- readline("press enter")
  
  #predictive analysis of the test database
  pr_nn <- compute(nn,test[,2:7])
  
  #conversion of standardized data into actual data
  nn_output <- pr_nn$net.result *(max(df$rating)- min(df$rating)) + min(df$rating) 
  test_output <- test$rating *(max(df$rating)- min(df$rating)) + min(df$rating)
  
  #final output:- comparison between actual and predicted output
  finalOutput <- cbind(nn_output,test_output)
  colnames(finalOutput) <- cbind("predicted","actual")
  View(finalOutput)
  
  #graph to show the variation between actual values and predicted values
  plot.default(nn_output, type = "b", col = "blue",ylim = c(0,100),ylab ="Ratings",main = "Actual values (black) vs predicted values (blue)")
  lines(test_output,type = "b")
  
  print("mean absolute percentage error")
  mape_nn = (mean(abs(test_output - nn_output)/test_output))*100
  
  print(mape_nn)
  
}

if(ch == 4)
{
  #Use of decision trees for machine learning 
  library(party)
  
  data <- attitude
  index <- sample(1:nrow(data),0.75 * nrow(data))
  
  #standardization of data is done here
  maxs <- apply(df,2,max)
  mins <- apply(df,2,min)
  
  scaled <- as.data.frame(scale(df,center = mins,scale = maxs - mins))
  
  train <- scaled[index,]
  test <-scaled[-index,]
  
  myFormula <- rating ~ complaints + privileges + learning + raises + critical + advance 
  
  #using decision trees to train the machine 
  dt <- ctree(myFormula, data = train)
  plot(dt,type ="simple")
  pause <- readline("press enter")
  
  #predictive analysis of data is done here
  dt_pr <- predict(dt ,newdata = test)
  
  #conversion of standardized data into actual data
  dt_output <- dt_pr *(max(data$rating)- min(data$rating)) + min(data$rating) 
  test_output <- test$rating *(max(data$rating)- min(data$rating)) + min(data$rating)
  
  #graph to show the variation between actual values and predicted values
  finalOutput <- cbind(nn_output,test_output)
  colnames(finalOutput) <- cbind("predicted","actual")
  View(finalOutput)
  
  #Graphically show the actual values and the predicted values
  plot.default(dt_output, type = "b", col = "blue",ylim = c(0,100),ylab ="Ratings",main = "Actual values (black) vs predicted values (blue)")
  lines(test_output,type = "b")
  
  print("mean absolute percentage error")
  mape_rf = (mean(abs(test_output - dt_output)/test_output))*100
  
  print(mape_rf)
  
  
}

if (ch == 5)
{
  
  #Use of random forest technique for Machine Learning
  
  df <- attitude
  index <- sample(1:nrow(df),round(0.75*nrow(df)))

  #dividing the data into two parts test and train
  train <- df[index,]
  test <-  df[-index,]
  
  #using the library randomForest  
  library("randomForest")
  myFormula <- rating ~ complaints + privileges + learning + raises + critical + advance 
  
  #using randomForest Technique on the train data set 
  rf <- randomForest(formula = myFormula ,data = train)
  plot(rf)
  
  pause <- readline("press enter")
  
  #To implement machine learning,i.e. predictive analysis of data
  pr_rf <- predict(rf,newdata = test)
  
  #Display the actual values and the predicted values side by side 
  finalOutput <- cbind(pr_rf,test$rating)
  colnames(finalOutput) <- cbind("predicted","actual")
  View(finalOutput)
  
  #Graphically show the actual values and the predicted values
  plot.default(pr_rf, type = "b", col = "blue",ylim = c(0,100),ylab ="Ratings",main = "Actual values (black) vs predicted values (blue)")
  lines(test$rating,type = "b")
  
  print("mean absolute percentage error")
  mape_rf = (mean(abs(test$rating - pr_rf)/test$rating))*100
  
  print(mape_rf)
  
    }

if(ch == 6)
{
  
  l <- 2  
}

}






