#Import the packages
library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)
library(dplyr)
library(grid)
library(MLmetrics)

data <- uow_consumption #getting the dataset
# Create input/output matrices with different lagged time steps
time_lagged_data_1 <- data.frame(
  t_1 = lag(data$time20, 1),
  output = data$time20)[-c(1, 1), ]
time_lagged_data_2 <- data.frame(t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1, 2), ]

time_lagged_data_3 <- data.frame(t_3 = lag(data$time20, 3),
                                 t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1:3), ]

time_lagged_data_4 <- data.frame(t_4 = lag(data$time20, 4),
                                 t_3 = lag(data$time20, 3),
                                 t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1:4), ]

time_lagged_data_4

#normalization funtion

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#unnormalization funtion
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


#normalize the I/O metix
Scale_time__data_1 <- as.data.frame(lapply(time_lagged_data_1[1:2], normalize ))
Scale_time__data_2 <- as.data.frame(lapply(time_lagged_data_2[1:3], normalize ))
Scale_time__data_3 <- as.data.frame(lapply(time_lagged_data_3[1:4], normalize ))
Scale_time__data_4 <- as.data.frame(lapply(time_lagged_data_4[1:5], normalize ))

Scale_time__data_1
Scale_time__data_2
Scale_time__data_3
Scale_time__data_4

#devied the train and test data set

train_norm_t_1 <- Scale_time__data_1 [2:380,]
test_norm_t_1 <- Scale_time__data_1 [381:469,]
test_norm_t_1

train_norm_t_2 <- Scale_time__data_2 [2:380,]
test_norm_t_2<- Scale_time__data_2 [381:468,]
test_norm_t_2
train_norm_t_3 <- Scale_time__data_3 [2:380,]
test_norm_t_3 <- Scale_time__data_3 [381:467,]
test_norm_t_3
train_norm_t_4 <- Scale_time__data_4 [2:380,]
test_norm_t_4 <- Scale_time__data_4 [381:466,]
test_norm_t_4

# Train MLP models with different configurations D
model_1 <- neuralnet(output ~ t_1, data = train_norm_t_1, hidden = c(1))
model_2 <- neuralnet(output ~ t_1, data = train_norm_t_2, hidden = c(3))
model_3 <- neuralnet(output ~ t_1 + t_2 + t_3, data = train_norm_t_3, hidden = c(6))
model_4 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4, data = train_norm_t_4, hidden = c(9), linear.output = FALSE)

model_5 <- neuralnet(output ~ t_1, data = train_norm_t_1, hidden = c(7,4))
model_6 <- neuralnet(output ~ t_1, data = train_norm_t_2, hidden = c(5,5))
model_7 <- neuralnet(output ~ t_1 + t_2 + t_3, data = train_norm_t_3, hidden = c(10, 4))
model_8 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4, data = train_norm_t_4, hidden = c(9, 7), linear.output = FALSE)

model_9 <- neuralnet(output ~ t_1, data = train_norm_t_1, hidden = c(6))
model_10 <- neuralnet(output ~ t_1, data = train_norm_t_2, hidden = c(2))
model_11 <- neuralnet(output ~ t_1 + t_2 + t_3, data = train_norm_t_3, hidden = c(5))
model_12 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4, data = train_norm_t_4, hidden = c(8), linear.output = FALSE)


plot(model_1)
plot(model_2)
plot(model_3)
plot(model_4)
plot(model_5)
plot(model_6)
plot(model_7)
plot(model_8)
plot(model_9)
plot(model_10)
plot(model_11)
plot(model_12)


# Make predictions on test data and calculate error metrics
predictions_1 <- predict(model_1, test_norm_t_1[, 1:2])
predictions_2 <- predict(model_2, test_norm_t_2[, 1:2])
predictions_3 <- predict(model_3, test_norm_t_3[, 1:3])
predictions_4 <- predict(model_4, test_norm_t_4[, 1:4])

predictions_5 <- predict(model_5, test_norm_t_1[, 1:2])
predictions_6 <- predict(model_6, test_norm_t_2[, 1:2])
predictions_7 <- predict(model_7, test_norm_t_3[, 1:3])
predictions_8 <- predict(model_8, test_norm_t_4[, 1:4])

predictions_9 <- predict(model_9, test_norm_t_1[, 1:2])
predictions_10 <- predict(model_10, test_norm_t_2[, 1:2])
predictions_11<- predict(model_11, test_norm_t_3[, 1:3])
predictions_12 <- predict(model_12, test_norm_t_4[, 1:4])

predictions_4



original_train_output1 <- Scale_time__data_1[1:380,"output"] 
original_test_output1 <- Scale_time__data_1[381:469,"output"]
original_train_output2 <- Scale_time__data_2[1:380,"output"] 
original_test_output2 <- Scale_time__data_2[381:468,"output"] 
original_test_output2
original_train_output3 <- Scale_time__data_3[1:380,"output"] 
original_test_output3 <- Scale_time__data_3[381:467,"output"] 
original_train_output4 <- Scale_time__data_4[1:380,"output"] 
original_test_output4 <- Scale_time__data_4[381:466,"output"] 



output_min1 <- min(original_train_output1)
output_max1 <- max(original_train_output1)

output_min2 <- min(original_train_output2)
output_max2 <- max(original_train_output2)

output_min3 <- min(original_train_output3)
output_max3 <- max(original_train_output3)


output_min4 <- min(original_train_output4)
output_max4 <- max(original_train_output4)

# unnormalize the data set
unScale_time__data_1 <- unnormalize(predictions_1, output_min1, output_max1)
unScale_time__data_2 <- unnormalize(predictions_2, output_min2, output_max2)
unScale_time__data_3 <- unnormalize(predictions_3, output_min3, output_max3)
unScale_time__data_4 <- unnormalize(predictions_4, output_min4, output_max4)

unScale_time__data_5 <- unnormalize(predictions_5, output_min1, output_max1)
unScale_time__data_6 <- unnormalize(predictions_6, output_min2, output_max2)
unScale_time__data_7 <- unnormalize(predictions_7, output_min3, output_max3)
unScale_time__data_8 <- unnormalize(predictions_8, output_min4, output_max4)

unScale_time__data_9 <- unnormalize(predictions_9, output_min1, output_max1)
unScale_time__data_10 <- unnormalize(predictions_10, output_min2, output_max2)
unScale_time__data_11 <- unnormalize(predictions_11, output_min3, output_max3)
unScale_time__data_12 <- unnormalize(predictions_12, output_min4, output_max4)


unScale_time__data_2
original_test_output2


rmse_1 <- RMSE(original_test_output1, unScale_time__data_1)
rmse_2 <- RMSE(original_test_output2, unScale_time__data_2)
rmse_3 <- RMSE(original_test_output3, unScale_time__data_3)
rmse_4 <- RMSE(original_test_output4, unScale_time__data_4)
rmse_5 <- RMSE(original_test_output1, unScale_time__data_5)
rmse_6 <- RMSE(original_test_output2, unScale_time__data_6)
rmse_7 <- RMSE(original_test_output3, unScale_time__data_7)
rmse_8 <- RMSE(original_test_output4, unScale_time__data_8)
rmse_9 <- RMSE(original_test_output1, unScale_time__data_9)
rmse_10 <- RMSE(original_test_output2,unScale_time__data_10)
rmse_11<- RMSE(original_test_output3, unScale_time__data_11)
rmse_12 <- RMSE(original_test_output4,unScale_time__data_12)



mape_1 <- MAPE(original_test_output1, unScale_time__data_1)
mape_2 <- MAPE(original_test_output2, unScale_time__data_2)
mape_3 <- MAPE(original_test_output3, unScale_time__data_3)
mape_4 <- MAPE(original_test_output4, unScale_time__data_4)
mape_5 <- MAPE(original_test_output1, unScale_time__data_5)
mape_6 <- MAPE(original_test_output2, unScale_time__data_6)
mape_7 <- MAPE(original_test_output3, unScale_time__data_7)
mape_8 <- MAPE(original_test_output4, unScale_time__data_8)
mape_9 <- MAPE(original_test_output1, unScale_time__data_9)
mape_10 <- MAPE(original_test_output2,unScale_time__data_10)
mape_11 <- MAPE(original_test_output3,unScale_time__data_11)
mape_12 <- MAPE(original_test_output4,unScale_time__data_12)

mse_1 <- MSE(original_test_output1, unScale_time__data_1)
mse_2 <- MSE(original_test_output2, unScale_time__data_2)
mse_3 <- MSE(original_test_output3, unScale_time__data_3)
mse_4 <- MSE(original_test_output4, unScale_time__data_4)
mse_5 <- MSE(original_test_output1, unScale_time__data_5)
mse_6 <- MSE(original_test_output2, unScale_time__data_6)
mse_7 <- MSE(original_test_output3, unScale_time__data_7)
mse_8 <- MSE(original_test_output4, unScale_time__data_8)
mse_9 <- MSE(original_test_output1, unScale_time__data_9)
mse_10 <- MSE(original_test_output2, unScale_time__data_10)
mse_11<- MSE(original_test_output3, unScale_time__data_11)
mse_12 <- MSE(original_test_output4, unScale_time__data_12)




# Print error metrics
cat("Model 1:\n")
cat("RMSE: ", rmse_1, "\n")
cat("MAPE: ", mape_1, "\n")
cat("MSE: ", mse_1, "\n")


cat("Model 2:\n")
cat("RMSE: ", rmse_2, "\n")
cat("MAPE: ", mape_2, "\n")
cat("MSE: ", mse_2, "\n")

cat("Model 3:\n")
cat("RMSE: ", rmse_3, "\n")
cat("MAPE: ", mape_3, "\n")
cat("MSE: ", mse_3, "\n")

cat("Model 4:\n")
cat("RMSE: ", rmse_4, "\n")
cat("MAPE: ", mape_4, "\n")
cat("MSE: ", mse_4, "\n")

cat("Model 5:\n")
cat("RMSE: ", rmse_5, "\n")
cat("MAPE: ", mape_5, "\n")
cat("MSE: ", mse_5, "\n")

cat("Model 6:\n")
cat("RMSE: ", rmse_6, "\n")
cat("MAPE: ", mape_6, "\n")
cat("MSE: ", mse_6, "\n")

cat("Model 7:\n")
cat("RMSE: ", rmse_7, "\n")
cat("MAPE: ", mape_7, "\n")
cat("MSE: ", mse_7, "\n")

cat("Model 8:\n")
cat("RMSE: ", rmse_8, "\n")
cat("MAPE: ", mape_8, "\n")
cat("MSE: ", mse_8, "\n")

cat("Model 9:\n")
cat("RMSE: ", rmse_9, "\n")
cat("MAPE: ", mape_9, "\n")
cat("MSE: ", mse_9, "\n")

cat("Model 10:\n")
cat("RMSE: ", rmse_10, "\n")
cat("MAPE: ", mape_10, "\n")
cat("MSE: ", mse_10, "\n")

cat("Model 11:\n")
cat("RMSE: ", rmse_11, "\n")
cat("MAPE: ", mape_11, "\n")
cat("MSE: ", mse_11, "\n")

cat("Model 12:\n")
cat("RMSE: ", rmse_12, "\n")
cat("MAPE: ", mape_12, "\n")
cat("MSE: ", mse_12, "\n")




###############################################################



time_lagged_data_5 <- data.frame(t_7 = data$time19,
                                 t_6 = data$time18,
                                 t_5 = lag(data$time20, 5),
                                 t_4 = lag(data$time20, 4),
                                 t_3 = lag(data$time20, 3),
                                 t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1:7), ]

time_lagged_data_6 <- data.frame(t_8 = data$time19,
                                 t_7 = data$time18,
                                 t_6 = lag(data$time20, 6),
                                 t_5 = lag(data$time20, 5),
                                 t_4 = lag(data$time20, 4),
                                 t_3 = lag(data$time20, 3),
                                 t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1:8), ]


time_lagged_data_7 <- data.frame(t_9 = data$time19,
                                 t_8 = data$time18,
                                 t_7 = lag(data$time20, 7),
                                 t_6 = lag(data$time20, 6),
                                 t_5 = lag(data$time20, 5),
                                 t_4 = lag(data$time20, 4),
                                 t_3 = lag(data$time20, 3),
                                 t_2 = lag(data$time20, 2),
                                 t_1 = lag(data$time20, 1),
                                 output = data$time20)[-c(1:9), ]




Scale_time__data_5 <- as.data.frame(lapply(time_lagged_data_5[1:8], normalize ))
Scale_time__data_6 <- as.data.frame(lapply(time_lagged_data_6[1:9], normalize ))
Scale_time__data_7 <- as.data.frame(lapply(time_lagged_data_7[1:10], normalize ))

Scale_time__data_5
Scale_time__data_6
Scale_time__data_7


train_norm_t_5 <- Scale_time__data_5 [2:380,]
test_norm_t_5 <- Scale_time__data_5 [381:463,]
test_norm_t_5
train_norm_t_6 <- Scale_time__data_6 [2:380,]
test_norm_t_6 <- Scale_time__data_6 [381:462,]
test_norm_t_6
train_norm_t_7 <- Scale_time__data_7 [2:380,]
test_norm_t_7 <- Scale_time__data_7 [381:461,]




new_model_1 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5+t_6+t_7, data = Scale_time__data_5, hidden = c(2))
new_model_2 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5+t_6+t_7, data = Scale_time__data_5, hidden = c(5,3))
new_model_3 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5 + t_6+t_7+t_8, data = Scale_time__data_6, hidden = c(8,6))
new_model_4 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5 + t_6+t_7+t_8, data = Scale_time__data_6, hidden = c(6,4))
new_model_5 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5 + t_6 + t_7+t_8+t_9, data = Scale_time__data_7, hidden = c(9))
new_model_6 <- neuralnet(output ~ t_1 + t_2 + t_3 + t_4+ t_5 + t_6 + t_7+t_8+t_9, data = Scale_time__data_7, hidden = c(5,10))


plot(new_model_1)
plot(new_model_2)
plot(new_model_3)
plot(new_model_4)
plot(new_model_5)
plot(new_model_6)


new_predictions_1 <- predict(new_model_1, Scale_time__data_5[, 1:8])
new_predictions_2 <- predict(new_model_2, Scale_time__data_5[, 1:8])
new_predictions_3 <- predict(new_model_3, Scale_time__data_6[, 1:9])
new_predictions_4<- predict(new_model_4, Scale_time__data_6[, 1:9])
new_predictions_5<- predict(new_model_5, Scale_time__data_7[, 1:10])
new_predictions_6<- predict(new_model_6, Scale_time__data_7[, 1:10])


original_train_output5 <- Scale_time__data_5[1:380,"output"] 
original_test_output5 <- Scale_time__data_5[381:463,"output"]

original_train_output6 <- Scale_time__data_6[1:380,"output"] 
original_test_output6 <- Scale_time__data_6[381:462,"output"]

original_train_output7 <- Scale_time__data_7[1:380,"output"] 
original_test_output7 <- Scale_time__data_7[381:461,"output"]

output_min5 <- min(original_train_output5)
output_max5 <- max(original_train_output5)
output_min6 <- min(original_train_output6)
output_max6 <- max(original_train_output6)
output_min7 <- min(original_train_output7)
output_max7 <- max(original_train_output7)

unScale_time__data_1 <- unnormalize(new_predictions_1, output_min5, output_max5)
unScale_time__data_2 <-unnormalize(new_predictions_2, output_min6, output_max6)
unScale_time__data_3 <- unnormalize(new_predictions_3, output_min7, output_max7)
unScale_time__data_4 <- unnormalize(new_predictions_4, output_min5, output_max5)
unScale_time__data_5 <-unnormalize(new_predictions_5, output_min6, output_max6)
unScale_time__data_6 <- unnormalize(new_predictions_6, output_min7, output_max7)


new_rmse_1 <- RMSE(original_test_output5, unScale_time__data_1)
new_rmse_2 <- RMSE(original_test_output6, unScale_time__data_2)
new_rmse_3 <- RMSE(original_test_output7, unScale_time__data_3)
new_rmse_4 <- RMSE(original_test_output5, unScale_time__data_4)
new_rmse_5 <- RMSE(original_test_output6, unScale_time__data_5)
new_rmse_6 <- RMSE(original_test_output7, unScale_time__data_6)


new_mape_1 <- MAPE(original_test_output5, unScale_time__data_1)
new_mape_2 <- MAPE(original_test_output6, unScale_time__data_2)
new_mape_3 <- MAPE(original_test_output7, unScale_time__data_3)
new_mape_4 <- MAPE(original_test_output5, unScale_time__data_4)
new_mape_5 <- MAPE(original_test_output6, unScale_time__data_5)
new_mape_6 <- MAPE(original_test_output7, unScale_time__data_6)

new_mse_1 <- MSE(original_test_output5, unScale_time__data_1)
new_mse_2 <- MSE(original_test_output6, unScale_time__data_2)
new_mse_3 <- MSE(original_test_output7, unScale_time__data_3)
new_mse_4 <- MSE(original_test_output5, unScale_time__data_4)
new_mse_5 <- MSE(original_test_output6, unScale_time__data_5)
new_mse_6 <- MSE(original_test_output7, unScale_time__data_6)


# Print error metrics
cat("new Model 1:\n")
cat("RMSE: ", new_rmse_1, "\n")
cat("MAPE: ", new_mape_1, "\n")
cat("MSE: ", new_mse_1, "\n")


cat("new Model 2:\n")
cat("RMSE: ", new_rmse_2, "\n")
cat("MAPE: ", new_mape_2, "\n")
cat("MSE: ", new_mse_2, "\n")

cat("new Model 3:\n")
cat("RMSE: ", new_rmse_3, "\n")
cat("MAPE: ", new_mape_3, "\n")
cat("MSE: ", new_mse_3, "\n")

cat("new Model 4:\n")
cat("RMSE: ", new_rmse_4, "\n")
cat("MAPE: ", new_mape_4, "\n")
cat("MSE: ", new_mse_4, "\n")

cat("new Model 5:\n")
cat("RMSE: ", new_rmse_5, "\n")
cat("MAPE: ", new_mape_5, "\n")
cat("MSE: ", new_mse_5, "\n")

cat("new Model 6:\n")
cat("RMSE: ", new_rmse_6, "\n")
cat("MAPE: ", new_mape_6, "\n")
cat("MSE: ", new_mse_6, "\n")

plot(original_test_output7 , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(unScale_time__data_6, ylab = " ", yaxt="n", type="l", col="green" ,main='Predicted Value Vs Expected Value Graph')
legend("topleft",
       c("Expected","Predicted"),
       fill=c("red","green")
)
#################################################################