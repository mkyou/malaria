rmse = function(real, pred){
  sqrt(mean((real - pred)^2))
}

mae = function(real, pred){
  mean(abs(real - pred))
}

mbe = function(real, pred){
  abs(mean(real - pred))
}

rae = function(real, pred){
  sum(abs(real - pred))/sum(abs(real - mean(real)))
}

rse = function(real, pred){
  sum((real - pred)^2)/sum((real - mean(real))^2)
}

nrmse = function(real, pred){
  rmse(real, pred)/max(real)
}

rmsle = function(real, pred){
  sqrt(mean((log(real + 1) - log(pred + 1))^2))
}

