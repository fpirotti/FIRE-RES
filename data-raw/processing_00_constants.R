
classesHCL <- c(
  "1" = "11 - Continuous urban fabric",
  "2" = "12 - Discontinuous urban fabric",
  "3" = "21 - Industrial or commercial units",
  "4" = "22 - Road and rail networks and associated land",
  "5" = "23 - Port areas",
  "6" = "24 - Airports",
  "7" = "31 - Mineral extraction sites",
  "8" = "32 - Dump sites",
  "9" = "33 - Construction sites",
  "10" = "141 - Green urban areas",
  "11" = "142 - Sport and leisure facilities",
  "12" = "211 - Non-irrigated arable land",
  "13" = "212 - Permanently irrigated arable land",
  "14" = "213 - Rice fields",
  "15" = "221 - Vineyards",
  "16" = "222 - Fruit trees and berry plantations",
  "17" = "223 - Olive groves",
  "18" = "231 - Pastures",
  "19" = "241 - Annual crops associated with permanent crops",
  "20" = "242 - Complex cultivation patter",
  "21" = "243 - Land principally occupied by agriculture with significant areas of natural vegetation",
  "22" = "244 - Agro-forestry areas",
  "23" = "311 - Broad-leaved forest",
  "24" = "312 - Coniferous forest",
  "25" = "313 - Mixed forest",
  "26" = "321 - Natural grasslands",
  "27" = "322 - Moors and heathland",
  "28" = "323 - Sclerophyllous vegetation",
  "29" = "324 - Transitional woodland-shrub",
  "30" = "331 - Beaches, dunes, sand",
  "31" = "332 - Bare rocks",
  "32" = "333 - Sparsely vegetated areas",
  "33" = "334 - Burnt areas",
  "34" = "335 - Glaciers and perpetual snow",
  "35" = "411 - Inland marshes",
  "36" = "412 - Peat bogs",
  "37" = "421 - Salt marshes",
  "38" = "422 - Salines",
  "39" = "423 - Intertidal flats",
  "40" = "511 - Water courses",
  "41" = "512 - Water bodies",
  "42" = "521 - Coastal lagoons",
  "43" = "522 - Estuaries"

)

## E0, B0 e B1 non c'è forest cover,  D4 non fa parte di EU
nonfare <- c("E0",  "D4", "B0", "B1")
### have to fix tiles that do not have a lot of EU data so must be skipped
classification.lut <- list("B4"="B3", C4="C3",  E4="E3" , F1="E1", E0="E1")



mean.pred.intervals <- function(x, y, pred.x) {
  n <- length(y) # Find sample size
  lm.model <- lm(y ~ x) # Fit linear model
  y.fitted <- lm.model$fitted.values # Extract the fitted values of y

  # Coefficients of the linear model, beta0 and beta1
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]

  pred.y <- b1 * pred.x + b0 # Predict y at the given value of x (argument pred.x)

  # Find SSE and MSE
  sse <- sum((y - y.fitted)^2)
  mse <- sse / (n - 2)

  t.val <- qt(0.975, n - 2) # Critical value of t

  mean.se.fit <- (1 / n + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the mean estimate
  pred.se.fit <- (1 + (1 / n) + (pred.x - mean(x))^2 / (sum((x - mean(x))^2))) # Standard error of the prediction

  # Mean Estimate Upper and Lower Confidence limits at 95% Confidence
  mean.conf.upper <- pred.y + t.val * sqrt(mse * mean.se.fit)
  mean.conf.lower <- pred.y - t.val * sqrt(mse * mean.se.fit)

  # Prediction Upper and Lower Confidence limits at 95% Confidence
  pred.conf.upper <- pred.y + t.val * sqrt(mse * pred.se.fit)
  pred.conf.lower <- pred.y - t.val * sqrt(mse * pred.se.fit)

  # Beta 1 Upper and Lower Confidence limits at 95% Confidence
  b1.conf.upper <- b1 + t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))
  b1.conf.lower <- b1 - t.val * sqrt(mse) / sqrt(sum((x - mean(x))^2))

  # Build data.frame of upper and lower limits calculated above, as well as the predicted y and beta 1 values
  upper <- data.frame(rbind(round(mean.conf.upper, 2), round(pred.conf.upper, 2), round(b1.conf.upper, 2)))
  lower <- data.frame(rbind(round(mean.conf.lower, 2), round(pred.conf.lower, 2), round(b1.conf.lower, 2)))
  fit <- data.frame(rbind(round(pred.y, 2), round(pred.y, 2), round(b1, 2)))

  # Collect all into data.frame and rename columns
  results <- data.frame(cbind(lower, upper, fit), row.names = c('Mean', 'Prediction', 'Coefficient'))
  colnames(results) <- c('Lower', 'Upper', 'Fit')

  return(results)
}


