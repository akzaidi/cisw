## -----import-flights, message = FALSE------------------------------------
library(nycflights13)
flights_xdf <- rxImport(inData = flights, outFile = "flights.xdf",
                        overwrite = TRUE)
rxGetInfo(flights_xdf, getVarInfo = TRUE)

## -----rxsummary, message = FALSE-----------------------------------------
rxSummary(~ ., data = flights_xdf)

## -----factors, messge = FALSE--------------------------------------------
rxFactors(inData = flights_xdf,
          outFile = flights_xdf,
          factorInfo = list(
              carrier_F = list(varName = "carrier"),
              origin_F = list(varName = "origin"),
              dest_F = list(varName = "dest")),
          overwrite = TRUE)
# rxSummary(~ ., data = flights_xdf)

## -----summary-multiple---------------------------------------------------
# rxSummary(~arr_delay + dep_delay, data = flight_xdf)
rxSummary(arr_delay ~ origin_F, data = flights_xdf)

## -----quantile, message = FALSE------------------------------------------
rxQuantile(varName = "arr_delay", data = flights_xdf)
lapply(c("arr_delay", "dep_delay"), rxQuantile, data = flights_xdf)


## -----cross-tabs, message = F--------------------------------------------
rxCrossTabs( ~ origin_F : carrier_F, data = flights_xdf)
# rxCrossTabs( ~ origin_F : F(month), data = flights_xdf)


## -----cube, message = FALSE----------------------------------------------
rxCube(arr_delay ~ carrier_F : F(month), data = flights_xdf,
       means = TRUE)
# rxCube(arr_delay ~ carrier_F : F(month), data = flights_xdf)

## -----plot-cube, message = FALSE, warning = F----------------------------
library(ggplot2)
library(magrittr)
rxCube(arr_delay ~ carrier_F:F(month):origin_F, data = flights_xdf,
       means = TRUE, returnDataFrame = TRUE) %>% 
  ggplot(aes(x = F_month, y = arr_delay)) + 
  geom_point(aes(size = Counts, color = origin_F, alpha = 0.5)) + 
  facet_wrap(~carrier_F) + theme_bw()


## -----data-step, message = FALSE-----------------------------------------
rxDataStep(inData = flights_xdf,
           outFile = flights_xdf,
           transforms = list(date = as.Date(paste(year, month, day, sep = "-")),
                             dayOfWeek = format(date, format = "%A")),
           overwrite = TRUE)


## -----dow, message = F---------------------------------------------------
rxFactors(inData = flights_xdf,
          outFile = flights_xdf,
          factorInfo = list(
              dayOfWeek_F = list(varName = "dayOfWeek",
                                 levels = c("Sunday", "Monday", "Tuesday",
                                            "Wednesday", "Thursday", "Friday",
                                            "Saturday"))
          ),
          overwrite = TRUE
)


## -----split, message = FALSE---------------------------------------------
rxDataStep(inData = flights_xdf,
           outFile = flights_xdf,
           transforms = list(was_delayed = factor(ifelse(arr_delay > 0,
                                                         1, 0), 
                                                  levels = c("0", "1"))),
           overwrite = TRUE)



## -----splitting, message = F---------------------------------------------
train_xdf <- rxDataStep(inData = flights_xdf,
                        outFile = "train.xdf",
                        rowSelection = month <= 6,
                        overwrite = TRUE)

rxDataStep(inData = flights_xdf,
           outFile = "test.xdf",
           rowSelection = month > 6,
           overwrite = TRUE) -> test_xdf



## -----dtree, message = FALSE---------------------------------------------

delay_prediction <- rxDTree(was_delayed ~ carrier_F + 
                              date + dayOfWeek_F + 
                              origin_F + dest_F, 
                            method = "class", pruneCp = "auto",
                            data = train_xdf)
# plot(RevoTreeView::createTreeView(delay_prediction))


## -----dforest, message = FALSE-------------------------------------------

delay_prediction_forest <- rxDForest(was_delayed ~ carrier_F + 
                                     date + dayOfWeek_F + 
                                     origin_F + dest_F, 
                                   method = "class", nTree = 10,
                                   data = train_xdf)


## -----sgb, message = FALSE-----------------------------------------------

delay_prediction_sgb <- rxBTrees(was_delayed ~ carrier_F +
                                   date + dayOfWeek_F +
                                   origin_F + dest_F, 
                                   method = "class", nTree = 10,
                                   data = train_xdf)


## -----predict, message = FALSE-------------------------------------------
rxPredict(delay_prediction, 
          outData = "predict.xdf", 
          writeModelVars = TRUE, 
          data = test_xdf, 
          predVarNames = c("0_tree", "1_tree"),
          overwrite = TRUE) -> predict_xdf


## -----predict-forest, message = FALSE------------------------------------

rxPredict(delay_prediction_forest, 
          outData = "predict.xdf", 
          writeModelVars = TRUE, 
          predVarNames = c("0_forest", "1_forest", "delay_pred_forest"),
          data = test_xdf, type = "prob") -> predict_xdf



## -----predict-sgb, message = FALSE---------------------------------------

rxPredict(delay_prediction_sgb, 
          outData = "predict.xdf", 
          writeModelVars = TRUE, 
          predVarNames = c("1_sgb"),
          data = test_xdf, type = "prob") -> predict_xdf



## -----binary-convert, message = FALSE------------------------------------
rxDataStep(inData = predict_xdf@file,
           outFile = predict_xdf,
           transforms = list(arrival_delay = as.integer(as.character(was_delayed))),
           overwrite = TRUE)


## -----roc-curve, message = FALSE-----------------------------------------
rxRocCurve(actualVarName = "arrival_delay", 
           predVarNames = c("1_tree", "1_forest", "1_sgb"), 
           data = predict_xdf)


