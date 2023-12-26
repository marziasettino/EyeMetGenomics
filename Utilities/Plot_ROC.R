## Get the ROC curve
roc0 <- roc(test_data$Target, 
            predict(best_model_final, test_data, type = "prob")[,1], 
            levels = rev(levels(test_data$Target)))
roc0

## Now plot
plot(roc0, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE)