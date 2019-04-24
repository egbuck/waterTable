# plot comparison plots for all models
library(reshape2)
# list with confusion matrices for models.
# predicted are rows, actual are cols
#  -> Var1: predicted, Var2: actual
classNames <- c('functional', 'functional needs repair', 'non functional')

confMats <- list(KNN = data.frame(melt(matrix(c(6904, 580, 1455, 251, 328, 129, 878, 176, 4149),
                              nrow = 3, byrow = TRUE, 
                              dimnames = list(classNames, classNames)))),
                 Log = data.frame(melt(matrix(c(19158, 2263, 5646, 126, 131, 83, 2183, 516, 9494), 
                              nrow = 3, byrow = TRUE,
                              dimnames = list(classNames, classNames)))),
                 RandForest = data.frame(melt(matrix(c(5809, 473, 1034, 150, 265, 78, 495, 115, 3461), 
                                     nrow = 3, byrow = TRUE, 
                                     dimnames = list(classNames, classNames)))),
                 OverSamp = data.frame(melt(matrix(c(8370, 383, 1453, 539, 446, 196, 770, 118, 5197), 
                                                     nrow = 3, byrow = TRUE, 
                                                     dimnames = list(classNames, classNames)))))

confMats$KNN$rescale <- confMats$KNN$value / 
  c(rep(6904 + 251 + 878, 3), rep(580 + 328 + 176, 3), rep(1455 + 129 + 4149, 3))
confMats$Log$rescale <- confMats$Log$value / 
  c(rep(19158 + 126 + 2183, 3), rep(2263 + 131 + 516, 3), rep(5646 + 83 + 9494, 3))
confMats$RandForest$rescale <- confMats$RandForest$value / 
  c(rep(5809 + 150 + 495, 3), rep(473 + 265 + 115, 3), rep(1034 + 78 + 3461, 3))
confMats$OverSamp$rescale <- confMats$OverSamp$value / 
  c(rep(8370 + 539 + 770, 3), rep(383 + 446 + 118, 3), rep(1453 + 196 + 5197, 3))

colnames(confMats$KNN) <- c('Predicted', 'Actual', 'value', 'Percent')
colnames(confMats$Log) <- c('Predicted', 'Actual', 'value', 'Percent')
colnames(confMats$RandForest) <- c('Predicted', 'Actual', 'value', 'Percent')
colnames(confMats$OverSamp) <- c('Predicted', 'Actual', 'value', 'Percent')

# array of accuracy scores for models.
accs <- c(0.7664, 0.7268, 0.8026, 0.802)   # KNN, Log, then RandForest


library(ggplot2)
plotConf <- function(data, title) {
  print(ggplot(data, aes(Actual, Predicted)) + 
    geom_tile(aes(fill = Percent), colour = 'white') + 
    geom_text(aes(label = paste(round(Percent * 100, 1), '%', sep = ""))) +
    ylim(rev(levels(data$Predicted))) +
    scale_fill_gradient(low = "white", high = "steelblue") + ggtitle(title))
}
setwd('../Paper/Figures/')
plotConf(confMats$KNN, title = "KNN Classification")
ggsave('KNNConfMatrix.png')
plotConf(confMats$Log, title = "Logistic Regression")
ggsave('LogConfMatrix.png')
plotConf(confMats$RandForest, title = "Random Forest")
ggsave('RandForestConfMatrix.png')
plotConf(confMats$OverSamp, title = "Random Forest with Over-Sampling")
ggsave('OverSampleConfMatrix.png')


plotBars <- function(data, title) {
  print(ggplot(data, aes(Predicted, Percent * 100)) + 
          geom_bar(aes(fill = Model), position = 'dodge', stat = 'identity') + 
          ylab('Percent') + ggtitle(title))
}

library(data.table)
allData <- data.frame(rbindlist(confMats))

allData$Model <- c(rep('KNN', 9), 
                   rep('Logistic', 9),
                   rep('Random Forest', 9),
                   rep('Over Sampling', 9))

functional <- allData[allData$Actual == 'functional',
                      c('Model', 'Predicted', 'Percent')]
broken <- allData[allData$Actual == 'functional needs repair',
                  c('Model', 'Predicted', 'Percent')]
replace <- allData[allData$Actual == 'non functional',
                   c('Model', 'Predicted', 'Percent')]

plotBars(functional, 'Actual Class: Functional')
ggsave('FunctionalBarComps.png')
plotBars(broken, 'Actual Class: Functional Needs Repair')
ggsave('BrokenBarComps.png')
plotBars(replace, 'Actual Class: Non Functional')
ggsave('ReplaceBarComps.png')



water <- read.csv('../../Data/ReducedWaterTraining.csv')
library(gridExtra)
p1 <- ggplot(water[water$population < 2000,], 
       aes(x = population)) + geom_histogram(bins = 20) + ylim(c(0, 34000))
#ggsave('PopulationHist.png')
p2 <- ggplot(water, aes(x = gps_height)) + 
  geom_histogram(bins = 20) + ylim(c(0, 34000))
#ggsave('gps_heightHist.png')
p3 <- ggplot(water, 
       aes(x = longitude)) + geom_histogram(bins = 20) + ylim(c(0, 34000))
#ggsave('longitudeHist.png')
p4 <- ggplot(water, 
       aes(x = latitude)) + geom_histogram(bins = 20) + ylim(c(0, 34000))
#ggsave('latitudeHist.png')
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave('HistsNotNormal.png')
