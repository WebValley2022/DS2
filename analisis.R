library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(cobalt)
library(MatchIt)
library(randomForest)
library(reshape2)
library(corrplot)
library(scales)
setwd("C:/Users/samue/OneDrive/Desktop/WebValley/DS2")

ds <- read.csv('data/appa_hr_crop_withCreated.csv', header = T)

ds <- ds[ !grepl(glob2rx("*heat*"), colnames(ds)) & !grepl(glob2rx("*TimeSince*"),colnames(ds))  ]
ds <- ds[ , ! names(ds) %in% c("Temperature", "Relative_Humidity", "Pressure")]

colnames(ds)
co_ds <- cor( subset(ds, select= -Time))
co <- melt(co_ds)

co
ggplot(co, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "lightblue", mid="grey0",high = "red") +
  geom_text(aes(label = format(round(value, 2), nsmall = 4)), color = "white", size = 4) +
  coord_fixed()
