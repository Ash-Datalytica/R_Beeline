## Нормализовать числовую переменную в диапазон (-1,1)
## mode="median" Тогда после нормализации Медиана нового распределения будет равна targetValue
## mode="mean" Тогда после нормализации Среднее нового распределения будет равно targetValue
normalizeTanh <- function (x, mode="median", targetValue=0.5) {
    if (mode == "median") {
        alpha <- median (x)/atanh(targetValue)
    } else if (mode == "mean") {
        alpha <- mean (x)/atanh(targetValue)
    } else {
        stop ("не известный режим mode  в normalizeTanh")
    }
    tanh(x/alpha)
}


## Нарисовать график с ROC кривой, используя ggplot2
## predicted -вектор предсказанных значений
## expected - вектор ожидаемых значенийъ
myPlotROC <- function (predicted, expected, title=NULL) {
    require(ROCR)    
    #ROC curve - сделать сводный по моделям
    pred <- prediction (predicted, expected)
    pe <- performance(pred, "tpr", "fpr")
    au <- performance(pred, "auc")@y.values[[1]]
    pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
    p <- ggplot(pd, aes(x=fpr, y=tpr))
    p <- p + geom_line(colour="red")
    p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
    
    p <- p + theme(plot.title=element_text(size=10))
    p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
    p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                      label=paste("AUC =", round(au, 2))) 
    if (!is.null(title)) {
        p <- p + ggtitle(title)    
    }
    
    p
}

## Нарисовать кривую обучения
## 
##

myPlotLearningCurve <- function (
    dataTrain, #data frame с тренировочными данными
    dataTest, # data frame с проверочными данными (с известными значениями целевой переменной)
    featuresList, #Спсиок фич, которые участвуют в обучении модели
    method, # тип модели (caret)
    title, # Заголовок графика
    tuneGrid, # data frame с параметрами модели (caret)
    targetVariable="y", #имя целевой переменной
    learningCurvePoints=10, # Количество точек для кривых обучения
    cvRepeats = 3, #Количество повторений в пререкрестной проверке
    seed = 150477, # Начальное значение счетчика случайных чисел
    ... # Прочие параметры caret::train()
) {
    require (caret)
    
    set.seed(seed)
    formula <- as.formula(paste0(targetVariable, " ~ ." ))
    res <- data.frame()
    res <- foreach  (m = ceiling(seq (nrow(dataTrain)*0.6, nrow(dataTrain), length.out = learningCurvePoints)), 
                     .combine=rbind) %dopar% {     
                         rows <- sample (1:nrow(dataTrain),m)    
                         mod <- caret::train (formula, method=method, 
                                              data = dataTrain[featuresList][rows, ],
                                              trControl = caret::trainControl(method = "cv", number=10, repeats=cvRepeats),
                                              trace = FALSE,
                                              tuneGrid = tuneGrid,
                                              ...
                         )    
                         
                         predictionsTrain <- predict (mod, newdata = dataTrain[featuresList][rows, ])
                         accTrain <- caret::confusionMatrix(predictionsTrain, dataTrain[,targetVariable][rows], positive="1")$overall[1]
                         predictions <- predict (mod, newdata = dataTest)
                         accTest <- caret::confusionMatrix(predictions, dataTest[,targetVariable], positive="1")$overall[1]
                         #res <- rbind(res, data.frame(m=m, errorTrain = 1-accTrain, errorTest = 1-accTest))
                         rbind(res, data.frame(m=m, errorTrain = 1-accTrain, errorTest = 1-accTest))
                     }
    
    p <- ggplot(aes (x=m, y=value, colour=variable, shape=variable), data = melt (res, id="m")) + 
        geom_line(size=1) + geom_point(size=5) +
        xlab("Размер обучающей выборки (m)") + 
        ylab ("Ошибка (1-Accuracy)") + ggtitle(title)+
        scale_colour_discrete(name="Выборка", labels=c("Обучающая", "Тестовая")) + 
        scale_shape_discrete(name="Выборка", labels=c("Обучающая", "Тестовая"))
    return(p)
}

## Нарисовать несколько графиков с попарным сравнением параметров
##
##
myPlotFeatures <- function (
    data, # data frame
    y, # столбец с целевой переменной
    featuresInGraph = 6 # кол-во параметров на одном графике
){
    require (ggplot2)
    require(Hmisc) #cut2
    #set.seed(20150926)
    #inEDA <- createDataPartition(dfTrainCM$y, p = 0.05, list = FALSE, times = 1)
    #ncol(dfNormNumeric) #41 шт
    nChunks <- (ncol(data) %/% featuresInGraph) + 1 # кол-во графиков
    #cut2(1:41, g=7, onlycuts=TRUE  )
    parts <- cut2(1:ncol(data), g=nChunks, onlycuts=TRUE  )
    parts[nChunks] <- parts[nChunks]+1 #для единообразия следующего цикла
    for (i in 1:nChunks) {
        fp <- featurePlot (x = data[parts[i]:(parts[i+1]-1)],
                           y = y,
                           plot="pairs", auto.key=list(columns=2))
        print (fp)
        #cat(i)
    }
}