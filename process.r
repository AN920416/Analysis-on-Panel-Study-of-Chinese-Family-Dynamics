library(readr)
library(ggplot2)
library(dplyr)
library(estimatr)
rr2011_data <- read_csv("rr2011/rr2011_data.csv")
rr2012_data <- read_csv("rr2012/rr2012_data.csv")
rawdata = rr2011_data 


#-----------------------------------------------------Q1
cat("主樣本數：", length(rawdata$x01))

#age
birth = rawdata$a02a
birth = birth + 1911
age = 2011 - birth 
ggplot() +
  geom_histogram(mapping = aes(x = age), color="black", fill="white",binwidth=1) +
  labs(title = "年齡統計") + 
  xlab("年齡(歲)") +
  ylab("人數(人)")

#education
eduStage = rawdata$a03c
eduYear = c()
eduYear = c(eduYear, length(eduStage[eduStage == 1]))
eduYear = c(eduYear, length(eduStage[eduStage == 3]))
eduYear = c(eduYear, length(eduStage[eduStage == 4 | eduStage == 5]))
eduYear = c(eduYear, length(eduStage[eduStage == 6 | eduStage == 7 | eduStage == 8]))
eduYear = c(eduYear, length(eduStage[eduStage == 9 | eduStage == 10]))
eduYear = c(eduYear, length(eduStage[eduStage == 11]))
eduYear = c(eduYear, length(eduStage[eduStage == 12 | eduStage == 13]))
eduYear = c(eduYear, length(eduStage[eduStage == 14]))
eduYear = c(eduYear, length(eduStage[eduStage == 15]))

#將自學設定為大學以下平均受教育年數
avgUnderCollege = (eduYear[2]*6 + eduYear[3]*9 + eduYear[4]*12 + eduYear[5]*14 + eduYear[6]*15) / sum(eduYear[1:6])
avgUnderCollege = round(avgUnderCollege, 2)
eduYear = c(eduYear[1:3], length(eduStage[eduStage == 2]), eduYear[4:9])

#作圖
dfEdu = data.frame(Years = c("0", "6", "9", avgUnderCollege, "12", "14", "15", "16", "18", "23.325"), count = eduYear)
dfEdu$Years = factor(dfEdu$Years, levels = c("0", "6", "9", avgUnderCollege, "12", "14", "15", "16", "18", "23.325"))
ggplot(data=dfEdu, aes(x=Years, y=count)) +
  geom_bar(stat="identity",color="black", fill="white") + 
  labs(title = "教育程度統計") +
  xlab("受教育年數(年)") + 
  ylab("人數(人)")

#marriage
marrCode = rawdata$a16a
marr = c()
marr = c(marr, length(marrCode[marrCode == 1 | marrCode == 7]))
marr = c(marr, length(marrCode[marrCode == 3]))
marr = c(marr, length(marrCode[marrCode == 6 | marrCode == 10]))
marr = c(marr, length(marrCode[marrCode == 4 | marrCode == 9]))
marr = c(marr, length(marrCode[marrCode == 2]))
marr = c(marr, length(marrCode[marrCode == 5 | marrCode == 8]))

#作圖
dfMarr = data.frame(type = c("已婚", "單身", "喪偶", "離婚", "同居", "分居"), count = marr)
dfMarr$type = factor(dfMarr$type, levels = c("已婚", "單身", "喪偶", "離婚", "同居", "分居"))
ggplot(data=dfMarr, aes(x=type, y=count)) +
  geom_bar(stat="identity",color="black", fill="white") +
  labs(title = "婚姻狀況統計") + 
  xlab("婚姻狀況") + 
  ylab("人數(人)")


#-----------------------------------------------------Q2
data = rawdata
data = select(data,a03c, x01b, b12)
data = as.data.frame(data)
eduDict = c( 0, avgUnderCollege, 6, 9, 9, 12, 12, 12, 14, 14, 15, 16, 16, 18, 23.325)
for(i in (1:4885)){
  if(data[i,]$a03c < 16){
    data[i,]$a03c <- eduDict[data[i,]$a03c]
  }
}

#分組
g1 = data[data$x01b == 1,]
g2 = data[data$x01b == 2,]
g3 = data[data$x01b == 3,]
g4 = data[data$x01b == 4,]
g5 = data[data$x01b == 5,]
eduDict = c( 0, 6, 9, avgUnderCollege, 12, 14, 15, 16, 18, 23.325)
xlist = c("0", "6", "9", avgUnderCollege, "12", "14", "15", "16", "18", "23.325")

#group1
g1avg = c()
for(i in 1:length(eduDict)){
  eduDict[i]
  avg_value <- mean(g1[g1$a03c == eduDict[i], ]$b12)
  g1avg[i] = avg_value
}
g1df = data.frame(x = xlist, y = g1avg)
g1df$x = factor(g1df$x, levels = xlist)
ggplot(data = g1df,mapping = aes(x = x, y = y)) + geom_point() + 
  labs(title = "教育程度與平均兒女數分布(1953-64年出生)") +
  xlab("受教育年數(年)") + ylab("平均兒女數(人)")
y = g1avg[-c(4,10)]#去除 outliers
x = eduDict[-c(4,10)]
g1model = lm_robust(y ~ x)
summary(g1model)

#group2
g2avg = c()
for(i in 1:length(eduDict)){
  eduDict[i]
  avg_value <- mean(g2[g2$a03c == eduDict[i], ]$b12)
  g2avg[i] = avg_value
}
g2df = data.frame(x = xlist, y = g2avg)
g2df$x = factor(g2df$x, levels = xlist)
ggplot(data = g2df) + geom_point(mapping = aes(x = x, y = y)) + 
  labs(title = "教育程度與平均兒女數分布(1935-54年出生)") +
  xlab("受教育年數(年)") + ylab("平均兒女數(人)")
y = g2avg[-c(4,10)]#去除 outliers
x = eduDict[-c(4,10)]
g2model = lm_robust(y ~ x)
summary(g2model)

#group3
g3avg = c()
for(i in 1:length(eduDict)){
  eduDict[i]
  avg_value <- mean(g3[g3$a03c == eduDict[i], ]$b12)
  g3avg[i] = avg_value
}
g3df = data.frame(x = xlist, y = g3avg)
g3df$x = factor(g3df$x, levels = xlist)
ggplot(data = g3df) + geom_point(mapping = aes(x = x, y = y)) + labs(title = "教育程度與平均兒女數分布(1964-76年出生)") +
  xlab("受教育年數(年)") + ylab("平均兒女數(人)")
y = g3avg[-c(1,4)]#去除 outliers
x = eduDict[-c(1,4)]
g3model = lm_robust(y ~ x)
summary(g3model)

#group4
g4avg = c()
for(i in 1:length(eduDict)){
  eduDict[i]
  avg_value <- mean(g4[g4$a03c == eduDict[i], ]$b12)
  g4avg[i] = avg_value
}
g4df = data.frame(x = xlist, y = g4avg)
g4df$x = factor(g4df$x, levels = xlist)
ggplot(data = g4df) + geom_point(mapping = aes(x = x, y = y)) + labs(title = "教育程度與平均兒女數分布(1977-83年出生)") +
  xlab("受教育年數(年)") + ylab("平均兒女數(人)")
y = g4avg[-c(1,4)]#去除 outliers
x = eduDict[-c(1,4)]
g4model = lm_robust(y ~ x)
summary(g4model)

#group5
g5avg = c()
for(i in 1:length(eduDict)){
  eduDict[i]
  avg_value <- mean(g5[g5$a03c == eduDict[i], ]$b12)
  g5avg[i] = avg_value
}
g5df = data.frame(x = xlist, y = g5avg)
g5df$x = factor(g5df$x, levels = xlist)
ggplot(data = g5df) + geom_point(mapping = aes(x = x, y = y)) + labs(title = "教育程度與平均兒女數分布(主樣本滿25歲之子女)") +
  xlab("受教育年數(年)") + ylab("平均兒女數(人)")
y = g5avg[-c(1,2,4,7)]#去除 outliers
x = eduDict[-c(1,2,4,7)]
g5model = lm_robust(y ~ x)
summary(g5model)


#-----------------------------------------------------Q3
eduDict = c( 0, avgUnderCollege, 6, 9, 9, 12, 12, 12, 14, 14, 15, 16, 16, 18, 23.325)

#marriage
marrData = rawdata
marrData = select(marrData, a16a, a03c, a18)
marrData = marrData[marrData$a16a == 1 |marrData$a16a == 2,]

for(i in 1:2578){
  if(marrData[i,]$a03c < 16){
    marrData[i,]$a03c <- eduDict[marrData[i,]$a03c]
  }
  if(marrData[i,]$a18 < 16){
    marrData[i,]$a18 <- eduDict[marrData[i,]$a18]
  }
}
marrData = marrData[marrData$a18 < 50,]

#作圖
marrDf = data.frame(x = marrData$a03c, y = marrData$a18)
marrDf = as_tibble(marrDf)
ggplot(data = marrDf, mapping = aes(x = x, y = y)) + geom_point() + 
  geom_smooth(method = "lm", se = F) + labs(title = "主樣本與配偶的教育程度關聯") +
  xlab("主樣本受教育年數(年)") + ylab("配偶受教育年數(年)")
x = marrData$a03c
y = marrData$a18
marrModel = lm_robust(y ~ x)
summary(marrModel)

#child
childData = rawdata
childData = childData[childData$b12 > 0,]
childData = childData[childData$x01b == 2,]
childData = select(childData,a03c,b12,b13dc1,b13dc2,b13dc3,b13dc4,b13dc5,b13dc6)

avgChildEdu = c()
for(i in 1:1067){
  tmp = 0
  
  #年數轉換
  if(childData[i,]$a03c < 16){
    childData[i,]$a03c <- eduDict[childData[i,]$a03c]
  }
  if(childData[i,]$b12 > 6){
    childData[i,]$b12 = 6
  }
  
  #計算子女受教育年數平均
  for(j in 1:childData[i,]$b12){
    if(as.numeric(childData[i,j + 2]) < 50 && as.numeric(childData[i,j + 2]) > 0){
      tmp = tmp + eduDict[as.numeric(childData[i,j+2])]
    }
  }
  avgChildEdu[i] = tmp / childData[i,]$b12
}
avgChildEdu = avgChildEdu[childData$a03c < 25]
childData = childData[childData$a03c < 25,]
childData = childData[avgChildEdu > 0,]
avgChildEdu = avgChildEdu[avgChildEdu > 0]

#作圖
childDf = data.frame(x = childData$a03c, y = avgChildEdu)
childDf = as_tibble(childDf)
ggplot(data = childDf, mapping = aes(x = x, y = y)) + geom_point() + geom_smooth(method = "lm") + 
  labs(title = "主樣本與其子女之教育程度關聯") + 
  xlab("主樣本受教育年數(年)") +
  ylab("子女受教育年數(年)")
x = childData$a03c
y = avgChildEdu
childModel = lm_robust(y ~ x)
summary(childModel)

#-----------------------------------------------------Q4
data1 = rr2011_data
data1 = data1$a16a
data1 = data1[data1 < 11]
changeToMarr = length(data1[data1 == 7])
changeToSingle = length(data1[data1 == 9 | data1 == 10])
noPartner = length(data1[data1 == 3 | data1 == 4 | data1 == 6])
havePartner = length(data1[data1 == 1|data1 == 5|data1 == 8|data1 == 2])
changeToMarr
changeToSingle
noPartner
havePartner
cat("2011年由單身變為有伴侶的比率：", changeToMarr/(noPartner+changeToMarr))

data2 = rr2012_data
data2 = data2$a16a
data2 = data2[data2 < 11]
changeToMarr = length(data2[data2 == 7])
changeToSingle = length(data2[data2 == 9 | data2 == 10])
noPartner = length(data2[data2 == 3 | data2 == 4 | data2 == 6])
havePartner = length(data2[data2 == 1|data2 == 5|data2 == 8|data2 == 2])
changeToMarr
changeToSingle
noPartner
havePartner
cat("2012年由單身變為有伴侶的比率：", changeToMarr/(noPartner+changeToMarr))

#-----------------------------------------------------Q5

dataInc = rr2011_data
dataInc = select(dataInc,a10a,a03c)
dataInc = dataInc[dataInc$a10a < 9999990,]
dataInc = dataInc[dataInc$a03c < 50,]
dataInc = dataInc[dataInc$a10a < 1000000,]
dataInc = as.data.frame(dataInc)

#年數轉換
for(i in 1:4281){
  if(dataInc[i,]$a03c < 16){
    dataInc[i,]$a03c <- eduDict[dataInc[i,]$a03c]
  }
}

#作圖
x = dataInc$a03c
y = dataInc$a10a
ggplot(data = dataInc, mapping = aes(x = x, y = y)) + geom_point() + 
  geom_smooth(method = "lm") + labs(title = "受教育年數與薪資的關聯") +
  xlab("受教育年數(年)") + ylab("平均每月薪資(元)")
incomeModel = lm_robust(y~x)
summary(incomeModel)
