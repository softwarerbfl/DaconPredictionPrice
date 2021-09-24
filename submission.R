library(lubridate) #날짜 연산 package
library(kernlab)
library(e1071)
library(xgboost)
setwd("c:/AI/public_data")
submission <- read.csv("sample_submission.csv",header=TRUE)
tr <- read.csv("train.csv")
tr <- tr[,-2]
tr$date <- as.Date(tr$date)

#nmae측정 함수
nm <- function(prediction, answer){
  nmae <- abs((prediction-answer)/answer)
  out <- c() #Inf인 값들의 index
  for(i in 1:38){
    if(nmae[i]==Inf) {
      out <- c(out, i)
    }
  }
  nmae <- nmae[-c(out)]
  score <- rowMeans(nmae)
  return(score)
}

######train데이터 준비######
#n=1~21-> 품목 개수 (1주뒤 예측) 모델 return 
xgB <- function(n,wk){
  #train할 날짜
  date <- c(seq(as.Date('2017/09/28','%Y/%m/%d'), 
                as.Date('2017/12/01','%Y/%m/%d'),1),
            seq(as.Date('2018/09/28','%Y/%m/%d'), 
                as.Date('2018/12/01','%Y/%m/%d'),1),
            seq(as.Date('2019/09/28','%Y/%m/%d'), 
                as.Date('2019/12/01','%Y/%m/%d'),1))
  tr_date <- matrix(rep(0,len=11700),nrow=60,ncol=195) #날짜data포함
  tr_date <- as.data.frame(tr_date)
  colnames(tr_date) <- date
  #train할 날짜데이터를 가지는 DF->tr_date
  for(i in 1:195){
    a <- c()
    day <- c()
    day <- date[i]
    a <- date[i]+7*wk #예측할 label값(wk주뒤)
    a <- c(a,day-years(1)) #1년전 데이터
    a <- c(a, day-seq(56,84,1))
    a <- c(a, day-seq(1,29,1)-years(1))
    tr_date[,i] <- a
  }
  rownames(tr_date) <- c('label','1year',56:84,1:29)
  tr_date <- t(tr_date)
  tr_date <- as.data.frame(tr_date)
  tr_price <- matrix(rep(0,len=11700),nrow=195,ncol=60) #가격데이터를 담은 DF
  tr_price <- as.data.frame(tr_price)
  #train할 가격데이터를 가지는 DF->tr_price
  for(i in 1:195){
    for(j in 1:60){
      tr_price[i,j] <- tr[tr_date[i,j]==tr$date,1+2*n]
    }
  }
  subset <- subset(tr,tr[,1+2*n]!=0)
  laplace <- mean(subset[,1+2*n]) #원하는 농작물행의 평균값
  for(i in 1:195){
    for(j in 1:60){
      #0인 값들을 각 농작물의 평균으로 대체
      if(tr_price[i,j]==0){
        tr_price[i,j] <- laplace
      }
    }
  }
  colnames(tr_price) <-c('label','1year',56:84,1:29)
  rownames(tr_price) <- date
  
  subset1 <- subset(tr_price$label,tr_price$label!=0)
  laplace1 <- mean(subset1)
  for(i in 1:length(tr_price$label)){
    if(tr_price$label[i]==0){
      tr_price$label[i] <- laplace1
    }
  }
  train.data <- xgb.DMatrix(data=as.matrix(tr_price[,2:60]),
                            label=tr_price[,1])
  model <- xgboost(data=train.data,
                   booster="gblinear",
                   max.depth=8,
                   nthread=2,
                   nrounds=20,
                   eval.metric="mae",
                   objective="reg:linear")
  return(model)
}

######test데이터 준비######
library(psych)#산포도와 상관관계, 히스토그램 등을 모두 보여주는 함수
library(MASS)
date2 <- seq(as.Date('2020/09/29','%Y/%m/%d'), #test할 데이터의 날짜
             as.Date('2020/11/05','%Y/%m/%d'),1)
prediction <- function(n,wk){
  ts_date <- matrix(rep(0,len=2280),nrow=60,ncol=38) #날짜data포함
  ts_date <- as.data.frame(ts_date)
  colnames(ts_date) <- date2
  #train할 날짜데이터를 가지는 DF
  for(i in 1:38){
    b <- c()
    day <- c()
    day <- date2[i]
    b <- date2[i]+7*wk #예측할 label값(wk주뒤)
    b <- c(b,day-years(1)) #1년 전 데이터
    b <- c(b,day-seq(56,84,1))
    b <- c(b, day-seq(1,29,1)-years(1))
    
    ts_date[,i] <- b
  }
  rownames(ts_date) <- c('label','1year',56:84,1:29)
  ts_date <- t(ts_date)
  
  ts_price <- matrix(rep(0,len=2280),nrow=38,ncol=60) #가격데이터를 담은 DF
  ts_price <- as.data.frame(ts_price)
  for(i in 1:38){
    for(j in 2:60){
      ts_price[i,j] <- tr[ts_date[i,j]==tr$date,1+2*n]
    }
  }
  subset <- subset(tr,tr[,1+2*n]!=0)
  laplace <- mean(subset[,1+2*n])
  for(i in 1:38){
    for(j in 1:60){
      #값이 0인 부분을 평균값으로 대체
      if(ts_price[i,j]==0){
        ts_price[i,j] <- laplace
      }
    }
  }
  
  colnames(ts_price) <-c('label','1year',56:84,1:29)
  rownames(ts_price) <- date2
  ts_price
  test.data <- xgb.DMatrix(data=as.matrix(ts_price[,2:60]),
                           label=ts_price$label)
  ml <- xgB(n,wk)#xgBoost모델
  pred <- predict(ml,test.data)
  return(pred)
} #예측값을 반환해주는 함수


######제출 데이터 생성######
#mx <- matrix(rep(0,len=798),ncol=38,nrow=21)
#mx <- as.data.frame(mx)
#rownames(mx) <- c("배추","무","양파","건고추","마늘"
#                  ,"대파","얼갈이배추","양배추","깻잎"
#                  ,"시금치","미나리","당근","파프리카","새송이"
#                  ,"팽이버섯","토마토","청상추","백다다기"
#                  ,"애호박","캠벨얼리","샤인마스캇")
#colnames(mx) <- date2+7
#for(i in 1:21){
#  mx[i,] <- prediction(i,1)  
#}

for(a in 1:21){
  actual <- prediction(a,1) #1주뒤 a번째 농산물의 예측 가격들
  for(b in 1:38){
    submission[3*b-2,a+1] <- actual[b]
  }
  actual <- prediction(a,2) #2주뒤 a번째 농산물의 예측 가격들
  for(b in 1:38){
    submission[3*b-1,a+1] <- actual[b]
  }
  actual <- prediction(a,3) #2주뒤 a번째 농산물의 예측 가격들
  for(b in 1:38){
    submission[3*b,a+1] <- actual[b]
  }
}
colnames(submission) <-c("예측대상일자","배추_가격(원/kg)","무_가격(원/kg)",
                         "양파_가격(원/kg)","건고추_가격(원/kg)","마늘_가격(원/kg)",
                         "대파_가격(원/kg)","얼갈이배추_가격(원/kg)","양배추_가격(원/kg)",
                         "깻잎_가격(원/kg)","시금치_가격(원/kg)","미나리_가격(원/kg)",
                         "당근_가격(원/kg)","파프리카_가격(원/kg)","새송이_가격(원/kg)",
                         "팽이버섯_가격(원/kg)","토마토_가격(원/kg)","청상추_가격(원/kg)",
                         "백다다기_가격(원/kg)","애호박_가격(원/kg)","캠벨얼리_가격(원/kg)",
                         "샤인마스캇_가격(원/kg)")

write.csv(submission,"c:/AI/public_data/Submission3.csv")
######NMAE값 돌려보기######
real_date <- date2+7 #1주일 뒤 음식의 실제 가격
real_price <- subset(tr, date %in% real_date) #기간내의 음식들의 실제 가격
NM <- c() #nmae값 담기
for(i in 1:21){
  NM <- c(NM,nm(mx[i,],real_price[,2*i+1]))
}
mean(NM)
