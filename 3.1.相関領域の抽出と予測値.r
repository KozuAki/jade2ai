# 相関係数の抽出領域の決定


if(tres.pvalue>0.10){
  # 抽出条件
  #2000~2018年のうち、2009年、2015年を除くと、n=17 p=0.05の両側確率で、0.482146以上が有意
  tres.tyusyutu<-0.482146  #ピアソンの相関係数に基づくもの ブリのみシビア
  # tres.tyusyutu<-0.60
  cond.tyusyutu<-!is.na(result.table$R)
  cond.tyusyutu<-abs(result.table$R) >=tres.tyusyutu &cond.tyusyutu  #T & T でないとＴにならず、　ＲがＮＡだとＮＡ＆ＦはＦ
  #グリッドを抽出
  area1<-result.table[cond.tyusyutu,]
  # test2<-test[test$lon>=136,]  #136度以東
}else{
  area1<-na.omit(result.table)
}


#直近データで、水温差を計算するために、書き出す
outputnames<-paste0(wdout.ocean,"/3.1.area.tyusyutu_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),
                    ".csv")
write.csv(area1,outputnames,row.names=F)

#位置を確認して、不適な場所（北朝鮮前など）を適宜削除。
# ***checked.csvとなったファイルを読み込み直して　area2へ。
rm(area2)
file.checked<-dir(path=wdout.ocean,full.names=T)
file.checked <-file.checked[grep("\\checked.csv$",file.checked )]
file.checked
# file.checked<-file.checked[length(file.checked)]  #最新の「checked」に絞る
library(stringr)
file.checked<-file.checked[which.max(str_sub(file.checked,-20,-13))]  #最新の「checked」に絞る
# file.checked<-file.checked[1]  #最新の「checked」に絞る
cat("読み込むファイル:::",file.checked)
(area2<-read.csv(file.checked,header=T))





#jd2 まずjd.slideを作成=======
jd.slide<-jade.df.half
jd.slide$month.slide<-month.slide
# .obj ha objective(catch) nado to hikaku surutameno kaikyouno  kakono nnitiji
names(jd.slide)[which(names(jd.slide)=="year")]<-"year.obj"
names(jd.slide)[which(names(jd.slide)=="month")]<-"month.obj"

slide.index<- month.i - month.slide%%12  #例えば、初期値が8月で、-4か月か、-14カ月か
if(slide.index>0){
  jd.slide$month<- jd.slide$month.obj + month.slide%%12
  jd.slide$year<-  jd.slide$year.obj  + month.slide%/%12
}else{
  jd.slide$month<- jd.slide$month.obj + month.slide%%12 -12
  jd.slide$year<-  jd.slide$year.obj  + month.slide%/%12 +1
}
#month.slideが０ならば、何も変化しない
jd.slide<-subset(jd.slide, month==month.i & depth==depth.i)

# month.new==month.i で絞り込まれているので
if(month.i>=tres.month){
  jd.slide$fyear<-jd.slide$year
}else{ #つまり　month.i < tres.month アオリイカでいえば、8月以降の漁期漁獲量が4月の水温（漁獲対象のイカの世代でいえば前の世代）で決定する
  # jd.slide$fyear<-jd.slide$year-1 #同世代資源　スルメイカ　12月以降の漁獲を集計、翌年2月と比較
  jd.slide$fyear<-jd.slide$year-1+future.index　#異世代資源　アオリイカ　4月の水温が、子の生育を経由して、次の代の資源変動へ
}

#エラー対策
if(nrow(jd.slide)==0){cat("jd.slide が　NULL fyear.v or depth.i check","\n")}
# jd.slide[1:3,]




#負の相関関係のある領域の平均水温の時系列 jd.nega=====
area.nega<-area2[area2$R<0,]
jd.nega<-merge(area.nega[,c("lon.half","lat.half")],
               jd.slide,  
               all.x=T)
if(nrow(jd.nega)!=0){
  jd.nega<-aggregate(data=jd.nega,
                     temp~year+month+day+year.obj+month.obj+depth+fyear,
                     FUN="mean",na.rm=T)
  colnames(jd.nega)<-c("year","month","day","year.obj","month.obj","depth","fyear","temp.nega" )
}else{
  colnames(jd.nega)<-c("year","month","day","year.obj","month.obj","depth","fyear","temp.nega")
}

#正の相関関係のある領域の平均水温の時系列 jd.posi=====
area.posi<-area2[area2$R>0,]
jd.posi<-merge(area.posi[,c("lon.half","lat.half")],
               jd.slide,
               all.x=T)
if(nrow(jd.posi)!=0){
  jd.posi<-aggregate(data=jd.posi,
                     temp~year+month+day+year.obj+month.obj+depth+fyear,
                     FUN="mean",na.rm=T)
  colnames(jd.posi)<-c("year","month","day","year.obj","month.obj","depth","fyear","temp.posi")
}else{
  colnames(jd.posi)<-c("year","month","day","year.obj","month.obj","depth","fyear","temp.posi")
}


#正、負の相関のareaを統合 jd2========
if(nrow(jd.nega)!=0 & nrow(jd.posi)!=0){
  jd2<-merge(jd.nega,jd.posi,all.x=T)  
}else if(nrow(jd.nega)==0){   #有意な負の相関領域がないとき
  jd2<-jd.posi
}else{　　　#有意な正の相関領域がないとき　　20190615にどちらの場合にも対応するように修正
  jd2<-jd.nega
}

jd2$month.slide<-month.slide
# rm(jd.nega,jd.posi)

#年別の冷水暖水の平均値の差
if(nrow(jd.nega)!=0 & nrow(jd.posi)!=0 ) jd2$temp.dif<-with(jd2,temp.posi-temp.nega)


#year.excludeを除く前の　水温差を取る海域の正、負の相関を示す海域の平均水温
outputnames<-paste0(wdout,"/",
                    "3.1.jd2_正負の海域水温_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),".csv")
write.csv(jd2,outputnames,row.names = F)

#year.excludeを除く
jd2<-jd2[!(jd2$fyear%in%year.exclude),]
jd2<-merge(jd2,catch.df,by="fyear",all.x=T)

# =======

#========

#すべてがNAの列を削除
jd2<-jd2[,!apply(is.na(jd2), 2, all)]　　#jd2がうまく作れていれば不要な気がする
#http://graspmyidioticlife.hatenablog.com/entry/2017/03/05/151501

library(reshape2)
jd3<-melt(jd2,id=c("fyear","year","month","day","year.obj","month.obj","depth","objective","pref","month.slide"))

if(length(unique(jd3$variable))==1) {
  names(jd3)[which(names(jd3)=="value")]<-"temp"
}else{jd3<-subset(jd3, variable=="temp.dif")  #水温差との相関を取る場合
names(jd3)[which(names(jd3)=="value")]<-"temp"  #本当は"temp.diff"の方がいいが、あとを変えるのが面倒なので、temp
}

# unique(jd3$variable)

jd3.1<-jd3[jd3$fyear%in%fyear.v,]
res.lm<-lm(objective~temp,data=jd3.1)
# jd3.1$est<-res.lm$fitted.values　#objectiveにNAがあると対応できない
jd3.1$est<-predict(res.lm, newdata=jd3.1)

# est.target<-predict(res.lm,interval="prediction")
# est.target<-predict(res.lm,interval="confidence")



#相関プロットを表示

outputnames<-paste0(wdout,"/3.1.相関プロット_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),
                    ".pdf")
pdf(outputnames)
# dev.new()
with(jd3.1,
     plot(temp,objective#,
          # xlim=c(0,7)
     )
) 
abline(res.lm)
dev.off()


#実績値と予測値の経年変化
dev.new()
with(jd3.1,
     plot(year,objective,
          type="b"))
with(jd3.1,
     points(year,est,
            type="b",
            col="green",lty=2))


#=============

#実績値と推定値の差分=====
# jd3.1$anom<-res.lm$residuals  #objectiveにNAがあると対応できない
jd3.1$anom<-with(jd3.1,objective-est)

dev.new()
with(jd3.1,
     plot(year,anom,
          type="b"))
abline(h=0)

#差分への再回帰のモデル　　重回帰とどう違う？



#jd3.2 関係性の推計に用いなかった期間の水温データからも、予測値を計算　objectiveが漁獲量のときのみ、scaleデータは対処できない====
# catch3.2  catch.dfは漁期年で切っているので、全期間====
# catch3.2<-with(catch.df.ori,
#                catch.df.ori[ month%in%month.v.catch, ]
#                )
# catch3.2<-aggregate(data=catch3.2,
#                     cbind(catch)~fyear+pref,
#                     FUN="sum",na.rm=T)
# catch3.2<-catch3.2[,c("fyear","catch","pref")]
# names(catch3.2)<-c("fyear","objective","pref")
# 
# jd3.2<-merge(jd3[,-1*which(names(jd3)%in%c("objective","pref"))],
#              catch3.2,
#              by=c("fyear"),all=T) #estを結合
# jd3.2$est<-with(jd3.2,
#                 res.lm$coefficients[1]+temp*res.lm$coefficients[2])
#=======

#実績値と予測値の経年変化=======
# jd3.2 関係性の推計に用いなかった年(year.exclude)の水温データを含めて、予測値を計算
jd3.2<-merge(jd3[,-1*which(names(jd3)%in%c("objective","pref"))],
             catch.df,
             by=c("fyear"),all=T) #estを結合
# jd3.2$est<-with(jd3.2,res.lm$coefficients[1]+temp*res.lm$coefficients[2])
jd3.2$est<-predict(res.lm, newdata=jd3.2)




outputnames<-paste0(wdout,"/3.1.実績値と予測値_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),
                    ".pdf")
pdf(outputnames)
# dev.new()
with(jd3.2,
     plot(year,objective,
          type="b"))
with(jd3.2,
     points(year,est,
            type="b",
            col="red",lty=2))
with(jd3.2[jd3.2$fyear%in%fyear.v,],
     points(year,est,
            type="b",
            col="green",lty=2))
dev.off()

#実績値と推定値の差分
jd3.2$anom<-with(jd3.2,objective-est)


outputnames<-paste0(wdout,"/3.1.残差プロット_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),
                    ".pdf")
pdf(outputnames)
# dev.new()
with(jd3.2,
     plot(year,anom,
          type="b"))
abline(h=0)
dev.off()

#=============

anom.df<-jd3.2

outputnames<-paste0(wdout,"/",
                    "3.1.anomaly_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),".csv")
write.csv(anom.df,outputnames,row.names = F)





#使わないのこり=============

#catch.dfの被説明変数の特性  
#rate.toyamawan は2峰型　　？比は2峰型になって当たり前？
# hist(jd2$rate.toyamawan)

# #冷水暖水の平均の差　とcatch.df の比較するべきのものの相関
# plot(rate.toyamawan~temp.nega,data=jd2)
# plot(rate.toyamawan~temp.posi,data=jd2)
# 
# 
# 
# #●●●水温差と漁獲配分比
# with(jd2,
#      plot(temp.dif,rate.toyamawan,
#           xlim=c(0,max(temp.dif))
#           )
#      )
# result.lm<-lm(rate.toyamawan~temp.dif,data=jd2)
# abline(result.lm)
# 
# # abline(coef = coef(result.lm)) と同じ
# #abline(coef = c(400000,130000)) c("切片","第一項目の係数","第二・・・")
# 
# 
# #★JADE2直近年ダウンロードデータ編集  で計算した、今漁期の推定水温差を読み込んで、使う
# # temp.dif.target<-0.4513095  #0908時点
# 
# file.thisyear<-list.files(path="C:/Users/水産研究所海洋資源課１/Documents/R作業ディレクトリ/1_1_Jade2の水温分布とブリ漁期漁獲/JADE2直近年ダウンロードデータ編集/output",
#                           pattern="0.1.抽出領域の水温差",
#                           full=T)
# #最新のcsvを読み込む
# test.target<-read.csv(file.thisyear[length(file.thisyear)],header=T)
# # temp.dif.target<-3.506359375
# 
# #推定値を計算 内積を使用
# est.value<-as.numeric(coef(result.lm)%*%c(1,test.target$temp.dif)
#                       )
# #推定値の描画
# points(test.target$temp.dif,est.value,col="red")
# text(test.target$temp.dif,est.value,
#      paste0(test.target$year,"-rate.toyamabay=",round(est.value,d=3)),pos=4)
# 
# 
# 
# test6<-merge(jd2,test.target,all=T)
# test6[test6$year==unique(test.target$year),"rate.toyamawan"]<-est.value
# #水温差と富山割合の過去データと当年の推測値
# outputnames<-paste0(wdout,"/",
#                     "1.4.","test6.result.final",
#                     "_",format(Sys.time(),"%m%d%H%M"),".csv")
# write.csv(test6,outputnames,row.names=F)



#ロジスティック回帰
# dev.new()
# with(jd2,
#      plot(temp.dif,rate.toyamawan),
#      xlim=c(0,max(temp.dif)
#      )
# )
# result.glm<-glm(rate.toyamawan~temp.dif,
#                 data=jd2,
#                 family=binomial())
# abline(result.lm)
# summary(result.glm)


# =============

#▲残差の計算方法

#summary(result.lm)
#output1<-summary(result.lm)
#lm.coef<-output1$coefficients
#pvalue<-lm.coef["temp.cw$difwc","Pr(>|t|)"]  #水温項のp値
#abline(a =0 , b = coef(result.lm), col = "blue")

