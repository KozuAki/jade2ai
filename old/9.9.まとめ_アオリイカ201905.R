drive<-"D:/"
setwd(drive)

wd<-"./R作業ディレクトリ/99.testfolder"
wd1<-"R作業ディレクトリ/99.testfolder"
wd1<-paste0(drive,wd1)


wdin.fish<-paste0(wd1,"/input.fish")
wdin.ocean<-paste0(wd1,"/input.ocean")
wdin.other<-paste0(wd1,"/input.other")
wdout<-paste0(wd1,"/output")
wdout.ocean<-paste0(wd1,"/output.ocean")

#make folder 
dir.create(wdin.fish)
dir.create(wdout)
dir.create(wdout.ocean)

##海況 JADE2データの読み込み～1年前までのデータを0.5°グリッドで平均化  anom.df
# setwd(wd)
# source("0.0.水温の粗目グリッドデータセット作成.R")



#水産情報システムから　抽出　　システム以外データを説明する場合は不要======
filename1<-"アオリイカ"
filename2<-"Aori"
wdin1<-"./元データ/水産情報システム元データ（CSV)/GYO_SD_さまざまな魚種"

####日本語が読み込めないトラブルで停止　なぜ？  , encoding="utf-8"を追加して解決した
source(paste0(wd,"/9.0_水産情報システムデータ読み込みと下処理.R"), encoding="utf-8" )  
system.df<-aggregate(data=system.df,
                     cbind(catch)~year+month,
                     FUN="sum",na.rm=T)
system.df$catch<-system.df$catch/1000
system.df$pref<-24
write.csv(system.df,paste0(wdin.fish,"/catch_年月_",filename1,".csv"),row.names=F)
#======



#漁獲データを年時系列に整理    catch.df=======
#あとで繰り返し読み込むが、　length(unique(catch.df$pref))を得るため　と、　
# 対象月と漁期の区切りを設定
filename<-filename1
month.v.catch<-c(8:12,1,2)
tres.month<-8  #漁期年の取り方に注意　　buri:10 aori:8
fyear.v<-1992:2018  #catch.dfにかかるfyear.vはとりあえず広く取っておけばよい
setwd(wd)
source("1.0.漁獲物データ読込み下処理.r")  #ベースのcatch.dfを整える
# rm(catch.df.ori)
#=======

#catch.dfデータの無次元化　移動平均値に対する比に直す=====
# year.ave<-5 #移動平均を取る年数　できれば、奇数
# source("1.2.漁獲量の確認と移動平均比変換.r")
# catch.df<-catch.df[,c("fyear","pref","scale")]  #★　何を新たな被説明変数objectiveにするか
# names(catch.df)<-c("fyear","pref","objective") ; catch.df<-catch.df[,c("fyear","objective","pref") ]


#jadeデータのfyearも変化させる
jade.df.half$fyear<-with(jade.df.half,
                         ifelse(month>=tres.month,year,year-1))


tres.cor<-0.1  #▲相関係数の有意性の検定を行いたい
tres.pvalue<-0.05 #ピアソンの積率相関係数の無相関検定 ｐ値の有意水準の閾値

#重要　month.slideによっては、９２年漁期は入れられない
fyear.v<-1992:2018
year.exclude<-9999  # #除外年なし　も追加する
# year.exclude<-c(2009,2015)   # 2009年と2015年の漁期を除外

month.i<-8 #比較するJade2の月
#tres.monthよりあとの月にして、slideで調整　そうしないと、漁期年がずれる

# month.slide.v<-8
# month.slide.v<-1:12
month.slide.v<-1*rep(0:12)
# month.slide.v<-3*rep(0:4)

# depth.v<-100
depth.v<-sort(unique(jade.df.half$depth))

#catch.dfにしたがって繰り返し計算
# result.table<-NULL    #初期値はNULL

for(depth.i in depth.v){
  for(month.slide in month.slide.v){
    for(i in 1:length(unique(catch.df$pref))){
      pref.i<-unique(catch.df$pref)[i]
      catch.df.i<-catch.df[catch.df$pref==pref.i,]
      objective<-paste0(filename2,"_catch.pref",pref.i,"_",
                        "month",paste(range(month.v.catch),collapse="-"))   # "month",paste(month.v.catch,collapse=","))
      source("2.1.相関係数計算.r", encoding="utf-8")
      source("2.3.相関係数マッピング.R", encoding="utf-8")
    }
  }
}



#抽出領域を絞って、予測値を比較=====
tres.cor<-0.3
tres.pvalue<-0.05
depth.i<-1
month.slide<-1
for(i in 1:length(unique(catch.df$pref))){
  pref.i<-unique(catch.df$pref)[i]
  catch.df.i<-catch.df[catch.df$pref==pref.i,]
  objective<-paste0(filename2,"_catch.pref",pref.i,"_",
                    "month",paste(range(month.v.catch),collapse="-"))   # "month",paste(month.v.catch,collapse=","))
  source("2.1.相関係数計算.r", encoding="utf-8")
}
#これで、欲しい条件のときのresult.tableにできた

#.checked.csv を作成
source("3.1.相関領域の抽出と予測値.r", encoding="utf-8")

source("3.3.相関最大グリッドと予測値.r", encoding="utf-8") 




