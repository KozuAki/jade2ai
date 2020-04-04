#スクリプト作成、チェック　
#作成者　富山県農林水産総合技術センター水産研究所　小塚晃
#作成日時　2019年5月20日

#使用上の注意============
#スクリプトの使用は自己責任でお願いします。
#再配布は禁止です（最新版を渡した方がよいため）。
#小塚までご連絡ください　akira.kozuka@pref.toyama.lg.jp

#Rstudioの文字化けは、メモ帳等から開いてコピー＆ペーストで対処してください。
# File > Reopen with encoding... で開きなおすと対処できました。

#必要なパッケージは適宜インストールしてください。　library() となっているものです。

# Rstudioを使って、プロジェクトで作業しています。複数のスクリプトを9.9....rのまとめのスクリプトで動かします。
#=============

#それぞれのスクリプトの概要===============

#0.0.水温の粗目グリッドデータセット作成.R
#このスクリプト、海洋再解析値を読み込んで、jade.df.halfというデータセットを作成
# jade.df.halfは、日本海＋東シナ海の各層の水温データを、0.5度格子で平均化したもの
# もとのデータはJADE2 の各月の1日時点のデータ（1992年～）　※本当は月平均が望ましい

# JADE2のウェブサイトからDLしてデータセットを作る場合は次の"0.1.DLしたjade2データを整形.r"を参照

# 0.1.DLしたjade2データを整形.r
# JADE2からDLし、解凍したファイル"XY_0_001_20190501.txt" などを、まとめて、jade.monを作成
# 前月の16日以降と、当月の15日までのデータの平均値を、1か月平均値として、1日に格納するのが望ましいが、計算が多く、非常に重い
# 月のはじめの1日しか使わないという方法もあるが、本当はよくない

# 1.0.漁獲物データ読込み下処理.r
#海況と比較するための、データセットcatch.df を作成
# names(catch.df)<-c("fyear","objective","pref")　でわかるように、被説明変数をobjectiveという名前にしている
# 将来的には、漁獲量だけでなく、卵稚仔の変動の説明も見越しての変数名。

# 2.1.相関係数計算.r
# fc.soukanという関数を作成し、相関係数を繰り返し計算
# 緯度経度ごとの相関係数のデータresult.tableを作成する



# 2.3.相関係数マッピング.R
# result.tableのデータをマップに可視化する

# 3.1.相関領域の抽出と予測値.r
# 相関係数の高い海域の中から抽出したい領域を手作業（csvファイルを開いて編集）で選択し、
# 正の相関と負の相関の領域の水温差で、漁獲量の予測モデル（単回帰）を作成し、グラフまで作図

# 3.3.相関最大グリッドと予測値.r
# 3.1のスクリプトに近いが、相関の最大と最小を機械的に選んで、水温差からの予測モデルを自動で作成し、作図

# 9.0_水産情報システムデータ読み込みと下処理.R
# 富山県の漁獲情報集計システムのフォーマットデータを、一般的なフォーマットに変換
#　変換後は、system.dfを作成し、catch.dfの元となる
# 　各県のデータフォーマットに合わせて使ってもらえればよい。最低限、月別漁獲量があればよい

# 9.9.まとめ_ホタルイカ201905.R
# 0.1~3.3までのスクリプトをまとめて走らせるための「まとめスクリプト」
#海洋再解析値の計算は時間がかかるので、最初だけsource("0.0.水温の粗目グリッドデータセット作成.R")で走らせるが、2回目以降は走らせない

#以下、指定する変数の説明

#月別漁獲量データを、漁期年（fyear）別のyearlyデータにする
# month.v.catch<-c(1:6)　　#対象とする月
# tres.month<-1  #漁期年の取り方に注意　　buri:10 aori:8
# 例えば、8月から翌年3月までが漁期の生物の場合、　month.v.catch<-c(8:12,1:3) ; tres.month<-8 とする

# fyear.v<-1992:2018  #catch.dfにかかるfyear.vはとりあえず広く取っておけばよい
# tres.cor<-0.1  #相関係数の閾値　絶対値が小さいものはNAにする
# tres.pvalue<-0.05 #ピアソンの積率相関係数の無相関検定 ｐ値の有意水準の閾値
# fyear.v<-2003:2018　　#漁期年
# year.exclude<-9999  # 除外年なし　も追加する
# year.exclude<-c(2009,2015)   # 2009年と2015年の漁期を除外

# month.i<-4 #最初に比較するJade2の月
#tres.monthよりあとの月にして、slideで調整　そうしないと、漁期年がずれる

#　month.slide.v　時間を遡って計算する月を指定する
# month.slide.v<-0:12　だと、ある月から、0か月前、1か月前、・・・、12か月前までの海洋環境との相関を計算
# month.slide.v<-3*rep(0:12)　とすると、0カ月前、3か月前、・・・、36か月前まで。計算量の節約

# depth.v<-c(50,100)  #計算に用いる水深
# depth.v<-sort(unique(jade.df.half$depth))



#============================================

#以下、"0.0.水温の粗目グリッドデータセット作成.R"の本文

#00 必要なパッケージ
library(data.table)

#00基本のディレクトリ
drive<-"D:/"
setwd(drive)

#01水温分布のグリッドを粗くする
#　データの整理、読み込み========																		
wd1<-"R作業ディレクトリ/99.testfolder"　　#●●●要変更
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


#　日本海のデータのパス=====
wd.jade<-paste0(drive,"元データ/JADE2/txtdata/1.0.allJS")　#●●●要変更

#複数のパスを処理
filename<-"JADE2output_allJS_Eachmonth01"　　#●●●要変更
path.v<-dir(path=wd.jade,full.names = T,
            pattern=filename)
path.v<-path.v[regexpr("\\.txt$",path.v)>0]  #.txtのみ読み込む

#　東シナ海　ESC data のパスを追加=====
wd.jade<-paste0(drive,"元データ/JADE2/txtdata/1.0.allECS")　　#●●●要変更
#　複数のパスを処理=====
filename<-"JADE2output_allECS_Eachmonth01"　　#●●●要変更
path.v2<-dir(path=wd.jade,full.names = T,pattern=filename)
path.v2<-path.v2[regexpr("\\.txt$",path.v2)>0]  #.txtのみ読み込む

path.v<-c(path.v,path.v2)
path.v

#function作成 =======　
# パスを読んで、0.5度グリッドにして返す　
fc.jd.half00<-function(path.v){
  jade.df<-NULL
  library(data.table)
  
  for(i in 1:length(path.v)){
    jade.df.i<-fread(path.v[i],stringsAsFactors = F,sep=" ")
    names(jade.df.i)<-c("year","month","day","lon","lat","depth","temp","sal")
    jade.df<-rbind(jade.df,jade.df.i)
    rm(jade.df.i)
  }

  #確認用
  # with(jade.df,table(year,month))
  # with(jade.df,table(lon,lat)) ; range(jade.df$lon) ; range(jade.df$lat)  
  # with(jade.df,table(year,day))
  # with(jade.df,table(depth,month))
  # with(jade.df,table(year,depth,month))
  # unique(jade.df$depth)

  
  
#オリジナルRDSで残す、失敗したときのバックアップ用======
  # jade.df.ori<-jade.df
  # outputnamerds<-paste0(wd.jade,"/",filename,".rds")
  # saveRDS(jade.df.ori,outputnamerds)　　#重すぎる場合は、ｒｄｓで保存
  # 
  # outputnamerds<-paste0(wd.jade,"/",filename,".rds")
  # outputnamerds<-paste0(wd.jade,"/","JADE2output_buri_Eachmonth01_1,50,100,140,200m.rds")
  #   outputnamerds<-paste0(wd.jade,"/","JADE2output_all_JSandECS_Eachmonth01_1m,50m,100m,200m.rds")
  # jade.df.ori<-readRDS(outputnamerds)
  # jade.df<-jade.df.ori


  #データ整形おわり
  
  
  
  
#●緯度、経度を0.5度単位にまとめる======
  #経度			
  # 整数部分　round(jade.df$lon%/%1,d=2)
  # 小数部分　round(jade.df$lon%%1,d=2)
  # unique(round(jade.df$lon%%1,d=2))　　　#経度は12グリッド
  #  "0"    "0.08" "0.17" "0.25" "0.33" "0.42" "0.5"  "0.58" "0.67" "0.75" "0.83" "0.92"
  #.33~.75は.50にまとめる
  cond1<-(round(jade.df$lon%%1,d=2)>0.25)&(round(jade.df$lon%%1,d=2)<=0.75)
  cond2<-(round(jade.df$lon%%1,d=2)>0.75)
  jade.df$lon.half<-ifelse(cond1,
                           jade.df$lon%/%1+0.5,
                           ifelse(cond2,jade.df$lon%/%1+1,jade.df$lon%/%1)
  )   　　　　　　　#if　とelseでは、条件式はベクトルでは入らないので、ifelseの入れこ式
  
  #確認用　　table(jade.df$lon,jade.df$lon111.0)
  
  #緯度
  # 整数部分　round(jade.df$lat%/%1,d=2)
  # 小数部分　round(jade.df$lat%%1,d=2)
  # unique(round(jade.df$lat%%1,d=2))　　　#緯度は１５グリッド
  # [1] "0"    "0.07" "0.13" "0.2"  "0.27" "0.33" "0.4"  "0.47" "0.53" "0.6"  "0.67" "0.73" "0.8"  "0.87" "0.93"
  #.33~.73は.50にまとめる
  cond1<-(round(jade.df$lat%%1,d=2)>0.27)&(round(jade.df$lat%%1,d=2)<=0.73)
  cond2<-(round(jade.df$lat%%1,d=2)>0.73)
  jade.df$lat.half<-ifelse(cond1,
                           round(jade.df$lat%/%1,d=2)+0.5,
                           ifelse(cond2,round(jade.df$lat%/%1,d=2)+1,round(jade.df$lat%/%1,d=2))
  )   　　　　　　　#if　とelseでは、条件式はベクトルでは入らないので、ifelseの入れこ式
  #確認用　　table(jade.df$lat,jade.df$lat11.0)
  rm(list=ls(pattern="cond"))
  #0.5℃　グリッドおわり
  
  #0.5度メッシュにまとめる========
  jade.df.half<-aggregate(data=jade.df,
                          cbind(temp,sal)~lon.half+lat.half+year+month+day+depth,
                          FUN="mean",na.rm=T)

  
  rm(jade.df)
  gc(T,T) ; gc(T,T) 
  return(jade.df.half)
}

#02繰り返し読み込み　＆　結合======
# 全部読み込んで0.5度グリッドで直すにはＲのメモリが足らない
jade.df.half<-c()
for(i in 1:length(path.v)){
  jd.half.i<-fc.jd.half00(path.v[i])
  if(is.null(jade.df.half)){
    jade.df.half<-jd.half.i
  }else{  jade.df.half<-merge(jade.df.half,jd.half.i,all=T)  }
  dim(jade.df.half)
  rm(jd.half.i)
}
#確認
# dim(jade.df.half)
# with(jade.df.half,table(year,month))
# with(jade.df.half,table(depth))
# jade.df.half.ori<-jade.df.half
# outputnamerds<-paste0(wd.jade,"/",filename,".rds")
# saveRDS(jade.df.ori,outputnamerds)　　#重すぎる場合は、ｒｄｓで保存
gc(T,T) ; gc(T,T) 


#　いらない地域のデータを落とす====
#太平洋側のデータを落とす　　▲fortranの時点でやるべき処理
# cond.p<-(jade.df.half$lon>=130 & jade.df.half$lon<=133 & jade.df.half$lat<33)
# cond.p<-cond.p|(jade.df.half$lon>133 & jade.df.half$lat<=35)
cond.p<-(jade.df.half$lon>133 & jade.df.half$lat<=35)
jade.df.half<-jade.df.half[!cond.p,] ;rm(cond.p)

#　漁期年fyearを作成　　仮に作成　あとで修正可能=======
jade.df.half$fyear<-with(jade.df.half,
                         ifelse(month>=1,year,year-1))



#　確認　水温をtapplyでまとめ,drawmap ========
#lon1,lat1,range.lon,range.latを作成
X<-with(jade.df.half,
        tapply(temp,
               list(fyear,month,lon.half,lat.half,depth),
               mean,na.rm=T))
fyear1<-dimnames(X)[[1]]
month1<-dimnames(X)[[2]]
lon1<-as.numeric(dimnames(X)[[3]])
lat1<-as.numeric(dimnames(X)[[4]])
depth1<-dimnames(X)[[5]]
# fyear.v<-sort(unique(anom.df$fyear))
mai<-1 #★余白
range.lon<-as.integer(c(min(lon1)-mai,max(lon1)+mai))
range.lat<-as.integer(c(min(lat1)-mai,max(lat1)+mai))

#グリッド描画、確認
#確認　地図表示用
library(mapdata) ;library(fields)
year.i<-2019
depth.i<-50
month.i<-12
year.i<-as.character(year.i) ; month.i<-as.character(month.i); depth.i<-as.character(depth.i)  
dev.new()
map("japan",xlim=range.lon,ylim=range.lat,col="white",boundary=F) ; par(new=T)
image.plot(lon1,lat1,X[year.i,month.i,,,depth.i],
           zlim=c(-5,35),
           main=paste(year.i,month.i,"_",depth.i,"m") ,
           nlevel=10
)
map("japan",xlim=range.lon,ylim=range.lat,fill=T,col=gray(0.7),add=T)
box()
#確認おわり


#03 fyearをまとめて、ある月の平均水温にするjade.df.half.averageyear===========
#  2016年までのデータ
# ▲注意点　データがない部分は0になっているので、平均化すると、沿岸の水温は下がる
# 陸地の0と、水温0の区別が難しいが、一端、0℃をNA に置換すれば、平均化するときにna.rm=Tだから関係ない
# jade.df.half$temp<-replace(jade.df.half$temp,which(jade.df.half$temp==0),NA)  #★
jade.df.half.ave<-aggregate(data=jade.df.half,
                            cbind(temp,sal)~lon.half+lat.half+month+day+depth,
                            FUN="mean",na.rm=T)
colnames(jade.df.half.ave)<-c("lon.half","lat.half","month","day","depth","tempmean","salmean")

jd.half.ori<-jade.df.half #backup用


#04 水温偏差を作成　　anom.df=======
#mergeで平均水温をjade.dfにくっつける
anom.df<-merge(jade.df.half,jade.df.half.ave,all.x=T)

#引いて、偏差にする
anom.df$temp.dif<-with(anom.df,temp-tempmean)
with(anom.df,table(year,depth,month))

#データを出力
# write.csv(anom.df,
#           paste0(wdout.ocean,
#                  "/anom.df_temp.anomary_halfgrid",
#                  "_year",paste0(range(anom.df$year),collapse = "-"),
#                  "_month",paste0(range(anom.df$month),collapse = "-"),
#                  "_depth",paste0(range(anom.df$depth),collapse = "-"),
#                  "_lon",paste0(range(anom.df$lon),collapse = "-"),
#                  "_lat",paste0(range(anom.df$lat),collapse = "-"),
#                  ".csv"),
#           row.names=F)

# メモリサイズの確認と容量空け====
gc(T,T) ;gc(T,T) 
memory.size()
