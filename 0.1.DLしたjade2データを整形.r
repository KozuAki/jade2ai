# JADE2からDLし、解凍したファイル"XY_0_001_20190501.txt" などを、まとめる
# 月のはじめの1日しか使わない　※小塚がHDDから読みだして行う方法　ピーキーになるので、本当はよくない
# 前月の16日以降と、当月の15日までのデータの平均値を、1か月平均値として、1日に格納する


#00基本のディレクトリ
drive<-"D:/"
setwd(drive)

wd1<-"R作業ディレクトリ/99.testfolder"　　#●●●要変更
wd1<-paste0(drive,wd1)

wdin.ocean<-paste0(wd1,"/input.ocean")
#make folder 
dir.create(wdin.ocean)

#複数のパスを処理
folder1<-dir(path=wdin.ocean,  full.names = T)
path.v <- dir(path = folder1,pattern="XY", full.names = T)


library(stringr)  # str_subで　負の数で末尾からの文字数を指定できる
library(data.table) #fread

#パス1つを読み込んで、月平均と0.5度グリッドを処理して、return
fc00jadehalf<-function(path.i){
  #yomikomi
    jade.raw<-fread(path.i,stringsAsFactors = F,sep=" ",skip=5)
    jade.raw<-jade.raw[,1:4]  #uvはいらない
    jade.raw$date    <-str_sub(path.i,-12, -5)
    jade.raw$depth   <-as.numeric(str_sub(path.i,-16, -14))
    # jade.raw$filename<-str_sub(path.i,-21, -1)
    
    colnames(jade.raw)<-c("lon","lat","temp","sal","date","depth")
    #date
    jade.raw$year<-as.numeric(substring(jade.raw$date,1,4))
    jade.raw$month<-as.numeric(substring(jade.raw$date,5,6))
    jade.raw$day<-as.numeric(substring(jade.raw$date,7,8))
    # 前月の16日以降と、当月の15日までのデータの平均値を、1か月平均値として、月初めの”1日ついたち”に格納する
    jade.raw$month.new<-with(jade.raw,ifelse(day>15,month+1,month))
    
    #整数商%/% 剰余%% で、i,jを経度、緯度に変換
    jade.raw$lon<-round(127+jade.raw$lon%/%12+(jade.raw$lon%%12)/12,d=2)
    jade.raw$lat<-round(35+jade.raw$lat%/%15+(jade.raw$lat%%15)/15,d=2)
    
    #●緯度、経度を0.5度単位にまとめる======
    #経度			
    # 整数部分　round(jade.raw$lon%/%1,d=2)
    # 小数部分　round(jade.raw$lon%%1,d=2)
    # unique(round(jade.raw$lon%%1,d=2))　　　#経度は12グリッド
    #  "0"    "0.08" "0.17" "0.25" "0.33" "0.42" "0.5"  "0.58" "0.67" "0.75" "0.83" "0.92"
    #.33~.75は.50にまとめる
    cond1<-(round(jade.raw$lon%%1,d=2)>0.25)&(round(jade.raw$lon%%1,d=2)<=0.75)
    cond2<-(round(jade.raw$lon%%1,d=2)>0.75)
    jade.raw$lon.half<-ifelse(cond1,
                              jade.raw$lon%/%1+0.5,
                              ifelse(cond2,jade.raw$lon%/%1+1,jade.raw$lon%/%1)
    )   　　　　　　　#if　とelseでは、条件式はベクトルでは入らないので、ifelseの入れこ式
    
    #確認用　　table(jade.raw$lon,jade.raw$lon111.0)
    
    #緯度
    # 整数部分　round(jade.raw$lat%/%1,d=2)
    # 小数部分　round(jade.raw$lat%%1,d=2)
    # unique(round(jade.raw$lat%%1,d=2))　　　#緯度は１５グリッド
    # [1] "0"    "0.07" "0.13" "0.2"  "0.27" "0.33" "0.4"  "0.47" "0.53" "0.6"  "0.67" "0.73" "0.8"  "0.87" "0.93"
    #.33~.73は.50にまとめる
    cond1<-(round(jade.raw$lat%%1,d=2)>0.27)&(round(jade.raw$lat%%1,d=2)<=0.73)
    cond2<-(round(jade.raw$lat%%1,d=2)>0.73)
    jade.raw$lat.half<-ifelse(cond1,
                              round(jade.raw$lat%/%1,d=2)+0.5,
                              ifelse(cond2,round(jade.raw$lat%/%1,d=2)+1,round(jade.raw$lat%/%1,d=2))
    )   　　　　　　　#if　とelseでは、条件式はベクトルでは入らないので、ifelseの入れこ式
    #確認用　　table(jade.raw$lat,jade.raw$lat11.0)
    rm(list=ls(pattern="cond"))
    #0.5度　グリッドおわり
    
    #0.5度メッシュにまとめる========
    jade.raw$temp<-replace(jade.raw$temp,which(jade.raw$temp==-999),NA)
    jade.raw$sal<-replace(jade.raw$sal,which(jade.raw$temp==-999),NA)
    jade.raw.half<-aggregate(data=jade.raw,
                             cbind(temp,sal)~lon.half+lat.half+depth+year+month.new,
                             FUN="mean",na.rm=T)
    return(jade.raw.half)
    }


#繰り返し計算
jade.raw<- do.call(rbind, lapply(path.v,fc00jadehalf))


# 前月の16日以降と、当月の15日までのデータの平均値を、1か月平均値として、月初めの”1日ついたち”に格納する
# jade.raw$month.new<-with(jade.raw,ifelse(day>15,month+1,month))
# jade.raw$day.new<-1

jade.mon<-aggregate(data=jade.raw,
                    cbind(temp,sal)~lon.half+lat.half+depth+year+month.new,
                    FUN="mean")
#時間がかかる　lapplyなどで、早くならないものか?

names(jade.mon)<-c("lon.half","lat.half","depth","year","month","temp","sal" )
jade.mon$day<-1
jade.mon$fyear<-jade.mon$year
  

names(jade.raw)
dim(jade.mon)
dim(jade.raw)


#確認 グリッド描画======
X<-with(jade.mon,
        tapply(temp,
               list(year,month.new,lon.half,lat.half,depth),
               mean,na.rm=T))
mai<-1 #★余白
range.lon<-as.integer(c(min(as.numeric(dimnames(X)[[3]]))-mai,max(as.numeric(dimnames(X)[[3]]))+mai))
range.lat<-as.integer(c(min(as.numeric(dimnames(X)[[4]]))-mai,max(as.numeric(dimnames(X)[[4]]))+mai))

#地図表示用
library(mapdata) ;library(fields)
dev.new()
map("japan",xlim=range.lon,ylim=range.lat,col="white",boundary=F) ; par(new=T)
image.plot(as.numeric(dimnames(X)[[3]]),
           as.numeric(dimnames(X)[[4]]),
           X["2019","1",,,"200"],
           xlim=range.lon,ylim=range.lat,
           zlim=c(-5,35),
           main=paste(2019,01,100,"m",sep="_") ,
           nlevel=10
           )
map("japan",xlim=range.lon,ylim=range.lat,fill=T,col=gray(0.7),add=T)
box()



