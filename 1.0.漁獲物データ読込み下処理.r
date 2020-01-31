#海況と比較するための、データセットcatch.df を作成

# wdin.fish　#●●●要確認=======
#▲注意　ファイルの外から、filenameを指定しても反応しない
# filename<-"アオリイカ"
path.fish.v<-dir(path=wdin.fish,
                 full.names = T,
                 pattern=paste0(filename))  
path.fish.v<-path.fish.v[regexpr("\\.csv$",path.fish.v)>0]  #.csv 以外を省く

catch.df.ori<-read.csv(path.fish.v,header=TRUE)


#漁期年fyearの作成======
# tres.month<-8  #●●●要確認
catch.df.ori$fyear<-with(catch.df.ori,
                         ifelse(month>=tres.month,year,year-1))

#漁期年別漁獲量データ　catch.dfを作成======
# month.v.catch<-c(9:12,1,2)  #●●●要確認
# fyear.v<-1992:2018
catch.df<-with(catch.df.ori,
               catch.df.ori[fyear%in%fyear.v & month%in%month.v.catch, ]
               )

if(any(names(catch.df)%in%"ind")){   #深海魚など、個体数を説明するときはind
  catch.df<-aggregate(data=catch.df,
                      cbind(ind)~fyear+pref,
                      FUN="sum",na.rm=T)
  catch.df<-catch.df[,c("fyear","ind","pref")]
  names(catch.df)<-c("fyear","objective","pref")
} else{
  catch.df<-aggregate(data=catch.df,
                      cbind(catch)~fyear+pref,
                      FUN="sum",na.rm=T)
  
  catch.df<-catch.df[,c("fyear","catch","pref")]
  names(catch.df)<-c("fyear","objective","pref")
}


