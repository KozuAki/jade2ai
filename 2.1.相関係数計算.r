#00 関数の定義　======
# 緯度経度ごとの相関係数のデータresult.subをreturnする 
# year.excludeは除外する年（複数年のベクトルも可能）
fc.soukan<-function(jade.df.half,catch.df,month.i,depth.i,
                    month.slide,
                    fyear.v,year.exclude,
                    objective,tres.cor,tres.pvalue,future.index){
  #001　year.vi は、year.excludeの年をぬいた、年時系列となる
  if(any(year.exclude%in%9999)){
    year.vi<-fyear.v #year.excludeが9999なら、除外年なし
  }else {year.vi<-fyear.v[-which(fyear.v%in%year.exclude)]　#year.excludeを除外
  }
  #002 海洋データ
  # 漁獲と比較する年月をmonth.slideだけずらす
  #例えば month.slide<-4
  #例えば month.i<-8
  jd.half.i<-jade.df.half
  slide.index<- month.i - month.slide%%12  
  #例えば、初期値が8月で、-4か月（12*0+8*0+4）か、-14カ月（12*1+8*0+2）か、-21カ月（12*1+8*1+1)か、（年のまたぎを考慮）
  if(slide.index>0){　#年をまたがない場合
    jd.half.i$month.new<- jd.half.i$month + month.slide%%12
    jd.half.i$year.new<-  jd.half.i$year  + month.slide%/%12
  }else{　　　　　　　#年をまたぐ場合
    jd.half.i$month.new<- jd.half.i$month + month.slide%%12 -12 #2年以上前でも対応？→「余り」を利用しているのでＯＫ
    jd.half.i$year.new<-  jd.half.i$year  + month.slide%/%12 +1
  }
  #month.slideが０ならば、何も変化しない
  # jd.i<-subset(jd.half.i,year.new%in%year.vi & month.new==month.i & depth==depth.i)
  jd.i<-subset(jd.half.i,fyear%in%year.vi & month.new==month.i & depth==depth.i)
  jd.i<-jd.i[,-1*which(names(jd.i)=="fyear")] #once delete "fyear" for sea temp
  # jd.i$year<-jd.i$year.new
  # jd.i$month<-jd.i$month.new
  
  # jd.i$fyear.new<-with(jd.i,
  #                      ifelse(month.new>=tres.month,year.new,year.new-1))
  # month.new==month.i で絞り込まれているので
  if(month.i>=tres.month){
    jd.i$fyear.new<-jd.i$year.new
  }else{ #つまり　month.i < tres.month アオリイカでいえば、8月以降の漁期漁獲量が4月の水温（漁獲対象のイカの世代でいえば前の世代）で決定する
    jd.i$fyear.new<-jd.i$year.new-1+future.index #future.indexが1なら未来、0なら過去
    # jd.i$fyear.new<-jd.i$year.new　#異世代資源(未来：海水温データの時点の未来を説明）
    　　　　　　　　　　　　　　　　　# )　アオリイカ　4月の水温が、子の生育を経由して、次の代の資源変動へ
    # jd.i$fyear.new<-jd.i$year.new-1 #同世代資源（過去の説明））　スルメイカ　12月以降の漁獲を集計、翌年2月と比較
    # jd.i$fyear.new<-jd.i$year.new-1　#深海魚（過去の説明）tres.month 10 month.i 3  リュウグウノツカイ10月～翌年9月が漁期で、
    　　　　　　　　　　　　　　　　# 漁期の漁模様が、漁期開始後の翌年3月の水温で説明できるという考え方
  }
  
  
  #エラー対策
  if(nrow(jd.i)==0){cat("jd.i が　NULL fyear.v or depth.i check","\n")}
  
  # 漁獲データ
  catch.i<-subset(catch.df,fyear%in%year.vi)
  jade.sub<-merge(jd.i,catch.i,
                  by.x="fyear.new",by.y="fyear",
                  all.x=T)
  # year,month 相関分析で比較する年月　  
  # year.new month.new  最初に想定する、漁獲年月　month.slideで探索的に調べる前　
  # fyear.new  　相関分析で比較するための漁期年　year.new month.newに対応する漁期年
  
  
  
  # jade.subのデータセットの整理
  jade.sub$catch<-jade.sub[,"objective"]
  #相関係数の計算
  # result.table<-NULL    #初期値はNULLとする																		
  # tres<-0.4  #★　Ｒの閾値　ピアソンの相関係数表より  自由度から計算できるとなおよい
  #緯度j経度iの1グリッドごとに、相関係数を計算し、result.tableに格納		
  #▲計算が重いので、軽くする方法を考える。　subsetでいちいち切り出して計算すると重いのかも													
  result.sub<-NULL
  for(i in 1:length(lon1)){																		
    for(j in 1:length(lat1)){		
      df.i<-subset(jade.sub,																		
                   lon.half==lon1[i]&lat.half==lat1[j],																		
                   select = c(temp,catch)									
                   )		
      df.i<-na.omit(df.i)
      if(nrow(df.i)!=0){
        res.cor.test<-cor.test(df.i$temp, df.i$catch , method="pearson")
        cond.i<-res.cor.test$p.value < tres.pvalue   #ピアソンの積率相関係数の無相関検定
        cond.i<- cond.i & abs(round(cor(df.i$temp,df.i$catch),d=3))>=tres.cor  #小さい相関係数は図が見づらくなるので、落とす
        result.i<-ifelse(cond.i,																		
                         round(cor(df.i$temp,df.i$catch),d=3),NA)
        rm(cond.i)
      }else{ #df.i がない場合
        result.i<-NA
      }
      result.sub.i<-data.frame("lon.half"=lon1[i],"lat.half"=lat1[j],"R"=result.i)																		
      result.sub<-rbind(result.sub,result.sub.i)																		
    }																		
  }
  #year.excludeをcsvに残すために、文字化する
  year.exclude.chara<-as.character(year.exclude)
  while(length(year.exclude.chara)>1){
    year.exclude.chara<-paste0(year.exclude.chara,collapse=",")
  }
  #resultのsubデータセットを作成
  result.sub$year.exclude<-year.exclude.chara
  result.sub$year.range<-paste0(min(fyear.v),"~",max(fyear.v))
  result.sub$objective<-objective
  result.sub$month<-month.i
  result.sub$depth<-depth.i
  result.sub$tres.cor<-tres.cor
  result.sub$month.slide<-month.slide
  # head(result.table);tail(result.table)																		
  result.sub<-result.sub[order(result.sub$lon,result.sub$lat),]
  return(result.sub)
}

#　計算する条件を表示
cat(objective , "depth",depth.i,"m_" , "m.slide",month.slide,"\n")

#01 計算し、結果をresult.tableに入れる=======
catch.df.i<-catch.df
result.table<-fc.soukan(jade.df.half,catch.df=catch.df.i,month.i,depth.i,month.slide,
                        fyear.v=fyear.v,year.exclude=year.exclude,
                        objective=objective,tres.cor,tres.pvalue=tres.pvalue,
                        future.index=future.index)
# head(result.table)

#02 csv書き出し=======
outputnames<-paste0(wdout,"/2.1.result.table_",
                    "Rmax.",format(round(max(abs(result.table$R),na.rm=T),d=2), nsmall = 2),"_",
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),
                    ".csv")
write.csv(result.table,outputnames,row.names = F)

