
#相関係数のマップ表示　　result.tableから作成
R<-with(result.table,
        tapply(R,list(lon.half,lat.half),mean)
        )																		
lon2<-as.numeric(dimnames(R)[[1]])																		
lat2<-as.numeric(dimnames(R)[[2]])


range.lon<-as.integer(c(min(lon2),max(lon2)))
range.lat<-as.integer(c(min(lat2),max(lat2)))


#pdfを作成======
outputnames<-paste0(wdout,"/",
                    "2.3.相関係数マップ_",
                    "Rmax.",format(round(max(abs(result.table$R),na.rm=T),d=2), nsmall = 2),"_",  #Rよりも、p値
                    objective,"_vs_",
                    "month",month.i,"_",
                    "m.slide",month.slide,"_",
                    "depth",depth.i,"_",
                    unique(result.table$year.range),"_",
                    format(Sys.time(),"%m%d%H%M"),".pdf")
pdf(outputnames)

#▲R>0.4i以上のみを色づけするとか。																		
#▲p値も計算するようにしたい。ピアソンの相関係数																		


# dev.new()
par(mfrow=c(1,1),mai=rep(2,4)+c(0,0,0,0))
par(family="Japan1GothicBBB") 
library(mapdata) ;library(fields);library(akima) 
map("japan",xlim=range.lon,ylim=range.lat,col="white",boundary=F) ; par(new=T)
#1 カラーで相関の強さ
image.plot(lon2,lat2,R[,],		
           zlim=c(-1,1),
           main=paste0("R-temp",unique(result.table$depth),"m","_month",unique(result.table$month),"_month.slide",unique(result.table$month.slide),
                       "\n","VS_",unique(result.table$objective)),  #"\n" は2行目
           cex.main=1.5,
           col=c(cm.colors(20)),nlevel=10, 
           horizontal = FALSE, legend.shrink = 0.9, legend.width = 0.9, 
           legend.mar = 0 , legend.lab = NULL#,   #▲まだ微妙にずれるがだいぶましになった
           #legend.line= 5)	
)		

#2 相関の等値コンター
#1度ごとに黒色実線
if(tres.pvalue>0.10){ 
  n<-100
  if(nrow(na.omit(result.table))>=3){  #複数のグリッドのＲが必要 ３点あれば平年できる
    akima.li <- with(result.table[!is.na(result.table$R),],
                     interp(lon.half, lat.half, R,duplicate="mean",
                            xo=seq(min(lon.half), max(lon.half), length = n),
                            yo=seq(min(lat.half), max(lat.half), length = n))
    )
    contour(akima.li$x,akima.li$y,akima.li$z,
            add=TRUE,col="black",
            levels=seq(-1,1,by=0.1))
  }
}

#3 相関の最大値、最小値を上書き
with(na.omit(result.table),
     text(max(range.lon)-5,min(range.lat)+1,
          paste0("r=_",round(min(R),d=2),"to ",round(max(R),d=2)),
          pos=1,col="black",cex=1.2) #小数第二位まで表示したい
)

#4 相関の値を図に書き入れる
if(tres.pvalue<=0.10){
  with(result.table,
       text(lon.half,lat.half,
            round(R*10,d=0)/10,
            pos=1,offset=0.2,col="black",cex=0.4)
  )
}



#地図を上書き
# map("japan",xlim=range.lon,ylim=range.lat,
# fill=T,col=gray(0.7),add=T)
map("worldHires",xlim=range.lon,ylim=range.lat,
    fill=T,col=gray(0.7),add=T)
box()
dev.off()	
#===========

#注意点　3と4の　Rの最高値、最低値の場所の表示と値の文字表示　
# ただし、ベースの図にいれてしまうと、資料作成の時に邪魔になることも。。




#========


# ========================================================================

# 															
# #Rの値確認用
# t(R[1:nrow(R),ncol(R):1])
# 
# R2<-round(R[(136<=lon2&lon2<=138),(38<=lat2&lat2<=40)],3)	
# t(R2[1:nrow(R2),ncol(R2):1])   #緯度ncol  経度nrow
# 
# max(R2,na.rm=T)				
# min(R2,na.rm=T)
# (maxindex<-which.max(R2)) 　　#maxindexは左上から右下方向に大きくなる。
# R2[maxindex]
# #dim(R2)[1] #lon方向のグリッド数  nrow(R2)
# #dim(R2)[2] #lat方向のグリッド数 
# colnames(R2)[maxindex%/%nrow(R2)+1] #lat　商なので、+1がポイント
# rownames(R2)[ifelse(maxindex%%nrow(R2)!=0,maxindex%%nrow(R2),nrow(R2))] #lon 余り  余りが0の時にエラーが生じる 余り0のときは、dim(R2)[1]の値をとらせる必要がある
# 
# max(R,na.rm=T);min(R,na.rm=T)
# maxindex<-which.max(R)  ;  R[maxindex]
# #maxindex%%dim(R)[1] ; maxindex%/%dim(R)[1] #商と余りから緯度経度が出せないわけではないが、もっといい方法があると思う。



#　数値データ見られるようにする
#i年j月に絞る 緯度経度も適度に絞る 

#length(lon2) ; length(lat2)
#数字を小さくすると、緯度は北へ、経度は西へ絞られる
#testR2<-R2[(lon2[5]>=lon2),(lat2[55]<=lat2)]   
#rev(dimnames(testR2)[[2]])
#表示 t(testR2[1:nrow(R2),ncol(R2):1])



#★アウトプット名																		
#outputfilename<-sprintf("R_j2d_vs_catch%s-%d-temp%sm.csv",prefname,monthcond,depth)																		
#write.csv(t(R), outputfilename, quote=FALSE)																		

