with(jd.i,jd.i[year.new==2019 & lon.half==lon1[20]  & lat.half==lat1[15],])


with(jd.half.i,jd.half.i[year.new==2019 & lon.half==lon1[20]  & lat.half==lat1[15] &month.new==month.i&depth==depth.i,])

# なぜか、同日、同水深、同緯度経度のデータが2つある　しかも水温、塩分が近いが異なる値を取る
with(jade.df.half,
     jade.df.half[year==2018 & lon.half==lon1[20]  & lat.half==lat1[15] &month==month.i&depth==depth.i,])
