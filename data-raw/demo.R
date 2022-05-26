#Demo
library(devtools)
load_all()

#Some toy data
df <- read.csv("https://paleobiodb.org/data1.2/colls/list.csv?base_name=Scleractinia&interval=Anisian,Piacenzian")
#assign midpoint age for later
df$age <- (df$max_ma + df$min_ma)/2

#time bins
time_bins()
#near-equal length time bins
time_bins(equal = TRUE, size = 6)
#assign data
time_bins(assign = df$age)

#latitudinal bins
lat_bins(size = 10)
#fit latitudinal bins
lat_bins(size = 13, fit = TRUE)
#assign to latitudinal bins
lat_bins(size = 10, assign = df$lat)

#palaeorotate
palaeorotate(x = df, model = "Merdith2021")
#palaeorotation uncertainity
palaeorotate(x = df, uncertainty = "TRUE")
