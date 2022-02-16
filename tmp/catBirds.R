birds_11 = read.csv("corhaven_2011-01-01_2011-12-31.csv",stringsAsFactors = F)
birds_12 = read.csv("corhaven_2012-01-01_2012-12-31.csv",stringsAsFactors = F)
birds_13 = read.csv("corhaven_2013-01-01_2013-12-31.csv",stringsAsFactors = F)
birds_14 = read.csv("corhaven_2014-01-01_2014-12-31.csv",stringsAsFactors = F)
birds_15 = read.csv("corhaven_2015-01-01_2015-12-31.csv",stringsAsFactors = F)
birds_16 = read.csv("corhaven_2016-01-01_2016-12-31.csv",stringsAsFactors = F)
birds_17 = read.csv("corhaven_2017-01-01_2017-12-31.csv",stringsAsFactors = F)
birds_18 = read.csv("corhaven_2018-01-01_2018-12-31.csv",stringsAsFactors = F)
birds_19 = read.csv("corhaven_2019-01-01_2019-12-31.csv",stringsAsFactors = F)
birds_20 = read.csv("corhaven_2020-01-01_2020-12-31.csv",stringsAsFactors = F)
birds_21 = read.csv("corhaven_2021-01-01_2021-12-31.csv",stringsAsFactors = F)

birds_21$year = NULL

birdsFin = rbind(birds_11,
                 birds_12,
                 birds_13,
                 birds_14,
                 birds_15,
                 birds_16,
                 birds_17,
                 birds_18,
                 birds_19,
                 birds_20,
                 birds_21)

write.csv(birdsFin,"corhaven_2011-2021.csv",row.names = F)
