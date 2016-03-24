#################################
#Takes Tweets and Finds Addresses
#################################

#remove tweets without lat/lon
data=filter(data, !is.na(lat),!is.na(lon))

#get address using lat/lon
lonlat=select(data,lon,lat)
lonlat$lon <- as.numeric(as.character(lonlat$lon))
lonlat$lat <- as.numeric(as.character(lonlat$lat))

#google maps API only allows 2500 queries per day. Can we get around this?
result <- do.call(rbind, lapply(1:nrow(data$lon),
                                function(i) revgeocode(as.numeric(lonlat[i,1:2]))))

#Parse full address to multiple, more useful fields
data2=lapply(result,  function(x) unlist(strsplit(x,",")))
address=sapply(data2,function(x) paste(x[1:3],collapse=''))
city=sapply(data2,function(x) x[2])
stzip=sapply(data2,function(x) x[3])
zipcode = as.numeric(str_extract(stzip,"[0-9]{5}"))
state=str_extract(stzip,"[:alpha:]{2}")
data2=as.data.frame(list(address=address,city=city,zipcode=zipcode,state=state))

#Until I figure out how to reverse geocode ALL addresses, I need to just use 100
data=data[1:2000,]
data=cbind(data,data2)
