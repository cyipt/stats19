# TO handle SPSS files with duplicated factors
# from https://dadoseteorias.wordpress.com/2017/04/29/read-spss-duplicated-levels/
Int2Factor <- function(x)
{
  if(!is.null(attr(x, "value.labels"))){
    vlab <- attr(x, "value.labels")
    if(sum(duplicated(vlab)) > 0)
      cat("Duplicated levels:", vlab, "\n")
    else if(sum(duplicated(names(vlab))) > 0)
      cat("Duplicated labels:",
          names(vlab)[duplicated(names(vlab))], "\n")
    else
      x <- factor(x, levels = as.numeric(vlab),
                  labels = names(vlab))
  }
  x
}


#Check Number of Digits in Nation Grid References
check.bng <- function(value){
  if(is.na(value)){
    #Remove NAs
    value <- 0
  }else{
    if(nchar(value) == 5){
      value <- value * 10
    }else if(nchar(value) == 4){
      value <- value * 100
    }
  }
  return(value)
}

#Get Date Time
get.datetime <- function(year, mth,day,hr,min,date,tim){
  #year <- dat$Year[1]
  #mth <- 5
  mth <- match(as.character(mth), month.name)
  #mth <- dat$Month[1]
  #day <- dat$Day[1]
  #hr <- dat$Hour[1]
  #min <- dat$Minute[1]
  #date <- dat$Date[1]
  #tim <- dat$Time[1]
  if(!is.na(date) & !is.na(tim)){
    #Use date and time
    res <- as.POSIXct(paste(date, tim), format="%d/%m/%Y %H:%M")
  }else if(!is.na(mth) & !is.na(day) & !is.na(hr) & !is.na(min)){
    #use year month day hour minute
    res <- ISOdatetime(year = year, month = mth, day = day, hour = hr, min = min, sec = 0)
  }else if(!is.na(mth) & !is.na(day) & (is.na(hr) | !is.na(min)) ){
    #Have date but not time
    if(is.na(hr)){hr <- 0}
    if(is.na(min)){min <- 0}
    res <- ISOdatetime(year = year, month = mth, day = day, hour = hr, min = min, sec = 0)
  }else{
    #message(paste0("class(time) is",class(tim)))
    message(paste0("Problem with date time for ", as.character(mth)," ",as.character(day)," ",as.character(hr)," ",as.character(min)," ",as.character(date)," ",as.character(tim)))
    res <- NA
  }
  return(res)

}




stats19.import <- function(file, type = c("acc", "cas", "veh")){
  #Get File Type
  #And open the file
  file.type <- substring(file,nchar(file) - 3, nchar(file))
  if(file.type  == ".sav"){
    dat <- read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
    dat <- lapply(dat, Int2Factor)
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  }else if(file.type  == ".por"){
    dat <- as.data.frame(as.data.set(spss.portable.file(file)))
  }else if(file.type  == ".csv"){
    dat <- read.csv(file, header = T, stringsAsFactors = F)
  }else{
    message("Unknow File type ",file)
  }

  #Get the possible name combinations
  dat.names <- read.csv(paste0("../stats19/data/",type,".csv"), stringsAsFactors = F, header = F)

  #Check which set of names we are using
  #Different years have different naming conventions and columns
  for(j in 1:nrow(dat.names)){
    compare <- as.character(as.vector(dat.names[j,2:ncol(dat.names)]))
    if(all(names(dat) %in% compare)){
      #Matched All Varaible Names but need to check order
      dat.names.sub <- data.frame(t(dat.names[c(1,j),2:ncol(dat.names)]), stringsAsFactors = F)
      names(dat.names.sub) <- c("new","old")
      dat.names.new <- dat.names.sub$new[match(names(dat), dat.names.sub$old)]
      names(dat) <- dat.names.new
      break #Stop loop as match found
    }
    if(j == nrow(dat.names)){
      #No match found
      message(paste0("Unknown Combination of Values for type = ",type))
      stop()
    }
  }

  #Check for any columns that are missing ann add them
  missing.col <- dat.names.sub$new[!(dat.names.sub$new %in% names(dat))]
  dat[,missing.col] <- NA

  #Reorder the dat into the standard order
  dat <- dat[,dat.names.sub$new]

  #Type specific changes
  if(type == "acc"){
    #Force out some rouge values
    dat$RoadNumber2 <- as.integer(as.character(dat$RoadNumber2))
  }


  #Clean up dates in the Acc
  #To store a singe DateTime Field
  if(type == "acc"){
    res <- mapply(get.datetime,dat$Year,dat$Month, dat$Day, dat$Hour, dat$Minute, dat$Date, dat$Time, SIMPLIFY = FALSE)
    dat$DateTime <- do.call("c",res)
    rm(res)
    dat <- dat[,names(dat)[!names(dat) %in% c("Month","Day","Hour","Minute","Date","Time")]] #Remove Columns
  }

  #Change factors to characters
  classes <- lapply(dat, class)
  for(b in 1:length(classes)){
    if(classes[b] == "factor"){
      dat[,b] <- as.character(dat[,b])
    }
  }

  #Clean values that have changes over the years to standard forms
  if(type == "cas"){
    #Remove Differetn types of "no data lable"
    dat$CasSex[dat$CasSex %in% c("Undefined","NULL or Invalid value","Data missing or out of range")] <- NA

    dat$Age[dat$Age == "NULL or Invalid value"] <- NA
    dat$Age <- as.integer(dat$Age) #For to number even if factor
    dat$Age[dat$Age == -1] <- NA

    dat$PedestrianLocation[dat$PedestrianLocation %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$PedestrianLocation[dat$PedestrianLocation %in% c("Not pedestrian","Not a pedestrian","Not a Pedestrian")] <- "Not a pedestrian"
    dat$PedestrianLocation[dat$PedestrianLocation == "Crossing elsewhere within 50m. of ped.crossing"] <- "Crossing elsewhere within 50m. of pedestrian crossing"
    dat$PedestrianLocation[dat$PedestrianLocation == "Crossing on ped. crossing facility"] <- "Crossing on pedestrian crossing facility"
    dat$PedestrianLocation[dat$PedestrianLocation == "Crossing elsewhere within 50m. of ped crossing"] <- "Crossing elsewhere within 50m. of pedestrian crossing"
    dat$PedestrianLocation[dat$PedestrianLocation %in% c("In centre of c-way - not on refuge, island or central res.","In centre of c-way - not refuge, island, central res.")] <- "In centre of carriageway - not on refuge, island or central reservation"

    dat$PedestrianDirection[dat$PedestrianDirection == "NULL or Invalid value"] <- NA

    dat$SchoolPupil[dat$SchoolPupil == "Undefined"] <- NA

    dat$CarPassenger[dat$CarPassenger %in% c("Undefined","NULL or Invalid value","Data missing or out of range")] <- NA

    dat$BusPassenger[dat$BusPassenger %in% c("Undefined","NULL or Invalid value","Data missing or out of range")] <- NA

    if(class(dat$CasualtyType) == "numeric"){
      castype <- read.csv("../stats19/data/import/cas-CasualtyType.csv", stringsAsFactors = F, header = T)
      tmp.cas <- castype$label[match(dat$CasualtyType,castype$code)]
      dat$CasualtyType <- tmp.cas
      rm(castype,tmp.cas)
    }

    dat$MaintenanceWorker[dat$MaintenanceWorker %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$MaintenanceWorker[dat$MaintenanceWorker == "Not Known"] <- "Not known"

    dat$HomeArea[dat$HomeArea %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$CasualtyIMD[dat$CasualtyIMD %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA


  }else if(type == "veh"){

    if(class(dat$VehicleType) == "numeric"){
      tmp.vals <- read.csv("../stats19/data/veh/VehicleType.csv", stringsAsFactors = F, header = T)
      tmp.veh <- tmp.vals$label[match(dat$VehicleType,tmp.vals$code)]
      dat$VehicleType <- tmp.veh
      rm(tmp.vals,tmp.veh)
    }

    dat$VehicleType[dat$VehicleType %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$VehicleType[dat$VehicleType == "Motor cycle 125cc and under"] <- "Motorcycle 125cc and under"
    dat$VehicleType[dat$VehicleType == "Motorcycle - unknown cc (from 2011)"] <- "Motorcycle - unknown cc"
    dat$VehicleType[dat$VehicleType == "Electric motorcycle (from 2011)"] <- "Electric motorcycle"
    dat$VehicleType[dat$VehicleType == "Ridden horse (from 1999)"] <- "Ridden horse"
    dat$VehicleType[dat$VehicleType == "Car (from 2005)"] <- "Car"
    dat$VehicleType[dat$VehicleType == "Taxi / Private hire car (from 2005)"] <- "Taxi/Private hire car"
    dat$VehicleType[dat$VehicleType == "Tram (from 1999)"] <- "Tram"
    dat$VehicleType[dat$VehicleType %in% c("Agricultural vehicle (from 1999)", "Agricultural vehicle (inc diggers etc)")] <- "Agricultural vehicle"
    dat$VehicleType[dat$VehicleType == "Minibus (8 - 16 passenger seats) (from 1999)"] <- "Minibus (8 - 16 passenger seats)"
    dat$VehicleType[dat$VehicleType %in% c("Goods over 3.5t and under 7.5t mgw", "Goods over 3.5t. and under 7.5t. (from 1999)")] <- "Goods over 3.5t. and under 7.5t"
    dat$VehicleType[dat$VehicleType == "Goods vehicle - unknown weight (self rep only)"] <- "Goods vehicle - unknown weight"
    dat$VehicleType[dat$VehicleType %in% c("Goods 3.5 tonnes mgw or under", "Goods 3.5 tonnes maximum gross weight or under", "Van / Goods 3.5 tonnes mgw or under")] <- "Goods under 3.5t."
    dat$VehicleType[dat$VehicleType == "Motorcycle over 500cc (from 2005)"] <- "Motorcycle over 500cc"
    dat$VehicleType[dat$VehicleType == "Motorcycle over 125cc and up to 500cc (from 2005)"] <- "Motorcycle over 125cc and up to 500cc"
    dat$VehicleType[dat$VehicleType %in% c("Goods 7.5 tonnes mgw and over", "Goods 7.5 tonnes mgw and over (from 1999)")] <- "Goods over 7.5t."







    dat$VehAgeBand[dat$VehAgeBand %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$HitRun[dat$HitRun %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    if(class(dat$JourneyPurpose) == "numeric"){
      jpurp <- read.csv("../stats19/data/import/veh-JourneyPurpose.csv", stringsAsFactors = F, header = T)
      tmp.veh <- jpurp$label[match(dat$JourneyPurpose,jpurp$code)]
      dat$JourneyPurpose <- tmp.veh
      rm(jpurp,tmp.acc)
    }

    dat$JourneyPurpose[dat$JourneyPurpose %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    if(class(dat$DriverArea) == "numeric"){
      darea <- read.csv("../stats19/data/import/veh-DriverArea", stringsAsFactors = F, header = T)
      tmp.veh <- darea$label[match(dat$DriverArea,darea$code)]
      dat$DriverArea <- tmp.veh
      rm(darea,tmp.veh)
    }

  }else if (type == "acc"){

    #Add police force if number is provided
    if(class(dat$Police) == "numeric"){
      tmp.list <- read.csv("../stats19/data/import/acc-Police.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$Police,tmp.list$code)]
      dat$Police <- tmp.acc
      rm(tmp.list,tmp.acc)
    }


    #Add LA  if number is provided
    if(class(dat$LA) == "numeric"){
      tmp.list <- read.csv("../stats19/data/import/acc-LA.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$LA,tmp.list$code)]
      dat$LA <- tmp.acc
      rm(tmp.list,tmp.acc)
    }

    dat$JunctionDetail[dat$JunctionDetail %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$JunctionControl[dat$JunctionControl %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$RoadClass2[dat$RoadClass2 %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$CrossingControl[dat$CrossingControl %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA
    dat$CrossingControl[dat$CrossingControl == "None within 50 metres"] <- "None"

    #Add CrossingFacilities  if number is provided
    if(class(dat$CrossingFacilities) == "numeric"){
      tmp.list <- read.csv("../stats19/data/import/acc-CrossingFacilities.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$CrossingFacilities,tmp.list$code)]
      dat$CrossingFacilities <- tmp.acc
      rm(tmp.list,tmp.acc)
    }

    dat$CrossingFacilities[dat$CrossingFacilities %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$CrossingFacilities[dat$CrossingFacilities == "Pelican, puffin, toucan or similar non-junct pedestrian ligh"] <- "Pelican, puffin, toucan or similar non-junction pedestrian light crossing"
    dat$CrossingFacilities[dat$CrossingFacilities == "No physical crossing facilities within 50 metres"] <- "No crossing facility within 50 metres"

    dat$Surface[dat$Surface %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$SpecialConditions[dat$SpecialConditions %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA

    dat$Hazards[dat$Hazards %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA

    dat$PoliceOfficerAttend[dat$PoliceOfficerAttend %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA

    #Add LAHighway  if number is provided

      tmp.list <- read.csv("../stats19/data/import/acc-LAHighway.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$LAHighway,tmp.list$code)]
      dat$LAHighway <- tmp.acc
      rm(tmp.list,tmp.acc)



  }


  #Convert Acc to SF
  if(type == "acc"){
    #Check for columns to use
    if(!(all(is.na(dat$Easting)) | all(is.na(dat$Northing)))){
      # Use Easting and Northings
      dat$Easting <- sapply(dat$Easting,check.bng)
      dat$Northing <- sapply(dat$Northing,check.bng)
      dat <- st_as_sf(dat, coords = c("Easting","Northing"), crs = 27700, remove = T)
    }else if(!(all(is.na(dat$Longitude)) | all(is.na(dat$Latitude)))){
      dat <- st_as_sf(dat, coords = c("Longitude","Latitude"), crs = 4326, remove = T)
      dat <- st_transform(dat,27700)
    }else{
      warning("Unknown coorinates system")
      stop()
    }
    dat <- dat[,names(dat)[!names(dat) %in% c("Easting","Northing","Longitude","Latitude")]] #Remove Columns

    #Reduce precison of data to reduce file size, accurate to about 11 cm
    dat$geometry <- st_as_sfc(st_as_binary(dat$geometry, precision = 1000000))
    st_crs(dat) <- 27700

  }


  #Return Data
  return(dat)

}






#Convert Stats19 Codes to Human readable strings
stats19.code2string <- function(dat,type = c("acc", "cas", "veh")) {
  #SF Data frame convert to data frame and then convert back at end
  #Otherwise numeric column subsetting does not work
  if("sf" %in% class(dat)){
    toSF <- TRUE
    dat <- as.data.frame(dat)
  }else{
    toSF <- FALSE
  }

  #Get the list of columns that can be revalued
  columns <- list.files(paste0("../stats19/data/",type,"/"))
  columns <- gsub(".csv","",columns)

  #Check for columns that are type character but only contain numbers
  for(l in 1:ncol(dat)){
    #Skip Geometry and Date Time columns
    if(length(class(dat[,l])) == 1){
      #Check that we can do the replacement
      if((class(dat[,l]) == "character") & (names(dat)[l] %in% columns)){
        #Check that all the characters are infact intergers
        if(all(unique(dat[,l]) %in% as.character(-1:100)))
        dat[,l] <- as.integer(dat[,l])
      }
    }

  }


  for(k in 1:ncol(dat)){
    #Skip Geometry and Date Time columns
    if(length(class(dat[,k])) == 1){
      #Check that we can do the replacement
      if((class(dat[,k]) == "integer") & (names(dat)[k] %in% columns)){
        lookup <- read.csv(paste0("../stats19/data/",type,"/",names(dat)[k],".csv"), stringsAsFactors = F, header = T)
        new.val <- lookup$label[match(dat[,k],lookup$code)]
        dat[,k] <- new.val
        message(paste0("Change Values for ",names(dat)[k]))
      }
    }

  }

  if(toSF){
    dat <- st_as_sf(dat)
  }

  return(dat)

}
