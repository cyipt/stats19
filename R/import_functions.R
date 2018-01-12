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
#check.bng <- function(value){
#  if(is.na(value)){
#    #Remove NAs
#    value <- 0
#  }else{
#    if(nchar(value) == 5){
#      value <- value * 10
#   }else if(nchar(value) == 4){
#      value <- value * 100
#    }
#  }
#  return(value)
#}

#Get Date Time
get.datetime <- function(year, mth,day,hr,min,date,tim){

  #If Month is text make into number
  if(!(class(mth) %in% c("numeric","interger")) ){
    mth <- match(as.character(mth), month.name)
  }


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




stats19.import <- function(file, type = c("acc", "cas", "veh"), lables = c(TRUE,FALSE)){
  #Get File Type
  #And open the file
  file.type <- substring(file,nchar(file) - 3, nchar(file))
  if(file.type  == ".sav" & lables == TRUE){
    dat <- read.spss(file, to.data.frame=FALSE, use.value.labels = FALSE)
    dat <- lapply(dat, Int2Factor)
    dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  }else if(file.type  == ".sav" & lables == FALSE){
    dat <- read.spss(file, to.data.frame=TRUE, use.value.labels = FALSE)
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
  names.match <- rep(NA,nrow(dat.names))

  for(j in 1:nrow(dat.names)){
    compare <- as.character(as.vector(dat.names[j,2:ncol(dat.names)]))
    compare <- compare[compare != ""]
    if(all(names(dat) %in% compare) & all(compare %in% names(dat))){
      names.match[j] <- TRUE
    }else{
      names.match[j] <- FALSE
    }

  }

  #subset to matching names
  names.match[1] <- TRUE
  dat.names <- dat.names[names.match,]

  #Matched All Varaible Names but need to check order
  if(nrow(dat.names) == 2){
    #Correct match do nothing
  }else if(nrow(dat.names) == 1){
    message("Unknown combindation of field names")
    stop()
  }else if(nrow(dat.names) > 2){
    message("More than one possible match")
    if(type == "acc"){
      if(length(unique(dat$a1_26)) == 4){
        message("Chossing Post 2006 Method based on a1_26")
        dat.names <- dat.names[dat.names$V1 %in% c("acc.names.new","acc.names.2006"),]
      }else{
        message("Chossing Post 1999 Method based on a1_26")
        dat.names <- dat.names[dat.names$V1 %in% c("acc.names.new","acc.names.1999"),]
      }
    }else{
      message("Unknown Resolution Method")
      stop()
    }

  }else{
    message("Error with number of field names")
    stop()
  }

  dat.names.sub <- data.frame(t(dat.names[,2:ncol(dat.names)]), stringsAsFactors = F)
  names(dat.names.sub) <- c("new","old")
  dat.names.new <- dat.names.sub$new[match(names(dat), dat.names.sub$old)]
  names(dat) <- dat.names.new


  #Check for any columns that are missing ann add them
  missing.col <- dat.names.sub$new[!(dat.names.sub$new %in% names(dat))]
  dat[,missing.col] <- NA

  #Reorder the dat into the standard order
  dat <- dat[,dat.names.sub$new]

  #Clean up dates in the Acc
  #To store a singe DateTime Field
  if(type == "acc"){
    res <- mapply(get.datetime,dat$Year,dat$Month, dat$Day, dat$Hour, dat$Minute, dat$Date, dat$Time, SIMPLIFY = FALSE)
    dat$DateTime <- do.call("c",res)
    rm(res)
    dat <- dat[,names(dat)[!names(dat) %in% c("Month","Day","Hour","Minute","Date","Time")]] #Remove Columns
  }

  #Convert Acc to SF
  if(type == "acc"){
    #Check for columns to use
    if(!(all(is.na(dat$Easting)) | all(is.na(dat$Northing)))){
      # Use Easting and Northings
      #remove NAs
      dat$Easting[is.na(dat$Easting)] <- 0
      dat$Northing[is.na(dat$Northing)] <- 0

      #Check number of character used, some year use lower precision cooridantes
      if(max(nchar(dat$Easting)) == 5){
        dat$Easting <- dat$Easting * 10
      }else if(max(nchar(dat$Easting)) == 4){
        dat$Easting <- dat$Easting * 100
      }

      if(max(nchar(dat$Northing)) == 5){
        dat$Northing <- dat$Northing * 10
      }else if(max(nchar(dat$Northing)) == 4){
        dat$Northing <- dat$Northing * 100
      }

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
        if(all(unique(dat[,l]) %in% c(NA,as.character(-1:100)))){
          dat[,l] <- as.integer(dat[,l])
        }
      }
    }

  }


  for(k in 1:ncol(dat)){
    #Skip Geometry and Date Time columns
    if(length(class(dat[,k])) == 1){
      #Check that we can do the replacement
      if((class(dat[,k]) %in% c("integer","numeric")  ) & (names(dat)[k] %in% columns)){
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

stats19.clean <- function(dat, type = c("acc", "cas", "veh")){

  #SF Data frame convert to data frame and then convert back at end
  #Otherwise numeric column subsetting does not work
  if("sf" %in% class(dat)){
    toSF <- TRUE
    dat <- as.data.frame(dat)
  }else{
    toSF <- FALSE
  }



  #Type specific changes
  if(type == "acc"){
    #Force out some rouge values
    dat$RoadNumber2 <- as.integer(as.character(dat$RoadNumber2))
  }

  #Change factors to characters
  classes <- lapply(dat, class)
  for(i in 1:length(classes)){
    if(classes[i] == "factor"){
      tmp.col <- as.character(dat[,i])
      dat[,i] <- tmp.col
    }
  }
  rm(i)

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

    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Crossing from n-side - masked by parked or stationary veh.")] <- "Crossing from nearside - masked by parked or stationary vehicle"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Crossing from drivers nearside")] <- "Crossing from driver's nearside"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Crossing from drivers offside")] <- "Crossing from driver's offside"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Crossing from n-side - masked by parked/stationary veh.")] <- "Crossing from nearside - masked by parked or stationary vehicle"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Crossing from o-side - masked by parked/stationary veh.","Crossing from o-side - masked by parked or stationary veh.")] <- "Crossing from offside - masked by  parked or stationary vehicle"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("In c-way, stationary - not crossing (standing/playing)","In c-way, stationary - not crossing (standing or playing)")] <- "In carriageway, stationary - not crossing  (standing or playing)"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("In c-way, stationary - not crossing etc - masked etc")] <- "In carriageway, stationary - not crossing  (standing or playing) - masked by parked or stationary vehicle"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Not pedestrian")] <- "Not a Pedestrian"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Walking along in c-way, back to traffic")] <- "Walking along in carriageway, back to traffic"
    dat$PedestrianMovement[dat$PedestrianMovement %in% c("Walking along in c-way, facing traffic")] <- "Walking along in carriageway, facing traffic"

    if(class(dat$PedestrianDirection) == "numeric"){
      castype <- read.csv("../stats19/data/cas/PedestrianDirection.csv", stringsAsFactors = F, header = T)
      tmp.cas <- castype$label[match(dat$PedestrianDirection,castype$code)]
      dat$PedestrianDirection <- tmp.cas
      rm(castype,tmp.cas)
    }

    dat$PedestrianDirection[dat$PedestrianDirection == "NULL or Invalid value"] <- NA
    dat$PedestrianDirection[dat$PedestrianDirection == "Unknown (from 1999)"] <- "Unknown"

    if(class(dat$SchoolPupil) == "numeric"){
      castype <- read.csv("../stats19/data/cas/SchoolPupil.csv", stringsAsFactors = F, header = T)
      tmp.cas <- castype$label[match(dat$SchoolPupil,castype$code)]
      dat$SchoolPupil <- tmp.cas
      rm(castype,tmp.cas)
    }

    dat$SchoolPupil[dat$SchoolPupil == "Undefined"] <- NA
    dat$SchoolPupil[dat$SchoolPupil == "Other (from 1994)"] <- "Other"

    dat$CarPassenger[dat$CarPassenger %in% c("Undefined","NULL or Invalid value","Data missing or out of range")] <- NA

    dat$BusPassenger[dat$BusPassenger %in% c("Undefined","NULL or Invalid value","Data missing or out of range")] <- NA

    if(class(dat$CasualtyType) == "numeric"){
      castype <- read.csv("../stats19/data/cas/CasualtyType.csv", stringsAsFactors = F, header = T)
      tmp.cas <- castype$label[match(dat$CasualtyType,castype$code)]
      dat$CasualtyType <- tmp.cas
      rm(castype,tmp.cas)
    }

    dat$CasualtyType[dat$CasualtyType %in% c("Goods veh (> 3.5t.,< 7.5t.)occupant")] <- "Goods veh (over 3.5t.,under 7.5t.)occupant"
    dat$CasualtyType[dat$CasualtyType %in% c("Goods vehicle (7.5 tonnes mgw and over) occupant")] <- "Goods veh (7.5t. and over) occupant"
    dat$CasualtyType[dat$CasualtyType %in% c("Motor cycle (up to 125cc) rider or pass")] <- "Motorcycle 125cc and under rider or passenger"



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
    dat$VehicleType[dat$VehicleType %in% c("Minibus (8 - 16 passenger seats) (from 1999)","Minibus (8-16 passenger seats)")] <- "Minibus (8 - 16 passenger seats)"
    dat$VehicleType[dat$VehicleType %in% c("Goods over 3.5t and under 7.5t mgw", "Goods over 3.5t. and under 7.5t. (from 1999)")] <- "Goods over 3.5t. and under 7.5t"
    dat$VehicleType[dat$VehicleType == "Goods vehicle - unknown weight (self rep only)"] <- "Goods vehicle - unknown weight"
    dat$VehicleType[dat$VehicleType %in% c("Goods 3.5 tonnes mgw or under", "Goods 3.5 tonnes maximum gross weight or under", "Van / Goods 3.5 tonnes mgw or under")] <- "Goods under 3.5t."
    dat$VehicleType[dat$VehicleType == "Motorcycle over 500cc (from 2005)"] <- "Motorcycle over 500cc"
    dat$VehicleType[dat$VehicleType == "Motorcycle over 125cc and up to 500cc (from 2005)"] <- "Motorcycle over 125cc and up to 500cc"
    dat$VehicleType[dat$VehicleType %in% c("Goods 7.5 tonnes mgw and over", "Goods 7.5 tonnes mgw and over (from 1999)")] <- "Goods over 7.5t."

    dat$VehAgeBand[dat$VehAgeBand %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$LeftHandDrive[dat$LeftHandDrive %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$TowingArticulation[dat$TowingArticulation %in% c("No tow or articulation")] <- "No tow/articulation"

    if(class(dat$ForeignVehicle) == "numeric"){
      darea <- read.csv("../stats19/data/veh/ForeignVehicle", stringsAsFactors = F, header = T)
      tmp.veh <- darea$label[match(dat$ForeignVehicle,darea$code)]
      dat$ForeignVehicle <- tmp.veh
      rm(darea,tmp.veh)
    }

    if(class(dat$HitRun) == "numeric"){
      darea <- read.csv("../stats19/data/veh/HitRun", stringsAsFactors = F, header = T)
      tmp.veh <- darea$label[match(dat$HitRun,darea$code)]
      dat$HitRun <- tmp.veh
      rm(darea,tmp.veh)
    }

    dat$HitRun[dat$HitRun %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$LocationRestrictedAway[dat$LocationRestrictedAway %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$LocationRestrictedAway[dat$LocationRestrictedAway %in% c("Cycleway or shared use footway (not part of  main carriageway)")] <- "Cycleway/shared use footway (not part of main carriageway)"
    dat$LocationRestrictedAway[dat$LocationRestrictedAway %in% c("Busway (inc guided busway)")] <- "Busway (including guided busway)"
    dat$LocationRestrictedAway[dat$LocationRestrictedAway %in% c("On main c'way - not in restricted lane","On main carriageway - not restr. lane","On main carriageway, not in restricted lane")] <- "On main carriageway - not in restricted lane"


    dat$Junction[dat$Junction %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$Junction[dat$Junction %in% c("Mid Junction - on roundabout or on main road")] <- "Mid junction - on roundabout or on main road"
    dat$Junction[dat$Junction %in% c("Not at or within 20 metres of junction","Not at, or within 20 metres of, junction")] <- "Not at junction"

    dat$SkiddingOverturning[dat$SkiddingOverturning %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$ObjectInCarriageway[dat$ObjectInCarriageway %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA


    if(class(dat$JourneyPurpose) == "numeric"){
      jpurp <- read.csv("../stats19/data/veh/JourneyPurpose.csv", stringsAsFactors = F, header = T)
      tmp.veh <- jpurp$label[match(dat$JourneyPurpose,jpurp$code)]
      dat$JourneyPurpose <- tmp.veh
      rm(jpurp,tmp.veh)
    }

    dat$JourneyPurpose[dat$JourneyPurpose %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$PointofImpact[dat$PointofImpact %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$SexDriver[dat$SexDriver %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$ObjectOffCarriageway[dat$ObjectOffCarriageway %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA

    dat$VehicleLetter[dat$VehicleLetter %in% c("Foreign or Diplomatic")] <- "Foreign or diplomatic"

    if(class(dat$DriverArea) == "numeric"){
      darea <- read.csv("../stats19/data/veh/DriverArea", stringsAsFactors = F, header = T)
      tmp.veh <- darea$label[match(dat$DriverArea,darea$code)]
      dat$DriverArea <- tmp.veh
      rm(darea,tmp.veh)
    }



  }else if (type == "acc"){

    #Add police force if number is provided
    if(class(dat$Police) == "numeric"){
      tmp.list <- read.csv("../stats19/data/acc/Police.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$Police,tmp.list$code)]
      dat$Police <- tmp.acc
      rm(tmp.list,tmp.acc)
    }


    #Add LA  if number is provided
    if(class(dat$LA) == "numeric"){
      tmp.list <- read.csv("../stats19/data/acc/LA.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$LA,tmp.list$code)]
      dat$LA <- tmp.acc
      rm(tmp.list,tmp.acc)
    }

    dat$JunctionDetail[dat$JunctionDetail %in% c("Data missing or out of range", "NULL or Invalid value","Undefined")] <- NA

    dat$JunctionControl[dat$JunctionControl %in% c("Data missing or out of range", "NULL or Invalid value","Undefined")] <- NA

    dat$RoadClass2[dat$RoadClass2 %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    if("Automatic traffic signal out" %in% dat$RoadClass2){
      message(paste0("unexpected value in RoadClass2"))
    }

    dat$RoadType[dat$RoadType %in% c("Data missing or out of range", "NULL or Invalid value","Unknown")] <- NA
    dat$RoadType[dat$RoadType %in% c("Slip road (from 2005)")] <- "Slip road"
    dat$RoadType[dat$RoadType %in% c("One way street (from 2005)")] <- "One way street"


    dat$CrossingControl[dat$CrossingControl %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA
    dat$CrossingControl[dat$CrossingControl %in% c("No crossing 50m or phys. crossing not controlled","No crossing in 50m or physical crossing not controlled by au","None within 50 metres")] <- "None"

    #Add CrossingFacilities  if number is provided
    if(class(dat$CrossingFacilities) == "numeric"){
      tmp.list <- read.csv("../stats19/data/acc/CrossingFacilities.csv", stringsAsFactors = F, header = T)
      tmp.acc <- tmp.list$label[match(dat$CrossingFacilities,tmp.list$code)]
      dat$CrossingFacilities <- tmp.acc
      rm(tmp.list,tmp.acc)
    }

    dat$CrossingFacilities[dat$CrossingFacilities %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$CrossingFacilities[dat$CrossingFacilities %in% c("Pelican, puffin, toucan or similar non-junct pedestrian ligh","Pelican or similar non-junct pedestrian light crossing")] <- "Pelican, puffin, toucan or similar non-junction pedestrian light crossing"
    dat$CrossingFacilities[dat$CrossingFacilities %in% c("No physical crossing facilities within 50 metres")] <- "No crossing facility within 50 metres"

    dat$Surface[dat$Surface %in% c("Data missing or out of range", "NULL or Invalid value")] <- NA
    dat$Surface[dat$Surface %in% c("Flood over 3cm. deep")] <- "Flood (surface water over 3cm deep)"
    dat$Surface[dat$Surface %in% c("Frost/ice")] <- "Frost or ice"
    dat$Surface[dat$Surface %in% c("Mud (from 1999)")] <- "Mud"
    dat$Surface[dat$Surface %in% c("Oil or diesel (from 1999)")] <- "Oil or diesel"
    dat$Surface[dat$Surface %in% c("Wet/damp")] <- "Wet or damp"


    dat$Light[dat$Light %in% c("Darkness - lightng unknown")] <- "Darkness - lighting unknown"

    dat$SpecialConditions[dat$SpecialConditions %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA
    dat$SpecialConditions[dat$SpecialConditions %in% c("Mud (from 2005 - see A23)")] <- "Mud"
    dat$SpecialConditions[dat$SpecialConditions %in% c("Oil or diesel (from 2005 - see A23)")] <- "Oil or diesel"
    dat$SpecialConditions[dat$SpecialConditions %in% c("Road sign or marking defective or obscured","Permanent road signing or marking defective or obscured")] <- "Road signs or markings defective or obscured"
    dat$SpecialConditions[dat$SpecialConditions %in% c("Road works present")] <- "Roadworks"
    dat$SpecialConditions[dat$SpecialConditions %in% c("Auto traffic signal - out")] <- "Automatic traffic signal out"
    dat$SpecialConditions[dat$SpecialConditions %in% c("Auto signal part defective")] <- "Automatic traffic signal partially defective"



    dat$Hazards[dat$Hazards %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA
    dat$Hazards[dat$Hazards %in% c("Any animal in carriageway (except ridden horse) (from 2005)")] <- "Any animal in carriageway (except ridden horse)"
    dat$Hazards[dat$Hazards %in% c("Pedestrian in carriageway - not injured (from 2005)")] <- "Pedestrian in carriageway - not injured"
    dat$Hazards[dat$Hazards %in% c("Involvement with previous accident")] <- "Previous accident"

    dat$PoliceOfficerAttend[dat$PoliceOfficerAttend %in% c("Data missing or out of range", "NULL or Invalid value", "Undefined")] <- NA
    dat$PoliceOfficerAttend[dat$PoliceOfficerAttend %in% c("No - accident was reported using a self completion  form (self rep only)")] <- "No (acc reported using self-comp form (Self-Comp Form only)"


    dat$PlaceReported[dat$PlaceReported == "At Scene"] <- "At scene"

    #Add LAHighway  if number is provided

    tmp.list <- read.csv("../stats19/data/acc/LAHighway.csv", stringsAsFactors = F, header = T)
    tmp.acc <- tmp.list$label[match(dat$LAHighway,tmp.list$code)]
    dat$LAHighway <- tmp.acc
    rm(tmp.list,tmp.acc)

    if(toSF){
      dat <- st_as_sf(dat)
    }



  }
  return(dat)
}
