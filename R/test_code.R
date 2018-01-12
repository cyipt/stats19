foo <- as.data.frame(veh.all)
foo$geometry <- NULL
for(i in 1:length(names(foo))){
  message(paste0(names(foo)[i]," is of class ",class(foo[,names(foo)[i]])))
  message("**********")
  if(class(foo[,names(foo)[i]]) %in% c("character","factor")){
    tab <- as.data.frame(table(foo[,i]))
    if(nrow(tab) < 500){
      print(tab)
      invisible(readline(prompt="Press [enter] to continue"))
    }else{
      message("More than 500 unique values")
    }
  }


}


dat2 <- dat[,c("a1_17","a1_18","a1_20a","a1_20b","a1_23","a1_24","a1_25","a1_26")]


files <- list.files("../stats19/data/veh", full.names = T)
for(i in 1:length(files)){
  tmp <- read.csv(files[i], header = T)
  if(all(names(tmp) %in% c("code","lable"))) {
    message(paste0("Error with ",files[i]))
  }
}

foo2 <- foo[!is.na(foo$Surface),]
foo2$Surface <- as.character(foo2$Surface)
foo2 <- foo2[foo2$Surface == "Data missing or out of range",]
