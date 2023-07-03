cropcaldir <- '/data/work/Ispra/LUCAS/CROP/LUCAS_C_vision/cropcalendar'
cropcals <- list.files(cropcaldir, full.names = T)

#check potato overlap 
cropcals_pot <- cropcals[grep('B21', cropcals)]

#early ware
cropcals_pot1 <- read.csv(cropcals_pot[1], stringsAsFactors = F)
cropcals_pot1 <- cropcals_pot1[complete.cases(cropcals_pot1), ]
nrow(cropcals_pot1)

#late wear
cropcals_pot2 <- read.csv(cropcals_pot[2], stringsAsFactors = F)
cropcals_pot2 <- cropcals_pot2[complete.cases(cropcals_pot2), ]
nrow(cropcals_pot2)

#take only those countries that have both early and late wear
cropcals_pot1_in <- cropcals_pot1[cropcals_pot1$Country %in% cropcals_pot2$Country,]
cropcals_pot2_in <- cropcals_pot2[cropcals_pot2$Country %in% cropcals_pot1$Country,]

#should be TRUE
nrow(cropcals_pot1_in) == nrow(cropcals_pot2_in)

#compare 
cropcals_pot_comp <- as.data.frame(cropcals_pot1_in == cropcals_pot2_in)
cropcals_pot_comp$Region <- cropcals_pot1_in$Region
cropcals_pot_comp$Country <- cropcals_pot1_in$Country

#dunno what this is fak it
cropcals_pot_same <- data.frame()
for(i in 1:nrow(cropcals_pot1)){
  dfint <- cropcals_pot1[i,]
  dfint2 <- cropcals_pot2[cropcals_pot2$Region == dfint$Region,]
  
  if(all(dfint==dfint2)){
    print(paste(i, 't'))
    cropcals_pot_same[i] <- rbind(cropcals_pot_same, dfint)
  }
}

