library(tidyverse)




hdds_calc <- function(data){




    ten_groups <- c("GrainsRootsTubers",#grains roots_tubers
                    "Legumes",#pulses
                    "Nuts_Seeds", #nuts_seeds
                    "Veg_Leafy", #green_veg
                    "VitA_Veg_Fruit",#vitA_veg vitA_fruits
                    "Vegetables", #other_veg
                    "Fruits",#other_fruits
                    "Meat",#meat_poultry organ_meat fish_seafood
                    "Eggs", #eggs
                    "Milk_Dairy" )#milk

    fourteen_groups <- c("grains",
                         "roots_tubers",
                         "pulses",
                         "nuts_seeds",
                         "milk",
                         "organ_meat",
                         "meat_poultry",
                         "fish_seafood",
                         "eggs",
                         "green_veg",
                         "vitA_veg",
                         "vitA_fruits",
                         "other_veg",
                         "other_fruits")

    potential_columns <- c("good_season",
                           "bad_season",
                           "last_month",
                           "source_good",
                           "source_bad",
                           "source_last_month")

    ten_groups_columns <- sapply(potential_columns, function(x) paste0(ten_groups,"_",x),simplify = F )
    fourteen_groups_columns <- sapply(potential_columns, function(x) paste0(fourteen_groups,"_",x),simplify = F )

    time_values <- c("daily","fewperweek","weekly","fewpermonth","monthly","never")
    conversion <- c(1,1,1,0,0,0)


    outputs <- c()

    # HDDS for fourteen food groups, looking at the bad season
    if(all(fourteen_groups_columns$bad_season %in% colnames(data)))
    {
        bad_season_14 <-  switch_units(data[,fourteen_groups_columns$bad_season],time_values,conversion)
        colnames(bad_season_14) <-gsub("_bad_season","",colnames(bad_season_14))
        bad_season_10 <- collapse_14_groups(bad_season_14)
        HDDS_bad_season <- rowSums(bad_season_10, na.rm = T)
        outputs$HDDS_bad_season <- HDDS_bad_season

        # Looking at the sources of the food
        if(all(fourteen_groups_columns$source_bad %in% colnames(data)))
        {
            bad_season_source_14 <- data[,fourteen_groups_columns$source_bad]
            colnames(bad_season_source_14) <-gsub("source_bad","",colnames(bad_season_14))

            bad_season_bought_14 <-  as_tibble(sapply(bad_season_source_14,function(x) as.numeric(grepl("bought",x))))
            bad_season_bought_10 <- collapse_14_groups(bad_season_bought_14)
            bad_season_bought_10[bad_season_10==0]<-0
            HDDS_bad_season_bought <- rowSums(bad_season_bought_10, na.rm = T)
            outputs$HDDS_bad_season_bought <-HDDS_bad_season_bought

            bad_season_farm_sourced_14 <-  as_tibble(sapply(bad_season_source_14,function(x) as.numeric(grepl("on-farm",x))))
            bad_season_farm_sourced_10 <- collapse_14_groups(bad_season_farm_sourced_14)
            bad_season_farm_sourced_10[bad_season_10==0]<-0
            HDDS_bad_season_farm <- rowSums(bad_season_farm_sourced_10, na.rm = T)
            outputs$HDDS_bad_season_farm <-HDDS_bad_season_farm

        }
    }

    # HDDS for fourteen food groups, looking at the good season

    if(all(fourteen_groups_columns$good_season %in% colnames(data)))
    {
        good_season_14 <-  switch_units(data[,fourteen_groups_columns$good_season],time_values,conversion)
        colnames(good_season_14) <-gsub("_good_season","",colnames(good_season_14))
        good_season_10 <- collapse_14_groups(good_season_14)
        HDDS_good_season <- rowSums(good_season_10, na.rm = T)
        outputs$HDDS_good_season <- HDDS_good_season

        # Looking at the sources during the good season
        if(all(fourteen_groups_columns$source_good %in% colnames(data)))
        {
            good_season_source_14 <- data[,fourteen_groups_columns$source_good]
            colnames(good_season_source_14) <-gsub("source_good","",colnames(good_season_14))

            good_season_bought_14 <-  as_tibble(sapply(good_season_source_14,function(x) as.numeric(grepl("bought",x))))
            good_season_bought_10 <- collapse_14_groups(good_season_bought_14)
            good_season_bought_10[good_season_10==0]<-0
            HDDS_good_season_bought <- rowSums(good_season_bought_10, na.rm = T)
            outputs$HDDS_good_season_bought <-HDDS_good_season_bought

            good_season_farm_sourced_10 <-  as_tibble(sapply(good_season_source_14,function(x) as.numeric(grepl("on-farm",x))))
            good_season_farm_sourced_10 <- collapse_14_groups(good_season_farm_sourced_14)
            good_season_farm_sourced_10[good_season_10==0]<-0
            HDDS_good_season_farm <- rowSums(good_season_farm_sourced_10, na.rm = T)
            outputs$HDDS_good_season_farm <-HDDS_good_season_farm

        }
    }

    # HDDS for fourteen food groups, looking at the last month
    if(all(fourteen_groups_columns$last_month %in% colnames(data)))
    {
        last_month_14 <-  switch_units(data[,fourteen_groups_columns$last_month],time_values,conversion)
        colnames(last_month_14) <-gsub("_last_month","",colnames(last_month_14))
        last_month_10 <- collapse_14_groups(last_month_14)
        HDDS_last_month <- rowSums(last_month_10, na.rm = T)
        outputs$HDDS_last_month <- HDDS_last_month

        # Looking at the sources over the last month
        if(all(fourteen_groups_columns$source_last_month %in% colnames(data)))
        {
            last_month_source_14 <- data[,fourteen_groups_columns$source_last_month]
            colnames(last_month_source_14) <-gsub("source_last_month","",colnames(last_month_14))

            last_month_bought_14 <-  as_tibble(sapply(last_month_source_14,function(x) as.numeric(grepl("bought",x))))
            last_month_bought_10 <- collapse_14_groups(last_month_bought_14)
            last_month_bought_10[last_month_10==0]<-0
            HDDS_last_month_bought <- rowSums(last_month_bought_10, na.rm = T)
            outputs$HDDS_last_month_bought <-HDDS_last_month_bought

            last_month_farm_sourced_14 <-  as_tibble(sapply(last_month_source_14,function(x) as.numeric(grepl("on-farm",x))))
            last_month_farm_sourced_10 <- collapse_14_groups(last_month_farm_sourced_14)
            last_month_farm_sourced_10[last_month_10==0]<-0
            HDDS_last_month_farm <- rowSums(last_month_farm_sourced_10, na.rm = T)
            outputs$HDDS_last_month_farm <-HDDS_last_month_farm

        }
    }


    if(all(ten_groups_columns$good_season %in% colnames(data)))
    {
        good_season_10 <-  switch_units(data[,ten_groups_columns$good_season],time_values,conversion)
        colnames(good_season_10) <-gsub("_good_season","",colnames(good_season_10))
        HDDS_good_season <- rowSums(good_season_10, na.rm = T)
        outputs$HDDS_good_season <- HDDS_good_season

        if(all(ten_groups_columns$source_good %in% colnames(data)))
        {
            good_season_source_10 <- data[,ten_groups_columns$source_good]
            colnames(good_season_source_10) <-gsub("source_good","",colnames(good_season_10))

            good_season_bought_10 <-  as_tibble(sapply(good_season_source_10,function(x) as.numeric(grepl("bought",x))))
            good_season_bought_10[good_season_10==0]<-0
            HDDS_good_season_bought <- rowSums(good_season_bought_10, na.rm = T)
            outputs$HDDS_good_season_bought <-HDDS_good_season_bought

            good_season_farm_sourced_10 <-  as_tibble(sapply(good_season_source_10,function(x) as.numeric(grepl("on-farm",x))))
            good_season_farm_sourced_10[good_season_10==0]<-0
            HDDS_good_season_farm <- rowSums(good_season_farm_sourced_10, na.rm = T)
            outputs$HDDS_good_season_farm <-HDDS_good_season_farm

        }
    }

    if(all(ten_groups_columns$bad_season %in% colnames(data)))
    {
        bad_season_10 <-  switch_units(data[,ten_groups_columns$bad_season],time_values,conversion)
        colnames(bad_season_10) <-gsub("_bad_season","",colnames(bad_season_10))
        HDDS_bad_season <- rowSums(bad_season_10, na.rm = T)
        outputs$HDDS_bad_season <- HDDS_bad_season
        outputs$HDDS_bad_season <-HDDS_bad_season

        if(all(ten_groups_columns$source_bad %in% colnames(data)))
        {
            bad_season_source_10 <- data[,ten_groups_columns$source_bad]
            colnames(bad_season_source_10) <-gsub("source_bad","",colnames(bad_season_10))

            bad_season_bought_10 <-  as_tibble(sapply(bad_season_source_10,function(x) as.numeric(grepl("bought",x))))
            bad_season_bought_10[bad_season_10==0]<-0
            HDDS_bad_season_bought <- rowSums(bad_season_bought_10, na.rm = T)
            outputs$HDDS_bad_season_bought <-HDDS_bad_season_bought

            bad_season_farm_sourced_10 <-  as_tibble(sapply(bad_season_source_10,function(x) as.numeric(grepl("on-farm",x))))
            bad_season_farm_sourced_10[bad_season_10==0]<-0
            HDDS_bad_season_farm <- rowSums(bad_season_farm_sourced_10, na.rm = T)
            outputs$HDDS_bad_season_farm <-HDDS_bad_season_farm

        }
    }

    if(all(ten_groups_columns$good_season %in% colnames(data)))
    {
        good_season_10 <-  switch_units(data[,ten_groups_columns$good_season],time_values,conversion)
        colnames(good_season_10) <-gsub("_good_season","",colnames(good_season_10))
        HDDS_good_season <- rowSums(good_season_10, na.rm = T)
        outputs$HDDS_good_season <- HDDS_good_season

        if(all(ten_groups_columns$source_good %in% colnames(data)))
        {
            good_season_source_10 <- data[,ten_groups_columns$source_good]
            colnames(good_season_source_10) <-gsub("source_good","",colnames(good_season_10))

            good_season_bought_10 <-  as_tibble(sapply(good_season_source_10,function(x) as.numeric(grepl("bought",x))))
            good_season_bought_10[good_season_10==0]<-0
            HDDS_good_season_bought <- rowSums(good_season_bought_10, na.rm = T)
            outputs$HDDS_good_season_bought <-HDDS_good_season_bought

            good_season_farm_sourced_10 <-  as_tibble(sapply(good_season_source_10,function(x) as.numeric(grepl("on-farm",x))))
            good_season_farm_sourced_10[good_season_10==0]<-0
            HDDS_good_season_farm <- rowSums(good_season_farm_sourced_10, na.rm = T)
            outputs$HDDS_good_season_farm <-HDDS_good_season_farm

        }
    }


    if(all(ten_groups_columns$last_month %in% colnames(data)))
    {
        last_month_10 <-  switch_units(data[,ten_groups_columns$last_month],time_values,conversion)
        colnames(last_month_10) <-gsub("_last_month","",colnames(last_month_10))
        HDDS_last_month <- rowSums(last_month_10, na.rm = T)
        outputs$HDDS_last_month <- HDDS_last_month

        if(all(ten_groups_columns$source_last_month %in% colnames(data)))
        {
            last_month_source_10 <- data[,ten_groups_columns$source_last_month]
            colnames(last_month_source_10) <-gsub("source_last_month","",colnames(last_month_source_10))

            last_month_bought_10 <-  as_tibble(sapply(last_month_source_10,function(x) as.numeric(grepl("bought",x))))
            last_month_bought_10[last_month_10==0]<-0
            HDDS_last_month_bought <- rowSums(last_month_bought_10, na.rm = T)
            outputs$HDDS_last_month_bought <-HDDS_last_month_bought

            last_month_farm_sourced_10 <-  as_tibble(sapply(last_month_source_10,function(x) as.numeric(grepl("on-farm",x))))
            last_month_farm_sourced_10[last_month_10==0]<-0
            HDDS_last_month_farm <- rowSums(last_month_farm_sourced_10, na.rm = T)
            outputs$HDDS_last_month_farm <-HDDS_last_month_farm

        }
    }

    results <- as_tibble(outputs)
    return(results)
}




collapse_14_groups <- function(hdds_data){

    hdds_data$GrainsRootsTubers<- rowSums(data.frame(as.numeric(hdds_data$grains), as.numeric(hdds_data$roots_tubers)),na.rm = T)
    hdds_data$GrainsRootsTubers<-as.numeric(gsub('2',1,as.character(hdds_data$GrainsRootsTubers)))
    hdds_data$grains<-NULL
    hdds_data$roots_tubers<-NULL

    hdds_data$Legumes<- hdds_data$pulses
    hdds_data$pulses<-NULL

    hdds_data$Nuts_Seeds<- hdds_data$nuts_seeds
    hdds_data$nuts_seeds<-NULL

    hdds_data$Veg_Leafy<- hdds_data$green_veg
    hdds_data$green_veg<-NULL

    hdds_data$VitA_Veg_Fruit<- rowSums(data.frame(as.numeric(hdds_data$vitA_veg), as.numeric(hdds_data$vitA_fruits)),na.rm = T)
    hdds_data$VitA_Veg_Fruit<-as.numeric(gsub('2',1,as.character(hdds_data$VitA_Veg_Fruit)))
    hdds_data$vitA_veg<-NULL
    hdds_data$vitA_fruits<-NULL

    hdds_data$Vegetables<- hdds_data$other_veg
    hdds_data$other_veg<-NULL

    hdds_data$Fruits<- hdds_data$other_fruits
    hdds_data$other_fruits<-NULL


    hdds_data$Meat<- rowSums(data.frame(as.numeric(hdds_data$meat_poultry), as.numeric(hdds_data$organ_meat), as.numeric(hdds_data$fish_seafood)),na.rm = T)
    hdds_data$Meat<-as.numeric(gsub('2',1,as.character(hdds_data$Meat)))
    hdds_data$Meat<-as.numeric(gsub('3',1,as.character(hdds_data$Meat)))
    hdds_data$meat_poultry<-NULL
    hdds_data$organ_meat<-NULL
    hdds_data$fish_seafood<-NULL

    hdds_data$Eggs<- hdds_data$eggs
    hdds_data$eggs<-NULL

    hdds_data$Milk_Dairy<- hdds_data$milk
    hdds_data$milk<-NULL





    return(hdds_data)
}
