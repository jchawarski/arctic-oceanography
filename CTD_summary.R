#This codue uses R version 3.3.3

library("oce")
library("lattice")
library("dplyr")
library("data.table")

####   SUMMARIZE SEABIRD CTD DATA   ####
{
files <- list.files(path="D:/NEG 2017/Leg 1/CTD/Dat", full.names = TRUE, pattern= "*.cnv")  # load files from CTD folder
    CTD.cnv <- lapply(files, function(i){ctdTrim(read.ctd.sbe(i))})                          # concatenates and trims upcast from all CTD files into large list
        meta.tbl <- setNames(data.frame(matrix(ncol = 3, nrow = 118)), c("Site", "Date", "ID")) # create empty df for summary data
                    # in this example nrow is the number of cast files contained in the folder - change it as needed
          meta.tbl[,1] <-  paste(substr(sapply(CTD.cnv, '[[', "filename"), 26, 50))             # selects and trims file name, put in in blank matrix
          meta.tbl[,2] <- sapply(CTD.cnv, function(i){paste(unique(i[["date"]]))})                # pulls date from each @data in data.in
            uniqueID <- c(1:118)                                                                   # create unique ID by cast
                    #once again change the length of uniqueID to match number of CTD casts
              meta.tbl[,3] <-  uniqueID                                                             # assigns a unique ID

CTD.all <- lapply(CTD.cnv, function(i){ rbind(unique(data.frame(i[["data"]]))) }) %>%  # creates a list of dataframes (of each cast)
               mapply(cbind, ., ID = uniqueID, SIMPLIFY = F) %>%                         # adds ID column with uniqueID to each df in list
                    bind_rows() %>%                                                        # binds all dfs
                       left_join(meta.tbl, ., by="ID")                                      # joins 

CTD.sum.tbl <-  CTD.all %>% filter(depth <= 100) %>%               # filters all data below 100 m depth
                   group_by(Site, Date) %>%                               # group fnx, allows headers to carry over
                      summarise(
                        avgT = mean(temperature), 
                        avgS = mean(salinity), 
                        lat = mean(latitude, na.rm = T),
                        lon = mean(longitude, na.rm = T)) 

write.csv(CTD.sum.tbl, "NEG.CTD.100m.TS.Summary.csv")                               #create file


}

#single site
{
st1 <- read.ctd.sbe("26DA.2017.10.101.cnv") #creates object CTD
st1.df <- data.frame(st.1@data) 

st1.df$site <- paste(substr(st.1@metadata[["filename"]], 22, 37)) # selects the filename from metadata, trims, and inserts into Df
st1.df$date <- paste(st.1@metadata[["date"]]) # selects the date from metadata and inserts in Df

st1.sum <- st1.df %>% filter(depth <= 100) %>%
  group_by(site, date) %>%
  summarise(avgT = mean(temperature), 
            avgS = mean(salinity), 
            lat = mean(latitude, na.rm = T),
            lon = mean(longitude, na.rm = T)) 
}

