---
title: "Looking at Sample Oyster Data"
author: "Arwa Sayegh"
date: "10 May, 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



## Data Source

Transport for London (TfL) provides Oyster card data which includes journey information for 5% sample of all oyster card data performed in a week during November 2009 on different public transport modes.

Here, I am going to explore this data set. Will first start by cleaning it and selecting a subset of interest, which is journeys made by London Underground (LUL), National Rail (NR), Docklands Light Railway (DLR), and London Overground (LRc). So, the journeys that I have removed for now are the London Buses and Croydon Tram journeys; this is mainly because detailed information of entry and exit are not available for these journeys, but also because I am more interested in non-road transport modes where journey routes (thus, distances between origin stations and destination stations) are rather fixed.

Here is how the data set looks like as a start:




```r
nrow(oyster_data)
```

```
## [1] 2623487
```

```r
head(oyster_data, n = 5)
```

```
##   downo daytype SubSystem  StartStn          EndStation EntTime
## 1     3     Tue       LUL Unstarted       Kings Cross M       0
## 2     4     Wed       LUL Unstarted        Sudbury Hill       0
## 3     3     Tue        NR Unstarted            Richmond       0
## 4     4     Wed        NR Unstarted             Romford       0
## 5     6     Fri        NR Unstarted Norwood Junction SR       0
##   EntTimeHHMM ExTime EXTimeHHMM ZVPPT JNYTYP DailyCapping FFare DFare
## 1       00:00    633      10:33 Z0104    TKT            N     0     0
## 2       00:00    447      07:27 Z0110    TKT            N     0     0
## 3       00:00    966      16:06 Z0304    TKT            N     0     0
## 4       00:00    657      10:57 Z0110    TKT            N     0     0
## 5       00:00    450      07:30 Z0104    TKT            N     0     0
##   RouteID           FinalProduct
## 1      XX   LUL Travelcard-7 Day
## 2      XX Freedom Pass (Elderly)
## 3      XX   LUL Travelcard-7 Day
## 4      XX Freedom Pass (Elderly)
## 5      XX   LUL Travelcard-7 Day
```

The number of bus/TRAM journeys is 


```r
length(which(oyster_data$SubSystem %in% c("LTB","TRAM")))
```

```
## [1] 1777663
```

## Data Manipulation



The data set is updated accordingly. There is also a number of journeys where either the start station or end station is not recorded. These are saved in a separate data frame which will be analysed later on to check the percentage of journeys per origin/destination, and to check for patterns of such cases.


```r
oyster_uns_unf <- subset(oyster_data, 
                         StartStn == "Unstarted" | 
                         EndStation %in% c("Unfinished","Not Applicable"))
length(which(oyster_uns_unf$StartStn == "Unstarted"))
```

```
## [1] 45994
```

```r
length(which(oyster_uns_unf$EndStation %in% c("Unfinished","Not Applicable")))
```

```
## [1] 35139
```

```r
oyster_data <- subset(oyster_data, 
                      StartStn != "Unstarted" &
                      !(EndStation %in% c("Unfinished","Not Applicable")) & 
                      as.character(StartStn) != as.character(EndStation) & 
                      ExTime != 0)
head(oyster_data)
```

```
##       downo daytype SubSystem            StartStn    EndStation EntTime
## 45990     2     Mon       LUL       Goodge Street    Totteridge    1000
## 45995     5     Thu       LUL        Preston Road     Northwood    1000
## 45999     5     Thu       LUL             Holborn  Bounds Green    1000
## 46004     1     Sun       LUL         Earls Court       Pimlico    1000
## 46006     3     Tue       LUL            Victoria Bethnal Green    1000
## 46007     6     Fri    LUL/NR Walthamstow Central       Arsenal    1000
##       EntTimeHHMM ExTime EXTimeHHMM   ZVPPT JNYTYP DailyCapping FFare
## 45990       16:40   1041      17:21   Z0110    TKT            N     0
## 45995       16:40   1024      17:04   Z0110    TKT            N     0
## 45999       16:40   1028      17:08   Z0104    TKT            N     0
## 46004       16:40   1021      17:01 -------    PPY            N   160
## 46006       16:40   1027      17:07   Z0102    TKT            N     0
## 46007       16:40   1017      16:57 -------    PPY            N    55
##       DFare RouteID            FinalProduct
## 45990     0      XX  Freedom Pass (Elderly)
## 45995     0      XX Freedom Pass (Disabled)
## 45999     0      XX   LUL Travelcard-Annual
## 46004   160      XX                    PAYG
## 46006     0      XX    LUL Travelcard-7 Day
## 46007    55      XX                    PAYG
```

So in one week, we have 759412 records with complete information on start/end station and start/endtime. Now, I move into some plotting and cleaning of individual columns.

downo is a numeric day of week column with 1 being Sunday and 7 being Saturday. daytype is a character column of the day of the week. We can see that the percentage of sampled journeys is lowest on weekends in comparison to weekdays, reflecting the lower PT demand during weekends.


```r
#adjusting factor levels
oyster_data$daytype <- factor(oyster_data$daytype, 
                              levels = c("Mon","Tue","Wed",
                                         "Thu","Fri","Sat","Sun"))
#presenting proportions of records by day
temp <- prop.table(xtabs(~daytype, oyster_data))
barplot(temp, col = RColorBrewer::brewer.pal(7, "Blues"), 
        width = 0.2,
        xlab = "Day of the Week", 
        ylab = "Percentage of Total Journeys")
```

![](01_data_manipulation_files/figure-html/dow-1.png)<!-- -->

Now, I move on to cleaning text/character columns for start and end stations. Here I have used the stringr pacakge and its r str_replace_all function to replace multiple patterns found in a column at the same time. I have also used the qdapRegex package and its rm_white function to remove all unnecessary white spaces.


```r
x <- c(" st " = " Street ", " STRT " = " Street ", 
       " Term " = " Terminal ", " Terms " = " Terminals ", 
       " Mkt " = " Market ", " Rd " = " Road ",
       " VIL " = " Village ", 
       " RdandB'sby " = " Road and Barnsbury ", 
       " WAGN TOC Gates" = " ", " FGW" = "", " SCL" = "", " 
       TOCs" = "", " NR" = " ", "&" = "and")
colrm <- c("StartStn", "EndStation")
oyster_data$StartStn <- stringr::str_replace_all(oyster_data$StartStn, x)
oyster_data$EndStation <- stringr::str_replace_all(oyster_data$EndStation, x)
oyster_data$Origin <- qdapRegex::rm_white(oyster_data$StartStn)
oyster_data$Destination <- qdapRegex::rm_white(oyster_data$EndStation)
oyster_data <- oyster_data[,!names(oyster_data) %in% colrm]
head(oyster_data, n = 5)
```

```
##       downo daytype SubSystem EntTime EntTimeHHMM ExTime EXTimeHHMM
## 45990     2     Mon       LUL    1000       16:40   1041      17:21
## 45995     5     Thu       LUL    1000       16:40   1024      17:04
## 45999     5     Thu       LUL    1000       16:40   1028      17:08
## 46004     1     Sun       LUL    1000       16:40   1021      17:01
## 46006     3     Tue       LUL    1000       16:40   1027      17:07
##         ZVPPT JNYTYP DailyCapping FFare DFare RouteID
## 45990   Z0110    TKT            N     0     0      XX
## 45995   Z0110    TKT            N     0     0      XX
## 45999   Z0104    TKT            N     0     0      XX
## 46004 -------    PPY            N   160   160      XX
## 46006   Z0102    TKT            N     0     0      XX
##                  FinalProduct        Origin   Destination
## 45990  Freedom Pass (Elderly) Goodge Street    Totteridge
## 45995 Freedom Pass (Disabled)  Preston Road     Northwood
## 45999   LUL Travelcard-Annual       Holborn  Bounds Green
## 46004                    PAYG   Earls Court       Pimlico
## 46006    LUL Travelcard-7 Day      Victoria Bethnal Green
```

Now we have clean version of the origin and destination columns. The next step is to clean up the start and end time in order to calculate journey times. For the sake of this exercise, I assumed that the day of the month is the downo column, although it is not clear whether the data was extracted from a single week or sampled across different weeks in November. The assumption here won't make any difference; we only care that the days are in the right order which is the case. Before adjusting the time, there exist 2701 rows where the entry time to the station was greater than 1440, 8186 rows where the exit time from the station was greater than 1440, and 2701 rows where both were greater than 1440; these are journeys started and/or finished after midnight, yet the recorded time/date did not capture that (kept them as previous days. So I have 



```r
#modifying error in time/day
#for those with both entry and exit errors
temp <- subset(oyster_data, EntTime > 1440 & ExTime > 1440)
temp$EntTime <- temp$EntTime - 1440
temp$ExTime <- temp$ExTime - 1440
temp$downo <- temp$downo + 1
levels(temp$daytype) <- c("Tue","Wed","Thu","Fri","Sat","Sun","Mon")
temp$EntTimeHHMM <- sprintf("%02i:%02i", 
                             lubridate::hour(lubridate::seconds_to_period(temp$EntTime*60)),
                             lubridate::minute(lubridate::seconds_to_period(temp$EntTime*60)))
temp$EXTimeHHMM <- sprintf("%02i:%02i", 
                             lubridate::hour(lubridate::seconds_to_period(temp$ExTime*60)),
                             lubridate::minute(lubridate::seconds_to_period(temp$ExTime*60)))
oyster_data <- subset(oyster_data, EntTime <= 1440 | ExTime <= 1440)
oyster_data <- rbind(oyster_data, temp)
#for those with exit errors
oyster_data$daytype_exit <- oyster_data$daytype
oyster_data$downo_exit <- oyster_data$downo
temp <- subset(oyster_data, ExTime > 1440)
temp$ExTime <- temp$ExTime - 1440
temp$downo_exit <- temp$downo_exit + 1
levels(temp$daytype_exit) <- c("Tue","Wed","Thu","Fri","Sat","Sun","Mon")
temp$EXTimeHHMM <- sprintf("%02i:%02i", 
                          lubridate::hour(lubridate::seconds_to_period(temp$ExTime*60)),
                          lubridate::minute(lubridate::seconds_to_period(temp$ExTime*60)))
oyster_data <- subset(oyster_data, ExTime <= 1440)
oyster_data <- rbind(oyster_data, temp)
rm(temp)
```

Based on the modified entry and exit times for each journey, three columns are added for the entry datetime, exit datetime, and entry type of day (weekend or weekday).


```r
#create single entry/exit datetime columns
oyster_data$EntryDateTime <- strptime(paste0(as.Date(paste0(oyster_data$downo, 
                                                            "/11/2009 "), 
                                         format = "%d/%m/%Y"), " ", 
                                 oyster_data$EntTimeHHMM),
                                  format = "%Y-%m-%d %H:%M")
oyster_data$ExitDateTime <- strptime(paste0(as.Date(paste0(oyster_data$downo_exit, 
                                                      "/11/2009 "), 
                                         format = "%d/%m/%Y"), " ", 
                                 oyster_data$EXTimeHHMM),
                                  format = "%Y-%m-%d %H:%M")
#based on the entry day, create weekend/weekday column
oyster_data$Week <- ifelse(oyster_data$downo %in% c(1,7), 
                           "weekend", "weekday")
temp <- prop.table(xtabs(~Week, oyster_data))
temp
```

```
## Week
## weekday weekend 
##  0.8529  0.1471
```

```r
rm(temp)
```

Final restructuring and renaming of data below.


```r
#some reordering and renaming of columns
oyster_data <- oyster_data[,c("Origin",
                              "downo",
                              "daytype",
                              "Week",
                              "EntTime",
                              "EntTimeHHMM",
                              "EntryDateTime",
                              "Destination",
                              "downo_exit",
                              "daytype_exit",
                              "ExTime",
                              "EXTimeHHMM",
                              "ExitDateTime",
                              "JNYTYP", 
                              "DailyCapping", 
                              "FFare")]
names(oyster_data) <- c("Origin",
                        "EntryDayNr",
                        "EntryDay",
                        "EnryWeek",
                        "EntryHour",
                        "EntryTime",
                        "EntryDateTime",
                        "Destination",
                        "ExitDayNr",
                        "ExitDay",
                        "ExitHour",
                        "ExitTime",
                        "ExitDateTime",
                        "JourneyType", 
                        "DailyCapping", 
                        "FFare")
```

This is how the final data set looks like.


```r
head(oyster_data, n = 5)
```

```
##              Origin EntryDayNr EntryDay EnryWeek EntryHour EntryTime
## 45990 Goodge Street          2      Mon  weekday      1000     16:40
## 45995  Preston Road          5      Thu  weekday      1000     16:40
## 45999       Holborn          5      Thu  weekday      1000     16:40
## 46004   Earls Court          1      Sun  weekend      1000     16:40
## 46006      Victoria          3      Tue  weekday      1000     16:40
##             EntryDateTime   Destination ExitDayNr ExitDay ExitHour
## 45990 2009-11-02 16:40:00    Totteridge         2     Mon     1041
## 45995 2009-11-05 16:40:00     Northwood         5     Thu     1024
## 45999 2009-11-05 16:40:00  Bounds Green         5     Thu     1028
## 46004 2009-11-01 16:40:00       Pimlico         1     Sun     1021
## 46006 2009-11-03 16:40:00 Bethnal Green         3     Tue     1027
##       ExitTime        ExitDateTime JourneyType DailyCapping FFare
## 45990    17:21 2009-11-02 17:21:00         TKT            N     0
## 45995    17:04 2009-11-05 17:04:00         TKT            N     0
## 45999    17:08 2009-11-05 17:08:00         TKT            N     0
## 46004    17:01 2009-11-01 17:01:00         PPY            N   160
## 46006    17:07 2009-11-03 17:07:00         TKT            N     0
```

##Data Fusion
### Location Data

For mapping purposes, I would like to fuse external data sets of the locations of the origin and destinations. Here, we extract spatial data of stations coordinates as well as the local authorities in which the stations are located.

On it...

###Distance Data

On it...
