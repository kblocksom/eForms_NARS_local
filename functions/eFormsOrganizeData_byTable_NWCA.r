# eFormsOrganizeData_byTable.r
# Purpose: For each type of data, organize into data frames
# First figure out the type of data by sample type
#
# Created 2/27/2019 by Karen Blocksom
###############################################################

eFormsOrganize_byTable.nwca <- function(rawData){
  # Extract visit info
  visitinfo <- as.data.frame(rawData[1:7],stringsAsFactors=F)
  # Extract sample type from 8th element in each file
  sampletype <- names(rawData)[8]
  
  # Create data frame of parsed data to start with, making them all character variables 
  parsedData <- as.data.frame(rawData[8])
  parsedData[,names(parsedData)] <- lapply(parsedData[,names(parsedData)], as.character)
  
  # run parsed data through organizing function, based on sample type 
  switch(sampletype,
         AA1 = {rr <- organizeAA.nwca(parsedData)},
         HYDRO = {rr <- organizeHydrology.nwca(parsedData)},
         STRESSORS = {rr <- organizeAApalt.nwca(parsedData)},
         BUFFER = {rr <- organizePALT.nwca(parsedData)},
         PTVER = {rr <- organizePointVer.nwca(parsedData)},
         SOIL = {rr <- organizeSoil.nwca(parsedData)},
         VEGPLOT = {rr <- organizeVegplot.nwca(parsedData)},
         SPECIES = {rr <- organizeV2.nwca(parsedData)}
         V3 = {rr <- organizeV3.nwca(parsedData)}
         SNAGS = {rr <- organizeV4.nwca(parsedData)}
         W1 = {rr <- organizeWater.nwca(parsedData)}
  )
  
  ss <- list(cbind(visitinfo, rr))
  # Add new object to list with sample type name
  ss[["SAMPLE_TYPE"]] <- sampletype
  return(ss)
}

#############################################################################################################
# This begins the section which organizes the parsed data by sample type

organizeAA.nwca <- function(parsedIn){
  # Simply melt these data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'ASSEESTAB'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('AA1\\.', '', PARAMETER)) 
  
  aa.long$SAMPLE_TYPE <- with(aa.long, ifelse(grepl('HGM_CLASS|SUBCLASS', PARAMETER), 
                                              'ASSECHAR', 'ASSEESTAB'))
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizeHydrology.nwca <- function(parsedIn){
  # Simply melt these data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'HYDRO'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('HYDRO\\.', '', PARAMETER)) 
  
  aa.long$SAMPLE_TYPE <- with(aa.long, ifelse(grepl('WEATHER|TIME|OUTFLOW|TIDAL_STAGE|PRESENT|RANK|COMMENTS',
                                                    PARAMETER), 'HYDRO', 'USACOE'))
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizePALT.nwca <- function(parsedIn){
  # DIRECTION AND PLOT, except for Comments and NONE_PRESENT variables, which are DIRECTION ONLY
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'COMMENT|BUFF_LAT|BUFF_LON')) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'BUFF_PALT'

    varLong <- names(aa)
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$LOCATION <- with(aa.long, substring(variable, 8, 9))
    aa.long$PLOT <- 0
    aa.long$PARAMETER <- with(aa.long, gsub('BUFFER\\.', '', variable))
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PLOT','LOCATION','PARAMETER','RESULT'))
  }  
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'COMMENT|BUFF_LAT|BUFF_LON')==FALSE)
  bb$SAMPLE_TYPE <- 'BUFF_PALT'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$variable <- with(bb.long, gsub('BUFFER\\.','',variable))
  bb.long$LOCATION <- with(bb.long, substring(variable, 1,1))
  bb.long$PLOT <- with(bb.long, substring(variable, 3,3))
  bb.long$PARAMETER <- with(bb.long, substring(variable, 5, nchar(variable)))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','PAGE','LINE','PARAMETER','RESULT'))
  
  cc <- rbind(aa.out, bb.out) 
  
  return(cc)
  }

organizeAApalt.nwca <- function(parsedIn){
  # Simply melt these data and clean up parameter names
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'AA_PALT'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('STRESSORS\\.', '', PARAMETER)) 
  
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}