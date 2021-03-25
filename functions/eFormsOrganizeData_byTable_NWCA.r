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
         SPECIES = {rr <- organizeV2.nwca(parsedData)},
         V3 = {rr <- organizeV3.nwca(parsedData)},
         SNAGS = {rr <- organizeV4.nwca(parsedData)},
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
  aa.long$PARAMETER <- with(aa.long, sub('AA1\\.', '', PARAMETER)) 
  
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
  aa.long$PARAMETER <- with(aa.long, sub('HYDRO\\.', '', PARAMETER)) 
  
  aa.long$SAMPLE_TYPE <- with(aa.long, ifelse(grepl('WEATHER|TIME|OUTFLOW|TIDAL_STAGE|PRESENT|RANK|COMMENTS',
                                                    PARAMETER), 'HYDRO', 'USACOE'))
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizePALT.nwca <- function(parsedIn){
  # DIRECTION AND PLOT, except for Comments and NONE_PRESENT variables, which are DIRECTION ONLY
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'COMMENT|BUFF_LAT|BUFF_LON') & 
                 str_detect(names(parsedIn), '[:digit:]')==FALSE) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'BUFF_PALT'

    varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE'))]
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$LOCATION <- with(aa.long, substring(variable, 8, 9))
    aa.long$PLOT <- '0'
    aa.long$PARAMETER <- with(aa.long, sub('BUFFER\\.', '', variable))
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PLOT','LOCATION','PARAMETER','RESULT'))
  }  
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'COMMENT|BUFF_LAT|BUFF_LON')==FALSE &
                 str_detect(names(parsedIn), '[:digit:]'))
  bb$SAMPLE_TYPE <- 'BUFF_PALT'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$variable <- with(bb.long, sub('BUFFER\\.','',variable))
  bb.long$LOCATION <- with(bb.long, substring(variable, 1,1))
  bb.long$PLOT <- with(bb.long, substring(variable, 3,3))
  bb.long$PARAMETER <- with(bb.long, substring(variable, 5, nchar(variable)))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','PLOT','LOCATION','PARAMETER','RESULT'))
  
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
  aa.long$PARAMETER <- with(aa.long, sub('STRESSORS\\.', '', PARAMETER)) 
  
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
  
}

organizePointVer.nwca <- function(parsedIn){
  # Simply melt these data and clean up parameter names
  aa <- parsedIn
  
  aa$SAMPLE_TYPE <- 'PTVER'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, gsub('PTVER\\.', '', PARAMETER)) 
  
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)

}

organizeSoil.nwca <- function(parsedIn){
  # Separate data from horizons and those that are pit attributes
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'SOIL\\.[:digit:]')==FALSE) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'SOIL_PROF'
    aa$PAGE <- '1'
    aa$HORIZON <- '0'
    
    varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE','PAGE','HORIZON'))]
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE','PAGE','HORIZON'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$PARAMETER <- with(aa.long, sub('SOIL\\.', '', variable))
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PAGE','HORIZON','PARAMETER','RESULT'))
  }  
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'SOIL\\.[:digit:]'))
  bb$SAMPLE_TYPE <- 'SOIL_PROF'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$variable <- with(bb.long, gsub('SOIL\\.','',variable))
  bb.long$HORIZON <- with(bb.long, str_extract(variable, '[:digit:]+'))
  bb.long$PAGE <- '1'
  bb.long$PARAMETER <- with(bb.long, substring(variable, 3, nchar(variable)))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','PAGE','HORIZON','PARAMETER','RESULT'))
  
  cc <- rbind(aa.out, bb.out) 
  
  return(cc)
}

organizeVegplot.nwca <- function(parsedIn){
  # Separate plot data and other data
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'VEGPLOT'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  aa.long$variable <- with(aa.long, sub('VEGPLOT\\.', '', variable)) 
  
  aa.long$PLOT <- with(aa.long, ifelse(str_detect(variable, 'LON|LAT|WETLAND_TYPE'), 
                         substring(variable, nchar(variable), nchar(variable)), '0'))
  aa.long$PARAMETER <- with(aa.long, ifelse(PLOT=='0', variable, substring(variable, 1, nchar(variable)-2)))
  
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PLOT','PARAMETER','RESULT'))
  
  return(aa.out)
}

organizeV2.nwca <- function(parsedIn){
  # Separate data from different lines and plots
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'ACTUAL_DATE')) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'PLANT'
    aa$LINE <- '1'
    aa$PLOT <- '1'
    
    varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE','LINE','PLOT'))]
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$PARAMETER <- with(aa.long, sub('SPECIES\\.', '', variable))
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','LINE','PLOT','PARAMETER','RESULT'))
  }  
  # bb pulls out and formats species by line number and sample type
  bb <- subset(parsedIn, select=str_detect(names(parsedIn), 'SPECIES\\.[:digit:]'))
  bb$SAMPLE_TYPE <- 'PLANT'
  
  varLong <- names(bb)[names(bb)!='SAMPLE_TYPE']
  bb.long <- reshape(bb, idvar = 'SAMPLE_TYPE', varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'variable', direction = 'long')
  bb.long$variable <- with(bb.long, gsub('SPECIES\\.','',variable))
  bb.long$LINE <- with(bb.long, str_extract(variable, '[:digit:]+'))
  bb.long$variable <- with(bb.long, str_remove(variable, '[:digit:]\\_'))
  bb.long$PLOT <- with(bb.long, substring(variable, 1, 1))
  bb.long$PARAMETER <- with(bb.long, substring(variable, 3, nchar(variable)))
  
  bb.out <- subset(bb.long, select = c('SAMPLE_TYPE','LINE','PLOT','PARAMETER','RESULT'))
  
  cc.out <- rbind(bb.out, aa.out)
  
  cc.out.wide <- reshape(cc.out, idvar = c('SAMPLE_TYPE','LINE','PLOT'), direction = 'wide',
                         v.names = 'RESULT', timevar = 'PARAMETER')
  names(cc.out.wide) <- gsub("RESULT\\.", "", names(cc.out.wide))
  
  return(cc.out.wide)
  
}

organizeV3.nwca <- function(parsedIn){
  # Separate data from veg strata and ground cover
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), '[:digit:]')) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'VEGTYPE'

    varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE'))]
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$PLOT <- with(aa.long, substring(variable, 4, 4))
    aa.long$PARAMETER <- with(aa.long, substring(variable, 6, nchar(variable)))
    aa.long$SAMPLE_TYPE <- with(aa.long, str_detect(variable, "EXPOSED|TIME|DEPTH|LITTER|WD|WATER"),
                                'GROUND', 'VEGTYPE')
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PLOT','PARAMETER','RESULT'))
  }  
 
 return(aa.out)
  
}

organizeV4.nwca <- function(parsedIn){
  # Separate data that have lines and plot from those with just plots
  aa <- subset(parsedIn, select=str_detect(names(parsedIn), 'REVIEW')==FALSE) 
  if(ncol(aa)>0){
    aa$SAMPLE_TYPE <- 'TREE'
    
    varLong <- names(aa)[!(names(aa) %in% c('SAMPLE_TYPE'))]
    aa.long <- reshape(aa, idvar = c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                       v.names = 'RESULT', timevar = 'variable', direction = 'long')
    aa.long$variable <- with(aa.long, sub('SNAGS\\.', '', variable))
    aa.long$PLOT <- with(aa.long, substring(variable, 1, 1))
    aa.long$variable.1 <- with(aa.long, str_remove(variable, '[:digit:]+\\_'))
    
    aa.long$V2_LINE <- with(aa.long, ifelse(str_detect(variable.1,"[:digit:]"),
                                            str_extract(variable.1, "[:digit:]"), NA))
    
    aa.long$PARAMETER <- with(aa.long, sub("[:digit:]\\_", "", variable.1))
    print(head(aa.long))
    
    aa.out <- subset(aa.long, select = c('SAMPLE_TYPE','PLOT','V2_LINE','PARAMETER','RESULT'))
  }  
  
  return(aa.out)
  
}

organizeWater.nwca <- function(parsedIn){
  # Separate plot data and other data
  aa <- parsedIn
  aa$SAMPLE_TYPE <- 'ASSEWQ'
  
  varLong <- names(parsedIn)
  aa.long <- reshape(aa, idvar=c('SAMPLE_TYPE'), varying = varLong, times = varLong,
                     v.names = 'RESULT', timevar = 'PARAMETER', direction = 'long')
  aa.long$PARAMETER <- with(aa.long, sub('W1\\.', '', PARAMETER)) 
  
  aa.long <- subset(aa.long, str_detect(PARAMETER, 'REVIEW')==FALSE)
  
  aa.out <- base::subset(aa.long, select = c('SAMPLE_TYPE','PARAMETER','RESULT'))
  
  return(aa.out)
}