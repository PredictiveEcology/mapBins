## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "mapBins",
  description = "",
  keywords = "",
  authors = authors = c(
       person("Isolde", "Lane-Shaw", role = "aut", email = "r.i.lane.shaw@gmail.com")),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.9", mapBins = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "mapBins.Rmd")), ## same file
  reqdPkgs = list("ggplot2", "data.table", "rgdal", "sf", "raster", "LandR",
                  "googledrive", "plotrix", "dplyr", "tidyverse", "reshape2"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer",
                 desc = "raster to match. default LCC2005.", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame",
                 desc = "study area polygon", sourceURL = NA),
    expectsInput(objectName = "birdList", objectClass = "list",
                 desc = "list of birds to create rasters for", sourceURL = NA),
    expectsInput(objectName = "birdPreds", objectClass = "list",
                 desc = "list output by postHocBinning module", sourceURL = NA),
    expectsInput(objectName = "forestClassRaster", objectClass = "RasterLayer",
                 desc = "forest class raster (e.g., derived from LandR Biomass)", sourceURL = NA),
    expectsInput(objectName = "ageRaster", objectClass = "RasterLayer",
                 desc = "raster of integer ages for forest areas", sourceURL = NA),
    expectsInput(objectName = "nonForClassRaster", objectClass = "RasterLayer",
                 desc = "raster of land cover classes (eg derived from LCC05)", sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "birdPredsRasters", objectClass = "list",
                  desc = "a list of rasters of the birdPreds mapped out")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.mapBins = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mapBins", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mapBins", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mapBins", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "mapBins", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "mapBins", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "mapBins", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
browser()
  #reclassify ageRaster into a raster of forest age classes, according to ageClassDefs in birdPreds
  birdPreds$ageClassDefs <- birdPreds$ageClassDefs[, ageClasses:=as.integer(ageClasses)] #change data type of ageClassDefs
  ageClassRaster <- ageRaster #make copy of ageRaster to be reclassified
  ageClassRaster <- reclassify(ageClassRaster, birdPreds$ageClassDefs) #do the reclassification based on ageClassDefs
  names(ageClassRaster) <- "ageClassRaster"
  sim$ageClassRaster #check over the raster that has been reclassified
  plot(sim$ageClassRaster)


  ###### MAKE BIRD PREDS RASTERS #######

  sim$birdPredsRasters <- lapply(X = birdList, FUN = function(bird){

      ##### MAKE RASTER OF 1D NON FOREST CLASSES #####

      raster1DBins <- nonForRaster
      birdTable <- eval(parse(text=paste("birdPreds$nonforBirdPreds$", bird, sep = "")))
      raster1DBins <- reclassify(raster1DBins, birdTable)

      #check it out visually
      print("raster1DBins written")
      raster1DBins
      plot(raster1DBins) #check it out visually

      ##### MAKE RASTER OF 2D FORESTED CLASSES #####

      # check that spatial extent is the same for ageClassraster and forClassraster
      extent(forClassRaster) == extent(ageClassRaster)

      #re-form matrix
      birdMatrix <- eval(parse(text=paste("birdPreds$birdMatricies$", bird, sep = "")))
      reclassTab2D <- melt(birdMatrix)
      colnames(reclassTab2D) <- c( "forClass","ageClass", "birdDensityPred")

      #reclassify Raster according to reclassTab2D, ageClassRaster and forClassRaster
      raster2DBins <- raster(forClassRaster); raster2DBins[] = NA #make an empty NA raster the same as forClassRaster

      #make dataframe of all the data in forClassRaster and ageClassRaster and give each cell/row a new definition column, birdDensityPred, from reclassTab2d
      f = data.frame(forClass=forClassRaster[], ageClass=ageClassRaster[])
      vec = c(1:nrow(f))
      f[,3] = vec
      m = merge(f, reclassTab2D, all.x=TRUE)
      colnames(m)[3] = "ord"
      m = m[order(m$ord),]

      #populate raster2DBins with the birdDensityPred row of the table m
      raster2DBins[] = m$birdDensityPred

      #check the new raster
      print("raster2DBins written")
      raster2DBins
      plot(raster2DBins)


      ##### COMBINE 1D AND 2D RASTERS INTO SINGLE MAP OF LANDSCAPE #####

      birdPredsRaster <- cover(x = raster2DBins,
                               y = raster1DBins,
                               filename = "birdPredsRaster",
                               overwrite = TRUE )

      names(birdPreds) <- c("birdpredsRaster")

      #visually check Raster
      print("birdPredsRaster written")
      birdPredsRaster
      plot(birdPredsRaster)

      return(birdPredsRaster)
    })

  names(sim$birdPredsRasters) <- birdList

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  #give default birdList
  if (!suppliedElsewhere("birdList")) {
    sim$birdList <- c("OVEN", "CAWA", "BAWW")
  }


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
