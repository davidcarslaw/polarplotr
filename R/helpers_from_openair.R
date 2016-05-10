## these are pre-defined type that need a field "date"; used by cutData
dateTypes <- c("year", "hour", "month", "season", "weekday", "weekend", "monthyear",
               "gmtbst", "bstgmt", "dst", "daylight")


# Check input file and prepare data
#
# Author: DCC
###############################################################################

checkPrep <- function(mydata, Names, type, remove.calm = TRUE, remove.neg = TRUE,
                      strip.white = TRUE, wd = "wd") {
  
  ## deal with conditioning variable if present, if user-defined, must exist in data
  ## pre-defined types
  ## existing conditioning variables that only depend on date (which is checked)
  conds <- c("default", "year", "hour", "month", "season", "weekday", 
             "weekend", "monthyear", "gmtbst", "bstgmt", "dst", "daylight")
  all.vars <- unique(c(names(mydata), conds))
  
  varNames <- c(Names, type) ## names we want to be there
  matching <- varNames %in% all.vars
  
  if (any(!matching)) {
    ## not all variables are present
    stop(cat("Can't find the variable(s)", varNames[!matching], "\n"))
  }
  
  ## add type to names if not in pre-defined list
  if (any(type %in% conds == FALSE)) {
    ids <- which(type %in% conds == FALSE)
    Names <- c(Names, type[ids])
  }
  
  ## if type already present in data frame
  if (any(type %in% names(mydata))) {
    ids <- which(type %in% names(mydata))
    Names <- unique(c(Names, type[ids]))
  }
  
  ## just select data needed
  mydata <- mydata[, Names]
  
  ## if site is in the data set, check none are missing
  ## seems to be a problem for some KCL data...
  if ("site" %in% names(mydata)) { ## split by site
    
    ## remove any NA sites
    if (anyNA(mydata$site)) {
      id <- which(is.na(mydata$site))
      mydata <- mydata[-id, ]
    }
  }
  
  
  ## sometimes ratios are considered which can results in infinite values
  ## make sure all infinite values are set to NA
  mydata[] <- lapply(mydata, function(x){replace(x, x == Inf | x == -Inf, NA)})
  
  if ("ws" %in% Names) {
    if ("ws" %in% Names & is.numeric(mydata$ws)) {
      
      ## check for negative wind speeds
      if (any(sign(mydata$ws[!is.na(mydata$ws)]) == -1)) {
        
        if (remove.neg) { ## remove negative ws only if TRUE
          warning("Wind speed <0; removing negative data")
          mydata$ws[mydata$ws < 0] <- NA
        }
      }
    }
  }
  
  ## round wd to make processing obvious
  ## data already rounded to nearest 10 degress will not be affected
  ## data not rounded will be rounded to nearest 10 degrees
  ## assumes 10 is average of 5-15 etc
  if (wd %in% Names) {
    if (wd %in% Names & is.numeric(mydata[, wd])) {
      
      ## check for wd <0 or > 360
      if (any(sign(mydata[[wd]][!is.na(mydata[[wd]])]) == -1 |
              mydata[[wd]][!is.na(mydata[[wd]])] > 360)) {
        
        warning("Wind direction < 0 or > 360; removing these data")
        mydata[[wd]][mydata[[wd]] < 0] <- NA
        mydata[[wd]][mydata[[wd]] > 360] <- NA
      }
      
      if (remove.calm) {
        if ("ws" %in% names(mydata)) {
          mydata[[wd]][mydata$ws == 0]  <- NA ## set wd to NA where there are calms
          mydata$ws[mydata$ws == 0] <- NA ## remove calm ws
        }
        mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
        
        ## round wd for use in functions - except windRose/pollutionRose
        mydata[[wd]] <- 10 * ceiling(mydata[[wd]] / 10 - 0.5)
        mydata[[wd]][mydata[[wd]] == 0] <- 360   # angles <5 should be in 360 bin
        
      }
      mydata[[wd]][mydata[[wd]] == 0] <- 360 ## set any legitimate wd to 360
    }
  }
  
  
  ## make sure date is ordered in time if present
  if ("date" %in% Names) {
    
    if ("POSIXlt" %in% class(mydata$date))
      stop ("date should be in POSIXct format not POSIXlt")
    
    ## if date in format dd/mm/yyyy hh:mm (basic check)
    if (length(grep("/", as.character(mydata$date[1]))) > 0) {
      
      mydata$date <- as.POSIXct(strptime(mydata$date, "%d/%m/%Y %H:%M"), "GMT")
      
    }
    
    ## try and work with a factor date - but probably a problem in original data
    if (is.factor(mydata$date))  {
      
      warning("date field is a factor, check date format")
      mydata$date <- as.POSIXct(mydata$date, "GMT")
      
    }
    
    mydata <- arrange(mydata, date)
    
    ## make sure date is the first field
    if (names(mydata)[1] != "date") 
      mydata <- mydata[c("date", setdiff(names(mydata), "date"))]
    
    ## check to see if there are any missing dates, stop if there are
    ids <- which(is.na(mydata$date))
    if (length(ids) > 0) {
      
      mydata <- mydata[-ids, ]
      warning(paste("Missing dates detected, removing",
                    length(ids), "lines"), call. = FALSE)
    }
    
    ## daylight saving time can cause terrible problems - best avoided!!
    
    if (any(dst(mydata$date))) {
      warning("Detected data with Daylight Saving Time, converting to UTC/GMT")
      mydata$date <- lubridate::force_tz(mydata$date, tzone = "GMT")
      
    }
    
  }
  
  
  
  
  if (strip.white) {
    ## set panel strip to white
    suppressWarnings(trellis.par.set(list(strip.background = list(col = "white"))))
  }
  
  
  ## return data frame
  return(mydata)
}


## gives names of lattice strips
strip.fun <- function(results.grid, type, auto.text) {
  ## proper names of labelling ###################################################
  pol.name <- sapply(levels(factor(results.grid[[type[1]]])),
                     function(x) quickText(x, auto.text))
  strip <- strip.custom(factor.levels = pol.name)
  
  if (length(type) == 1 ) {
    
    strip.left <- FALSE
    
  } else { ## two conditioning variables
    
    pol.name <- sapply(levels(factor(results.grid[[type[2]]])),
                       function(x) quickText(x, auto.text))
    strip.left <- strip.custom(factor.levels = pol.name)
  }
  if (length(type) == 1 & type[1] == "default") strip <- FALSE ## remove strip
  list(strip, strip.left, pol.name)
}



## makeOpenKeyLegend v0.1

##common code for making legend list
##objects for use with drawOpenkey outputs

##uses listUpdate in utilities

makeOpenKeyLegend <- function(key, default.key, fun.name = "function"){
  #handle logicals and lists
  if (is.logical(key)) {
    legend <- if (key) default.key else NULL
  } else if (is.list(key)) {
    legend <- listUpdate(default.key, key)
  } else {
    if(!is.null(key))
      warning(paste("In ", fun.name, "(...):\n unrecognised key not exported/applied\n",
                    " [see ?drawOpenKey for key structure/options]", sep = ""),
              call. = FALSE)
    legend <- NULL
  }
  
  #structure like legend for drawOpenKey
  if(!is.null(legend)){
    legend <- list(right = list(fun = drawOpenKey, args = list(key = legend),
                                draw =FALSE))
    if("space" %in% names(legend$right$args$key))
      names(legend)[[1]] <- legend$right$args$key$space
  }
  legend
}



## list update function
## for lattice type object structure and ... handling

## (currently used by)
## (all openair plots that include colorkey controlled by drawOpenKey)

##listUpdate function
#[in development]
listUpdate <- function(a, b, drop.dots = TRUE,
                       subset.a = NULL, subset.b = NULL){
  if(drop.dots){
    a <- a[names(a) != "..."]
    b <- b[names(b) != "..."]
  }
  if(!is.null(subset.a))
    a <- a[names(a) %in% subset.a]
  if(!is.null(subset.b))
    b <- b[names(b) %in% subset.b]
  if(length(names(b) > 0))
    a <- modifyList(a, b)
  a
}


##' Automatic text formatting for openair
##'
##' Workhorse function that automatically applies routine text formatting to
##' common expressions and data names used in openair.
##'

##'
##' \code{quickText} is routine formatting lookup table. It screens the
##' supplied character vector \code{text} and automatically applies formatting
##' to any recognised character sub-series. The function is used in a number of
##' \code{openair} functions and can also be used directly by users to format
##' text components of their own graphs (see below).
##'
##' @param text A character vector.
##' @param auto.text A logical option. The default, \code{TRUE}, applies
##'   \code{quickText} to \code{text} and returns the result. The alternative,
##'   \code{FALSE}, returns \code{text} unchanged. (A number of \code{openair}
##'   functions enable/unenable \code{quickText} using this option.
##' @export
##' @return The function returns an expression for graphical evaluation.
##' @author Karl Ropkins.
##' @keywords methods
##' @examples
##'
##'
##' #example 1
##' ##see axis formatting in an openair plot, e.g.:
##' scatterPlot(mydata, x = "no2", y = "pm10")
##'
##' #example 2
##' ##using quickText in other plots
##' plot(mydata$no2, mydata$pm10, xlab = quickText("my no2 label"),
##'      ylab = quickText("pm10 [ ug.m-3 ]"))
##'
##'
quickText <- function(text, auto.text = TRUE){
  
  ## the lookup table version
  
  ## #return if auto.text false
  if (!auto.text) return(ans <- text)
  
  ## #return if already expression
  if (is.expression(text)) return(ans <- text)
  
  ans <- paste("expression(paste('", text, " ", sep = "")
  ans <- gsub("NO2", "' 'NO' [2] * '", ans)
  ans <- gsub("no2", "' 'NO' [2] * '", ans)
  ans <- gsub("NOX", "' 'NO' [x] * '", ans)
  ans <- gsub("nox", "' 'NO' [x] * '", ans)
  ans <- gsub("NOx", "' 'NO' [x] * '", ans)
  ans <- gsub("NH3", "' 'NH' [3] * '", ans)
  ans <- gsub("nh3", "' 'NH' [3] * '", ans)
  ans <- gsub("co ", "' 'CO ' '", ans)
  ans <- gsub("co,", "' 'CO,' '", ans)
  ans <- gsub("nmhc", "' 'NMHC' '", ans)
  
  
  ans <- if (nchar(as.character(text)) == 2 && length(grep("ws", text)) > 0)
    gsub("ws", "' 'wind spd.' '", ans) else ans
  ans <- gsub("wd", "' 'wind dir.' '", ans)
  ans <- gsub("rh ", "' 'relative humidity' '", ans)
  ans <- gsub("PM10", "' 'PM' [10] * '", ans)
  ans <- gsub("pm10", "' 'PM' [10] * '", ans)
  ans <- gsub("pm1", "' 'PM' [1] * '", ans)
  ans <- gsub("PM1", "' 'PM' [1] * '", ans)
  ans <- gsub("PM4", "' 'PM' [4] * '", ans)
  ans <- gsub("pm4", "' 'PM' [4] * '", ans)
  ans <- gsub("PMtot", "' 'PM' [total] * '", ans)
  ans <- gsub("pmtot", "' 'PM' [total] * '", ans)
  ans <- gsub("pmc", "' 'PM' [coarse] * '", ans)
  
  ans <- gsub("pmcoarse", "' 'PM' [coarse] * '", ans)
  ans <- gsub("PMc", "' 'PM' [coarse] * '", ans)
  ans <- gsub("PMcoarse", "' 'PM' [coarse] * '", ans)
  ans <- gsub("pmf", "' 'PM' [fine] * '", ans)
  ans <- gsub("pmfine", "' 'PM' [fine] * '", ans)
  ans <- gsub("PMf", "' 'PM' [fine] * '", ans)
  ans <- gsub("PMfine", "' 'PM' [fine] * '", ans)
  ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("PM2.5", "' 'PM' [2.5] * '", ans)
  ans <- gsub("PM25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("pm25", "' 'PM' [2.5] * '", ans)
  ans <- gsub("O3", "' 'O' [3] * '", ans)
  ans <- gsub("o3", "' 'O' [3] * '", ans)
  ans <- gsub("ozone", "' 'O' [3] * '", ans)
  ans <- gsub("CO2", "' 'CO' [2] * '", ans)
  ans <- gsub("co2", "' 'CO' [2] * '", ans)
  ans <- gsub("SO2", "' 'SO' [2] * '", ans)
  ans <- gsub("so2", "' 'SO' [2] * '", ans)
  ans <- gsub("H2S", "' 'H' [2] * 'S''", ans)
  ans <- gsub("h2s", "' 'H' [2] * 'S''", ans)
  ans <- gsub("CH4", "' 'CH' [4] * '", ans)
  ans <- gsub("ch4", "' 'CH' [4] * '", ans)
  ans <- gsub("dgrC", "' * degree * 'C' '", ans)
  ans <- gsub("degreeC", "' * degree * 'C' '", ans)
  ans <- gsub("deg. C", "' * degree * 'C' '", ans)
  ans <- gsub("degreesC", "' * degree * 'C' '", ans)
  ans <- gsub("degrees", "' * degree *'", ans)
  ans <- gsub("Delta", "' * Delta *'", ans)
  ans <- gsub("delta", "' * Delta *'", ans)
  ans <- gsub("ug/m3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ug.m-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ug m-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("ugm-3", "' * mu * 'g m' ^-3 *'", ans)
  ans <- gsub("mg/m3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mg.m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mg m-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("mgm-3", "' * 'm' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng/m3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng.m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ng m-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("ngm-3", "' * 'n' * 'g m' ^-3 *'", ans)
  ans <- gsub("m/s2", "' 'm s' ^-2 *'", ans)
  ans <- gsub("m/s", "' 'm s' ^-1 *'", ans)
  ans <- gsub("m.s-1", "' 'm s' ^-1 *'", ans)
  ans <- gsub("m s-1", "' 'm s' ^-1 *'", ans)
  ans <- gsub("g/km", "' 'g km' ^-1 *'", ans)
  ans <- gsub("g/s", "' 'g s' ^-1 *'", ans)
  ans <- gsub("km/hr/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/hour/s", "' 'km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/h/s", "km hr' ^-1 * ' s' ^-1 *'", ans)
  ans <- gsub("km/hr", "' 'km hr' ^-1 *'", ans)
  ans <- gsub("km/h", "' 'km hr' ^-1 *'", ans)
  ans <- gsub("km/hour", "' 'km hr' ^-1 *'", ans)
  
  ans <- gsub("r2", "R' ^2 *'", ans)
  ans <- gsub("R2", "R' ^2 *'", ans)
  
  ans <- gsub("tau ", "' * tau * '", ans)
  
  ans <- gsub("umol/m2/s", "' * mu * 'mol m' ^-2 * ' s' ^-1 *'", ans)
  ans <- gsub("umol/m2", "' * mu * 'mol m' ^-2 *'", ans)
  
  ans <- paste(ans, "'))", sep = "")
  
  ## commands to strip unecessary * etc...
  
  if (substr(ans, (nchar(ans) - 8), (nchar(ans) - 6)) == "] *") {
    a <- ans
    ans <- paste(substr(a, 1, (nchar(a) - 7)),
                 substr(a, (nchar(a) - 5), nchar(a)), sep = "")
  }
  
  ans <- gsub("''", "", ans)
  ans <- gsub("' '", "", ans)
  ans <- gsub("\\*  \\*", "~", ans)
  ans <- gsub("^expression\\(paste\\( \\*", "expression(paste(", ans)
  ans <- gsub("^expression\\(paste\\(\\*", "expression(paste(", ans)
  
  if (substr(ans, (nchar(ans) - 2), (nchar(ans) - 2)) == "*") {
    a <- ans
    ans <- paste(substr(a, 1, (nchar(a) - 2)), " ' ' ",
                 substr(a, (nchar(a) - 1), nchar(a)), sep = "")
  }
  
  
  ## ###################
  ## new bit
  ## replace a \n b with atop(a,b)
  ## one newline only
  
  if (grepl("\n", ans)) {
    a <- ans
    ans <- paste(substr(a, 1, 17), "atop(", substr(a, 18, nchar(a)), sep = "")
    ans <- gsub("\n", "' , '", ans)
    temp <- paste(")", sep = "", collapse = "")
    ans <- paste(ans, temp, sep="")
  }
  
  ## ########################
  
  
  if (inherits(try(eval(parse(text = ans)), TRUE), "try-error") ==
      FALSE) {
    ans <- eval(parse(text = ans))
  }
  else {
    ans <- text
  }
}


##' Function to split data in different ways for conditioning
##' 
##' Utility function to split data frames up in various ways for
##' conditioning plots. Users would generally not be expected to call
##' this function directly.  Widely used by many \code{openair}
##' functions usually through the option \code{type}.
##' 
##' This section give a brief description of each of the define levels
##' of \code{type}. Note that all time dependent types require a
##' column \code{date}.
##' 
##' "default" does not split the data but will describe the levels as
##' a date range in the format "day month year".
##'
##' "year" splits the data by each year.
##'
##' "month" splits the data by month of the year.
##'
##' "hour" splits the data by hour of the day.
##' 
##' "monthyear" splits the data by year and month. It differs from
##' month in that a level is defined for each month of the data set.
##' This is useful sometimes to show an ordered sequence of months if
##' the data set starts half way through a year; rather than starting
##' in January.
##' 
##' "weekend" splits the data by weekday and weekend.
##' 
##' "weekday" splits the data by day of the week - ordered to start
##' Monday.
##' 
##' "season" splits data up by season. In the northern hemisphere
##' winter = December, January, February; spring = March, April, May
##' etc. These defintions will change of \code{hemisphere =
##' "southern"}.
##'
##' "daylight" splits the data relative to estimated sunrise and
##' sunset to give either daylight or nighttime. The cut is made by
##' \code{cutDaylight} but more conveniently accessed via
##' \code{cutData}, e.g. \code{cutData(mydata, type = "daylight",
##' latitude = my.latitude, longitude = my.longitude)}. The daylight
##' estimation, which is valid for dates between 1901 and 2099, is 
##' made using the measurement location, date, time and astronomical
##' algorithms to estimate the relative positions of the Sun and the
##' measurement location on the Earth's surface, and is based on NOAA
##' methods. 
##' Measurement location should be
##' set using \code{latitude} (+ to North; - to South) and
##' \code{longitude} (+ to East; - to West).
##' 
##' "dst" will split the data by hours that are in daylight saving
##' time (DST) and hours that are not for appropriate time zones. The
##' option "dst" also requires that the local time zone is given
##' e.g. \code{local.tz = "Europe/London"}, \code{local.tz =
##' "America/New_York"}. Each of the two periods will be in
##' \emph{local time}. The main purpose of this option is to test
##' whether there is a shift in the diurnal profile when DST and
##' non-DST hours are compared. This option is particularly useful
##' with the \code{timeVariation} function. For example, close to the
##' source of road vehicle emissions, `rush-hour' will tend to occur
##' at the same \emph{local time} throughout the year e.g. 8 am and 5
##' pm. Therefore, comparing non-DST hours with DST hours will tend to
##' show similar diurnal patterns (at least in the timing of the
##' peaks, if not magnitude) when expressed in local time. By
##' contrast a variable such as wind speed or temperature should show
##' a clear shift when expressed in local time. In essence, this
##' option when used with \code{timeVariation} may help determine
##' whether the variation in a pollutant is driven by man-made
##' emissions or natural processes.
##'
##' "wd" splits the data by 8 wind sectors and requires a column
##' \code{wd}: "NE", "E", "SE", "S", "SW", "W", "NW", "N".
##' 
##' "ws" splits the data by 8 quantiles of wind speed and requires a
##' column \code{ws}.
##' 
##' "site" splits the data by site and therefore requires a column
##' \code{site}.
##' 
##' Note that all the date-based types e.g. month/year are derived 
##' from a column \code{date}. If a user already has a column with a 
##' name of one of the date-based types it will not be used.
##' 
##' @param x A data frame containing a field \code{date}.
##' @param type A string giving the way in which the data frame should
##'   be split. Pre-defined values are: \dQuote{default}, 
##'   \dQuote{year}, \dQuote{hour}, \dQuote{month}, \dQuote{season}, 
##'   \dQuote{weekday}, \dQuote{site}, \dQuote{weekend}, 
##'   \dQuote{monthyear}, \dQuote{daylight}, \dQuote{dst} (daylight 
##'   saving time).
##'   
##'   \code{type} can also be the name of a numeric or factor. If a 
##'   numeric column name is supplied \code{cutData} will split the 
##'   data into four quantiles. Factors levels will be used to split 
##'   the data without any adjustment.
##' @param hemisphere Can be \code{"northern"} or \code{"southern"}, 
##'   used to split data into seasons.
##' @param n.levels Number of quantiles to split numeric data into.
##' @param start.day What day of the week should the \code{type = 
##'   "weekday"} start on?  The user can change the start day by 
##'   supplying an integer between 0 and 6. Sunday = 0, Monday = 1, 
##'   \ldots For example to start the weekday plots on a Saturday, 
##'   choose \code{start.day = 6}.
##' @param is.axis A logical (\code{TRUE}/\code{FALSE}), used to 
##'   request shortened cut labels for axes.
##' @param local.tz Used for identifying whether a date has daylight 
##'   savings time (DST) applied or not. Examples include 
##'   \code{local.tz = "Europe/London"}, \code{local.tz = 
##'   "America/New_York"} i.e. time zones that assume DST. 
##'   \url{http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones} 
##'   shows time zones that should be valid for most systems. It is 
##'   important that the original data are in GMT (UTC) or a fixed 
##'   offset from GMT. See \code{import} and the openair manual for 
##'   information on how to import data and ensure no DST is applied.
##' @param latitude The decimal latitude used in \code{type =
##'   "daylight"}.
##' @param longitude The decimal longitude. Note that locations west
##'   of Greenwich are negative.
##' @param ... All additional parameters are passed on to next 
##'   function(s).
##' @export
##' @return Returns a data frame with a column \code{cond} that is 
##'   defined by \code{type}.
##' @author David Carslaw (cutData) and Karl Ropkins (cutDaylight)
##' @keywords methods
##' @examples
##'
##' ## split data by day of the week
##' mydata <- cutData(mydata, type = "weekday")
##'
##'
cutData <- function(x, type = "default", hemisphere = "northern", 
                    n.levels = 4, start.day = 1, is.axis = FALSE, 
                    local.tz = NULL, latitude = 51, longitude = -0.5,
                    ...) {
  
  ## function to cutData depending on choice of variable
  ## pre-defined types and user-defined types
  ## If another added, then amend checkPrep
  
  ## note: is.axis modifies factor levels to give shorter labels for axis
  ##       generic label shortening handled at end of section
  ##       format(date, "%?") outputs modified by is.axis are set using temp
  ##       declared at at start of associated type section - karl
  
  
  makeCond <- function(x, type = "default") {
    
    ## if type is time based and already exists in data, 
    ## just return data
    
    if (type %in% dateTypes & type %in% names(x)) {
      message(paste0("\nUsing ", "'", type, "'", " in data frame and not date-based openair version. \nThis may result in different behaviour compared with openair calculations."))
      return(x)
    }
    
    conds <- c("default", "year", "hour", "month", "season", 
               "weekday", "wd", "site", "weekend", "monthyear", 
               "bstgmt", "gmtbst", "dst", "daylight")
    
    ## if conditioning type already built in, is present in data frame and is a factor
    if (type %in% conds & type %in% names(x)) {
      
      if (is.factor(x[[type]])) {
        
        x[[type]] <- factor(x[[type]])  ## remove unused factor levels
        return(x)
      }
    }
    
    if (type %in% conds == FALSE) { ## generic, user-defined
      ## split by quantiles unless it is a factor, in which case keep as is
      ## number of quantiles set by n.levels
      
      if (is.factor(x[[type]]) | is.character(x[[type]]) | class(x[[type]])[1] == "Date" |
          "POSIXt" %in% class(x[[type]])) {
        
        ## drop unused levels while we are at it
        x[[type]] <- factor(x[[type]])
        
      } else {
        
        temp.levels <- 
          levels(cut(x[[type]], unique(quantile(x[[type]],
                                                probs = seq(0, 1, length =
                                                              n.levels + 1),
                                                na.rm = TRUE)),
                     include.lowest = TRUE))
        
        x[[type]] <- cut(x[[type]], 
                         unique(quantile(x[[type]], 
                                         probs = seq(0, 1, length = n.levels + 1),
                                         na.rm = TRUE)), include.lowest = TRUE,
                         labels = FALSE)
        
        x[[type]] <- as.factor(x[[type]])
        temp.levels <- gsub("[(]|[)]|[[]|[]]", "", temp.levels)
        temp.levels <- gsub("[,]", " to ", temp.levels)
        levels(x[[type]]) <- if(is.axis) temp.levels else paste(type, temp.levels)
      }
      
    }
    
    if (type == "default") {
      ## shows dates (if available)
      ## not always available e.g. scatterPlot
      if ("date" %in% names(x)) {
        
        x[[type]] <- factor(paste(format(min(x$date), "%d %B %Y"), " to ",
                                  format(max(x$date), "%d %B %Y"), sep = ""))
        ## order the data by date
        x <- arrange(x, date)
        
      } else {
        x[[type]] <- factor("all data")
      }
      
    }
    
    if (type == "year") x[[type]] <- factor(format(x$date, "%Y"))
    
    if (type == "hour") x[[type]] <- factor(format(x$date, "%H"))
    
    if (type == "month") {
      ## need to generate month abbrevs on the fly for different languages
      temp <- if (is.axis) "%b" else "%B"
      x[[type]] <- format(x$date, temp)
      
      ## month names
      month.abbs <- format(seq(as.Date("2000-01-01"), 
                               as.Date("2000-12-31"), "month"), temp)
      
      ## might only be partial year...
      ids <- which(month.abbs %in% unique(x$month))
      the.months <- month.abbs[ids]
      
      x[[type]] <- ordered(x[[type]], levels = the.months)
    }
    
    if (type == "monthyear") {
      x[[type]] <- format(x$date, "%B %Y")
      x[[type]] <- ordered(x[[type]], levels = unique(x[[type]]))
    }
    
    if (type == "season") {
      
      if (!hemisphere %in% c("northern", "southern")) {
        stop("hemisphere must be 'northern' or 'southern'")}
      
      if (hemisphere == "northern") {
        x[[type]] <- "winter (DJF)" ## define all as winter first, then assign others
        ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
        x[[type]][ids] <- "spring (MAM)"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
        x[[type]][ids] <- "summer (JJA)"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
        x[[type]][ids] <- "autumn (SON)"
        
        seasons <- c("spring (MAM)", "summer (JJA)", "autumn (SON)", 
                     "winter (DJF)")
        
        ## might only be partial year...
        ids <- which(seasons %in% unique(x$season))
        the.season <- seasons[ids]
        x[[type]] <- ordered(x[[type]], levels = the.season)
      }
      if (hemisphere == "southern") {
        
        x[[type]] <- "summer (DJF)" ## define all as winter first, then assign others
        ids <- which(as.numeric(format(x$date, "%m")) %in% 3:5)
        x[[type]][ids] <- "autumn (MAM)"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 6:8)
        x[[type]][ids] <- "winter (JJA)"
        ids <- which(as.numeric(format(x$date, "%m")) %in% 9:11)
        x[[type]][ids] <- "spring (SON)"
        
        seasons <- c("spring (SON)", "summer (DJF)", "autumn (MAM)", 
                     "winter (JJA)")
        
        ## might only be partial year...
        ids <- which(seasons %in% unique(x$season))
        the.season <- seasons[ids]
        x[[type]] <- ordered(x[[type]], 
                             levels = c("spring (SON)", "summer (DJF)",
                                        "autumn (MAM)", "winter (JJA)"))
      }
    }
    
    if (type == "weekend") {
      ## split by weekend/weekday
      weekday <- selectByDate(x, day = "weekday")
      weekday[[type]] <- "weekday"
      weekend <- selectByDate(x, day = "weekend")
      weekend[[type]] <- "weekend"
      
      x <- rbind(weekday, weekend)
      x[[type]] <- ordered(x[[type]], levels = c("weekday", "weekend"))
      
    }
    
    if (type == "weekday") {
      
      x[[type]] <- format(x$date, "%A")
      # weekday.names <-  format(ISOdate(2000, 1, 3:9), "%A")
      weekday.names <- format(ISOdate(2000, 1, 2:8), "%A")
      
      if (start.day < 0 || start.day > 6) 
        stop("start.day must be between 0 and 6.")
      
      day.ord <- c(weekday.names[(1 + start.day):7], 
                   weekday.names[1:(1 + start.day - 1)])
      
      ## might only be certain days available...
      ids <- which(weekday.names %in% unique(x$weekday))
      # the.days <- weekday.names[ids]
      the.days <- day.ord[ids]
      
      ## just use sequence of days given if <7, if not order them
      if (length(unique(x$weekday)) < 7) {
        x[[type]] <- ordered(x[[type]], levels = factor(unique(x$weekday)))
      } else {
        x[[type]] <- ordered(x[[type]], levels = the.days)
      }
    }
    
    if (type == "wd") {
      
      ## could be missing data
      id <- which(is.na(x$wd))
      if (length(id) > 0) {
        x <- x[-id, ]
        warning(paste(length(id),
                      "missing wind direction line(s) removed"), call. = FALSE)
      }
      
      x[[type]] <- cut(x$wd, breaks = seq(22.5, 382.5, 45),
                       labels = c("NE", "E", "SE", "S", "SW", "W",
                                  "NW", "N"))
      x[[type]][is.na(x[[type]])] <- "N" # for wd < 22.5
      
      x[[type]] <- ordered(x[[type]], 
                           levels = c("N", "NE", "E",
                                      "SE", "S", "SW", "W", "NW"))}
    
    
    if (type == "site") {
      x[[type]] <- x$site
      x[[type]] <- factor(x[[type]]) ## will get rid of any unused factor levels
    }
    
    if (type %in% c("dst", "bstgmt", "gmtbst")) {
      type <- "dst" ## keep it simple
      
      ## how to extract BST/GMT
      if (is.null(local.tz)) {
        message("missing time zone, assuming Europe/London")
        local.tz <- "Europe/London"
      }
      
      attr(x$date, "tzone") <- local.tz
      
      id.nondst <- which(as.POSIXlt(x$date)$isdst == 0)
      id.dst <- which(as.POSIXlt(x$date)$isdst == 1)
      
      if (any(as.POSIXlt(x$date)$isdst == -1)) 
        stop("Not possible to identify DST")
      
      x[id.nondst, type] <- "Non-DST"
      x[id.dst, type] <- "DST"
      x[, type] <- factor(x[, type])
      
    }
    
    if (type == "daylight") {
      x <- cutDaylight(x, latitude, longitude, ...)
    }
    
    x
  }
  
  for (i in 1:length(type)) {
    x <- makeCond(x, type[i])
  }
  x
  
}

###########################################################################################
#cutDaylight function
cutDaylight <- function(x, latitude = 51.522393, longitude = -0.154700, ...){
  
  ##long, hour.off
  
  #condition openair data by daylight
  #using date (POSIXt)
  #kr v 0.2
  #################################
  #based on noaa methods
  #http://www.srrb.noaa.gov/highlights/sunrise/calcdetails.html
  #by Chris Cornwall, Aaron Horiuchi and Chris Lehman
  #
  
  ######################
  #notes
  ######################
  #calculations use
  #(lat, long) position relative to sun
  #to estimate if daylight or nighttime hour
  ######################
  #solar.noon.lst, etc are factions of day
  #seconds into that day = p.time * 86400
  #so for example sunset time is
  #as.POSIXct(sunset.time.lst * 86400, origin = format(x$date, "%Y-%m-%d"))
  #(assuming you do not run into next day!)
  ######################
  #currently unsure about extremes
  #long nights and days at poles need checking
  #
  
  ##################
  #suggestions:
  ##################
  #local hour offset could be a lookup table linked to tz
  #
  
  if(!"POSIXt" %in% class(x$date))
    stop("required field 'date' missing or not POSIXt\n", call. = FALSE)
  
  # local hour offset
  
  local.hour.offset <- as.numeric(lubridate::force_tz(x$date[1], "UTC") - x$date[1])
  
  ###################
  #temp functions
  ###################
  rad <- function(x) x * pi / 180
  degrees <- function(x) x * (180 / pi)
  
  ###############
  #get local time
  ###############
  temp <- x$date
  
  #################
  #make julian.refs
  #################
  #ref Gregorian calendar back extrapolated.
  #assumed good for years between 1800 and 2100
  
  p.day <- (as.numeric(format(temp, "%H")) * 3600) +
    (as.numeric(format(temp, "%M")) * 60) +
    as.numeric(format(temp, "%S"))
  p.day <- p.day / 86400
  
  #julian century (via julian day)
  julian.century <-
    as.numeric(as.Date(temp, format = "%m/%d/%Y")) + 2440587.5 +
    p.day - (local.hour.offset / 24)
  julian.century <- (julian.century - 2451545) / 36525
  
  ##################
  #main calcs
  ##################
  #as of noaa
  
  geom.mean.long.sun.deg <-
    (280.46646 + julian.century * (36000.76983 + julian.century * 0.0003032)) %% 360
  
  geom.mean.anom.sun.deg <-
    357.52911 + julian.century * (35999.05029 - 0.0001537 * julian.century)
  
  eccent.earth.orbit <-
    0.016708634 - julian.century * (0.000042037 + 0.0001537 * julian.century)
  
  sun.eq.of.ctr <- sin(rad(geom.mean.anom.sun.deg)) *
    (1.914602 - julian.century * (0.004817 + 0.000014 * julian.century)) +
    sin(rad(2 * geom.mean.anom.sun.deg)) *
    (0.019993 - 0.000101 * julian.century) +
    sin(rad(3 * geom.mean.anom.sun.deg)) * 0.000289
  
  sun.true.long.deg <- sun.eq.of.ctr + geom.mean.long.sun.deg
  
  sun.app.long.deg <- sun.true.long.deg - 0.00569 - 0.00478 *
    sin(rad(125.04 - 1934.136 * julian.century))
  
  mean.obliq.ecliptic.deg <- 23 + (26 + ((
    21.448 - julian.century *
      (46.815 + julian.century *
         (0.00059 - julian.century
          * 0.001813))
  )) / 60) / 60
  
  obliq.corr.deg <- mean.obliq.ecliptic.deg +
    0.00256 * cos(rad(125.04 - 1934.136 * julian.century))
  
  sun.declin.deg <- degrees(asin(sin(rad(obliq.corr.deg)) *
                                   sin(rad(sun.app.long.deg))))
  
  vary <- tan(rad(obliq.corr.deg / 2)) * tan(rad(obliq.corr.deg / 2))
  
  eq.of.time.minutes <-
    4 * degrees(
      vary * sin(2 * rad(geom.mean.long.sun.deg)) -
        2 * eccent.earth.orbit * sin(rad(geom.mean.anom.sun.deg)) +
        4 * eccent.earth.orbit * vary * sin(rad(geom.mean.anom.sun.deg)) *
        cos(2 * rad(geom.mean.long.sun.deg)) - 0.5 * vary * vary *
        sin(4 * rad(geom.mean.long.sun.deg)) - 1.25 * eccent.earth.orbit *
        eccent.earth.orbit * sin(2 * rad(geom.mean.anom.sun.deg))
    )
  
  #original nooa code
  ##
  #ha.sunrise.deg <- degrees(acos(cos(rad(90.833)) /
  #                  (cos(rad(latitude)) * cos(rad(sun.declin.deg))) -
  #                  tan(rad(latitude)) * tan(rad(sun.declin.deg))))
  ##
  #R error catcher added
  #for long nights>24hours/short nights<0
  
  ha.sunrise.deg <- cos(rad(90.833)) /
    (cos(rad(latitude)) * cos(rad(sun.declin.deg))) -
    tan(rad(latitude)) * tan(rad(sun.declin.deg))
  ha.sunrise.deg <- ifelse(ha.sunrise.deg > 1, 1, ha.sunrise.deg)
  ha.sunrise.deg <- ifelse(ha.sunrise.deg < -1,-1, ha.sunrise.deg)
  ha.sunrise.deg <- degrees(acos(ha.sunrise.deg))
  
  solar.noon.lst <-
    (720 - 4 * longitude - eq.of.time.minutes + local.hour.offset * 60) / 1440
  
  sunrise.time.lst <- solar.noon.lst - ha.sunrise.deg * 4 / 1440
  
  sunset.time.lst <- solar.noon.lst + ha.sunrise.deg * 4 / 1440
  
  sunlight.duration.minutes <- 8 * ha.sunrise.deg
  
  #################################
  #daylight factor
  #################################
  #need to confirm dusk/dawn handing
  
  daylight <- ifelse(
    sunlight.duration.minutes == 0,
    FALSE,
    ifelse(
      sunlight.duration.minutes == 1440,
      TRUE,
      ifelse(
        sunrise.time.lst < sunset.time.lst,
        ifelse(p.day < sunset.time.lst &
                 p.day > sunrise.time.lst, TRUE, FALSE),
        ifelse(p.day <= sunrise.time.lst &
                 p.day >= sunset.time.lst, FALSE, TRUE)
      )
    )
  )
  #as ordered factor
  daylight <-
    factor(
      daylight,
      levels = c(TRUE, FALSE),
      labels = c("daylight", "nighttime")
    )
  
  ###############################
  #output
  ###############################
  x <- cbind(x, daylight = daylight)
  
}


##' openair colours
##'
##' Pre-defined openair colours and definition of user-defined colours
##'
##' This in primarily an internal openair function to make it easy for users to
##' select particular colour schemes, or define their own range of colours of a
##' user-defined length.
##'
##' Each of the pre-defined schemes have merits and their use will
##' depend on a particular situation. For showing incrementing
##' concentrations e.g. high concentrations emphasised, then
##' "default", "heat", "jet" and "increment" are very useful. See also
##' the description of \code{RColorBrewer} schemes for the option
##' \code{scheme}.
##'
##' To colour-code categorical-type problems e.g. colours for different
##' pollutants, "hue" and "brewer1" are useful.
##'
##' When publishing in black and white, "greyscale" is often convenient.  With
##' most openair functions, as well as generating a greyscale colour gradient,
##' it also resets strip background and other coloured text and lines to
##' greyscale values.
##'
##' Failing that, the user can define their own schemes based on R colour
##' names. To see the full list of names, type \code{colors()} into R.
##'
##' @param scheme The pre-defined schemes are "increment", "default",
##' "brewer1", "heat", "jet", "hue", "greyscale", or a vector of R
##' colour names e.g. c("green", "blue"). It is also possible to
##' supply colour schemes from the \code{RColorBrewer} package. This
##' package defines three types of colour schemes: sequential,
##' diverging or qualitative. See \url{http://colorbrewer2.org} for
##' more details concerning the orginal work on which this is based.
##'
##' Sequential colours are useful for ordered data where there is a
##' need to show a difference between low and high values with colours
##' going from light to dark. The pre-defined colours that can be
##' supplied are: "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys",
##' "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu",
##' "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd".
##'
##'  Diverging palettes put equal emphasis on mid-range critical
##' values and extremes at both ends of the data range. Pre-defined
##' values are: "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
##' "RdYlBu", "RdYlGn", "Spectral".
##'
##' Qualitative palettes are useful for differentiating between
##' categorical data types. The pre-defined schemes are "Accent",
##' "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3".
##'
##' Note that because of the way these schemes have been developed
##' they only exist over certain number of colour gradations
##' (typically 3--10) --- see ?\code{brewer.pal} for actual
##' details. If less than or more than the required number of colours
##' is supplied then \code{openair} will interpolate the colours.
##' @param n number of colours required.
##' @export
##' @import RColorBrewer
##' @return Returns colour values - see examples below.
##' @author David Carslaw
##' @references \url{http://colorbrewer2.org}
##' @keywords methods
##' @examples
##'
##' # to return 5 colours from the "jet" scheme:
##' cols <- openColours("jet", 5)
##' cols
##'
##' # to interpolate between named colours e.g. 10 colours from yellow to
##' #  green to red:
##' cols <- openColours(c("yellow", "green", "red"), 10)
##' cols
##'
##'
openColours <- function(scheme = "default", n = 100) {
  
  ## pre-defined brewer colour palletes sequential, diverging, qualitative
  brewer.col <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu",
                  "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
                  "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral",
                  "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
  ## max colours allowed
  
  brewer.n <- c(rep(9, 18), rep(9, 9), c(8, 8, 12, 9, 8, 9, 8, 12))
  
  ## predefined schemes
  schemes <- c("increment", "default", "brewer1", "heat", "jet", "hue", "greyscale", brewer.col)
  
  ## schemes
  heat <- colorRampPalette(brewer.pal(9, "YlOrRd"), interpolate = "spline")
  
  jet <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                            "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  
  default.col <- colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")
  
  
  
  ## for this pallete use specfified number if possible - because it has been thought about...
  brewer1 <- function (n) {
    if (n >= 3 & n <= 9) {
      
      brewer.pal(n, "Set1")
      
    } else {
      
      thefun <- suppressWarnings(colorRampPalette(brewer.pal(9, "Set1"), interpolate = "spline"))
      thefun(n)
    }
    
  }
  
  ## for this pallete use specfified number if possible - because it has been thought about...
  find.brewer <- function (thecol, n) {
    
    n.brew <- brewer.n[scheme == brewer.col]
    
    if (n >= 3 & n <= n.brew) {
      
      brewer.pal(n, thecol)
      
    } else {
      
      thefun <- suppressWarnings(colorRampPalette(brewer.pal(n.brew, thecol), interpolate = "spline"))
      thefun(n)
    }
    
  }
  
  increment <- colorRampPalette(c("#B0FFF1", "#9CFFC7", "#87FF8E", "#A0FF73",
                                  "#B4FF69", "#CCFF60", "#E7FF56", "#FFF84D", "#FFCB46", "#FF9C40",
                                  "#FF6939", "#FF3333", "#CC1B62", "#990A7C", "#520066"))
  
  h = c(0, 360) + 15
  l = 65
  c = 100
  
  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }
  
  hue <- grDevices::hcl(
    h = seq(h[1], h[2], length = n),
    c = c,
    l = l)
  
  greyscale <- grey(seq(0.9, 0.1, length=n))
  
  ## error catcher
  if (length(scheme) == 1){
    if (scheme %in% brewer.col) cols <- find.brewer(scheme, n)
    if (scheme == "increment") cols <- increment(n)
    if (scheme == "default") cols <- rev(default.col(n))
    if (scheme == "brewer1") cols <- brewer1(n)
    if (scheme %in% brewer.col) cols <- find.brewer(scheme, n)
    if (scheme == "heat") cols <- heat(n)
    if (scheme == "jet") cols <- jet(n)
    if (scheme == "hue") cols <- hue
    if (scheme == "greyscale") cols <- greyscale
  }
  
  if (!any(scheme %in% schemes)) { #assume user has given own colours
    if (length(scheme) > 1) {  ## interpolate
      user.cols  <- colorRampPalette(scheme)
      cols =  user.cols(n)
    } else {
      cols <- rep(scheme, n)
    }
  }
  
  cols
}


########################################
#drawOpenKey v0.2
########################################
#drawOpenKey is a modification of:
#draw.colorkey
#
#original code from lattice, reference:
#Deepayan Sarkar (2010). lattice: Lattice Graphics.
#R package version 0.18-5.
#http://r-forge.r-project.org/projects/lattice/
#
#additional code by Karl Ropkins, allows:
#some crude header and footer labelling
#text formatting by openair::quickText
#addition plot style and layout control
########################################

########################################
#The help, advice and extreme patience
#of Deepayan Sarkar are also gratefully
#acknowledged
########################################



##' Scale key handling for openair
##'
##' General function for producing scale keys for other openair functions.  The
##' function is a crude modification of the draw.colorkey function developed by
##' Deepayan Sarkar as part of the lattice package, and allows additional key
##' labelling to added, and provides some additional control of the appearance
##' and scaling.
##'
##' The \code{drawOpenKey} function produces scale keys for other openair
##' functions.
##'
##' Most \code{drawOpenKey} options are identical to those of
##' \code{lattice::draw.colorkey}.  For example, scale key size and position
##' are controlled via \code{height}, \code{width} and \code{space}. Likewise,
##' the axis labelling can be set in and formatted by \code{labels}. See
##' \code{\link{draw.colorkey}} for further details.
##'
##' Additional scale labelling may be added above and below the scale using
##' \code{header} and \code{footer} options within \code{key}. As in other
##' \code{openair} functions, automatic text formatting can be enabled via
##' \code{auto.key}.
##'
##' (Note: Currently, the formatting of \code{header} and \code{footer} text
##' are fixed to the same style as \code{labels} (the scale axis) and cannot be
##' defined locally.)
##'
##' The relationship between \code{header}, \code{footer} and the scale key
##' itself can be controlled using \code{fit} options. These can be set in
##' \code{key$fit} to apply uniform control or individually in
##' \code{key$header$fit} and/or \code{key$footer$fit} to control locally.
##'
##' The appearance of the scale can be controlled using \code{plot.style}.
##'
##' @param key List defining the scale key structure to be produced. Most
##'   options are identical to original \code{draw.colorkey} function.
##'
##' Original \code{draw.colorkey} options:
##'
##' \code{space} location of the scale key ("left", "right", "top" or
##'   "bottom").  Defaults to "right".
##'
##' \code{col} vector of colours, used in scale key.
##'
##' \code{at} numeric vector specifying where the colors change. Must be of
##'   length 1 more than the col vector.
##'
##' \code{labels} a character vector for labelling the at values, or more
##'   commonly, a list describing characteristics of the labels. This list may
##'   include components \code{labels}, \code{at}, \code{cex}, \code{col},
##'   \code{rot}, \code{font}, \code{fontface} and \code{fontfamily}.
##'
##' \code{tick.number} approximate number of ticks.
##'
##' \code{width} width of the key.
##'
##' \code{height} height of key.
##'
##' Note: \code{width} and \code{height} refer to the key dimensions.
##'   \code{height} is the length of the key along the plot axis it is
##'   positioned against, and \code{width} is the length perpendicular to that.
##'
##' Additional options include:
##'
##' \code{header} a character vector of extra text to be added above the key,
##'   or a list describing some characteristics of the \code{header}. This list
##'   may include components \code{header}, the character vector of header
##'   labels, \code{tweaks}, a list of local controls, e.g. 'gap' and 'balance'
##'   for spacing relative to scale and footer, respectively, \code{auto.text},
##'   \code{TRUE/FALSE} option to apply \code{quickText}, and \code{slot}, a
##'   numeric vector setting the size of the text boxes \code{header} text is
##'   placed in.
##'
##' \code{footer} as in \code{header} but for labels below the scale key.
##'
##' Notes: \code{header} and \code{footer} formatting can not be set locally,
##'   but instead are matched to those set in \code{labels}. \code{drawOpenKey}
##'   allows for up to six additional labels (three above and three below scale
##'   key). Any additional text is ignored.
##'
##' \code{tweak, auto.text, slot} as in \code{header} and \code{footer} but
##'   sets all options uniformly. This also overwrites anything in
##'   \code{header} and/or \code{footer}.
##'
##' \code{fit} the fit method to be applied to the header, scale key and footer
##'   when placing the scale key left or right of the plot. Options include:
##'   'all', 'soft' and 'scale'.  The default 'all' fits header, key and footer
##'   into \code{height} range. The alternative 'scale' fits only the key
##'   within \code{height}. (This means that keys keep the same proportions
##'   relative to the main plot regardless of positioning but that header and
##'   footer may exceed plot dimensions if \code{height} and/or \code{slots}
##'   are too large.
##'
##' \code{plot.style} a character vector of key plotting style instructions:
##'   Options currently include: 'paddle', 'ticks' and 'border'. 'paddle'
##'   applies the incremental paddle layout used by \code{winRose}. 'ticks'
##'   places ticks between the labels scale key. 'border' places a border about
##'   the scale key. Any combination of these may be used but if none set,
##'   scale key defaults to \code{c("ticks", "border")} for most plotting
##'   operations or \code{c("paddle")} for \code{windRose}.
##'

##' @param draw Option to return the key object or plot it directly.  The
##'   default, FALSE, should always be used within openair calls.
##' @param vp View port to be used when plotting key. The default, NULL, should
##'   always be used within openair calls.
##'
##' (Note: \code{drawOpenKey} is a crude modification of
##'   \code{lattice::draw.colorkey}, that provides labelling options for
##'   \code{openair} plot scale keys. Some aspects of the function are in
##'   development and may to subject to change. Therefore, it is recommended
##'   that you use parent \code{openair} function controls, e.g.
##'   \code{key.position}, \code{key.header}, \code{key.footer} options, where
##'   possible.  \code{drawOpenKey} may obviously be used in other plots but it
##'   is recommended that \code{draw.colorkey} itself be used wherever this
##'   type of additional scale labelling is not required.)
##' @export
##' @return The function is a modification of \code{lattice::draw.colorkey} and
##'   returns a scale key using a similar mechanism to that used in in the
##'   original function as developed by Deepayan Sarkar.
##' @note We gratefully acknoweldge the considerable help and advice of
##'   Deepayan Sarkar.
##' @author \code{draw.colorkey} is part of the \code{lattice} package,
##'   developed by Deepayan Sarkar.
##'
##' Additional modifications by Karl Ropkins.
##' @seealso Functions using \code{drawOpenKey} currently include
##'   \code{\link{windRose}}, \code{\link{pollutionRose}}.
##'
##' For details of the original function, see \code{\link{draw.colorkey}}
##' @references Deepayan Sarkar (2010). lattice: Lattice Graphics. R package
##'   version 0.18-5.  http://r-forge.r-project.org/projects/lattice/
##' @keywords methods
##' @examples
##'
##'
##' ##########
##' #example 1
##' ##########
##'
##' #paddle style scale key used by windRose
##'
##' windRose(mydata,)
##'
##' #adding text and changing style and position via key
##'
##' #note:
##' #some simple key control also possible directly
##' #For example, below does same as
##' #windRose(mydata, key.position="right")
##'
##' windRose(mydata,
##'    key =list(space="right")
##' )
##'
##' #however:
##' #more detailed control possible working with
##' #key and drawOpenKey. For example,
##'
##' windRose(mydata,
##'    key = list(header="Title", footer="wind speed",
##'               plot.style = c("ticks", "border"),
##'               fit = "all", height = 1,
##'               space = "top")
##' )
##'
##'
drawOpenKey <- function (key, draw = FALSE, vp = NULL) {
  
  ################
  #quick end if key obviously not right
  ################
  if (!is.list(key))
    stop("In drawOpenKey(...) key must be a list",
         call. = FALSE)
  
  ################
  #special case
  #windRose colour key
  ################
  if(is.null(key$at)){
    if(is.null(key$labels)){
      stop("In drawOpenKey(...) neither 'at' nor 'labels' in key",
           "\n\tplease suppied at least one",
           call. = FALSE)
    } else {
      if(is.list(key$labels)){
        if(is.null(key$labels$labels))
          stop("In drawOpenKey(...) unable to recover missing 'at' in key",
               "\n\tplease check 'labels' structure or add 'at'",
               call. = FALSE)
        key$at <- 0:length(key$labels$labels)
        if(is.null(key$labels$at)) {
          key$labels$at <- 1:length(key$labels$labels) - 0.5
        }
      } else {
        key$at <- 0:length(key$labels)
        key$labels <- list(labels = key$labels,
                           at = 1:length(key$labels) - 0.5)
      }
    }
  }
  
  ################
  #process key
  #modification of sk
  ################
  process.key <- function(col = regions$col, alpha = regions$alpha,
                          at, tick.number = 7, width = 2, height = 1, space = "right",
                          plot.style = c("ticks", "border"),
                          ...) {
    regions <- trellis.par.get("regions")
    list(col = col, alpha = alpha, at = at, tick.number = tick.number,
         width = width, height = height, space = space,
         plot.style = plot.style,
         ...)
  }
  axis.line <- trellis.par.get("axis.line")
  axis.text <- trellis.par.get("axis.text")
  key <- do.call("process.key", key)
  
  ###############
  #test space
  #otherwise drops without creating key.gf
  #COULD default to one?
  ###############
  temp <- c("right", "left", "top", "bottom")
  if (!key$space %in% temp) {
    stop(" In drawOpenKey(...):", "\n\tkey.position (space) argument in key not recognised",
         "\n\tplease use one of:\n\t\"", paste(temp,
                                               sep = "", collapse = "\", \""), "\"", call. = FALSE)
  }
  
  ###############
  #original sk key handling
  #with
  #modified error messaging
  ###############
  check.overlap <- TRUE
  key$at <- sort(key$at)
  numcol <- length(key$at) - 1
  key$col <- level.colors(x = seq_len(numcol) - 0.5, at = seq_len(numcol +
                                                                    1) - 1, col.regions = key$col, colors = TRUE)
  atrange <- range(key$at, finite = TRUE)
  scat <- as.numeric(key$at)
  reccentre <- (scat[-1] + scat[-length(scat)])/2
  recdim <- diff(scat)
  cex <- axis.text$cex
  col <- axis.text$col
  font <- axis.text$font
  fontfamily <- axis.text$fontfamily
  fontface <- axis.text$fontface
  rot <- 0
  if (is.null(key$lab)) {
    at <- pretty(atrange, key$tick.number)
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- format(at, trim = TRUE)
  } else if ((is.character(key$lab) | is.expression(key$lab) | is.numeric(key$lab))
             && length(key$lab) == length(key$at)) {
    check.overlap <- FALSE
    at <- key$at
    labels <- key$lab
  } else if (is.list(key$lab)) {
    at <- if (!is.null(key$lab$at))
      key$lab$at
    else pretty(atrange, key$tick.number)
    at <- at[at >= atrange[1] & at <= atrange[2]]
    labels <- if (!is.null(key$lab$lab)) {
      check.overlap <- FALSE
      key$lab$lab
    } else format(at, trim = TRUE)
    if (!is.null(key$lab$cex))
      cex <- key$lab$cex
    if (!is.null(key$lab$col))
      col <- key$lab$col
    if (!is.null(key$lab$font))
      font <- key$lab$font
    if (!is.null(key$lab$fontface))
      fontface <- key$lab$fontface
    if (!is.null(key$lab$fontfamily))
      fontfamily <- key$lab$fontfamily
    if (!is.null(key$lab$rot))
      rot <- key$lab$rot
  } else {
    stop("In drawOpenKey(...) unexpected labels structure in key",
         "\n\tplease check 'labels' structure",
         "\n\tor see 'labels' in ?drawOpenKey",
         call. = FALSE)    }
  labscat <- at
  rot <- 0
  
  #############
  #header set up
  #############
  if(is.null(key$hea))
    key$hea <- list(header="")
  if(is.character(key$hea) | is.numeric(key$hea) | is.expression(key$hea) )
    key$hea <- list(header=key$hea)
  if(is.list(key$hea)){
    h.text <- if(is.null(key$hea$hea)) "" else key$hea$hea
    h.tweaks <- if(is.null(key$hea$twe)) c("gap", "balance") else key$hea$twe
    h.auto.text <- if(is.null(key$hea$auto.text)) TRUE else key$hea$auto.text
    h.slot <- if(is.null(key$hea$slot)) 0.05 else key$hea$slot
  } else {
    stop("In drawOpenKey(...) unexpected header structure in key",
         "\n\tplease check 'header' structure",
         "\n\tor see 'header' in ?drawOpenKey",
         call. = FALSE)
  }
  
  ############
  #footer setup
  ############
  if(is.null(key$foo))
    key$foo <- list(footer="")
  if(is.character(key$foo) | is.numeric(key$foo) | is.expression(key$foo) )
    key$foo <- list(footer=key$foo)
  if(is.list(key$foo)){
    f.text <- if(is.null(key$foo$foo)) "" else key$foo$foo
    f.tweaks <- if(is.null(key$foo$twe)) c("gap", "balance") else key$foo$twe
    f.auto.text <- if(is.null(key$foo$auto.text)) TRUE else key$foo$auto.text
    f.slot <- if(is.null(key$foo$slot)) 0.05 else key$foo$slot
  } else {
    stop("In drawOpenKey(...) unexpected footer structure in key",
         "\n\tplease check 'footer' structure",
         "\n\tor see 'footer' in ?drawOpenKey",
         call. = FALSE)
  }
  
  #################
  #higher level handling
  #auto.text, slot, tweak,
  #################
  if(!is.null(key$auto.text)) {
    if(is.logical(key$auto.text)){
      h.auto.text <- key$auto.text
      f.auto.text <- key$auto.text
    }
  }
  if(!is.null(key$slot)) {
    if(is.numeric(key$slot)){
      h.slot <- key$slot
      f.slot <- key$slot
    }
  }
  if(!is.null(key$twe)){
    if(is.vector(key$twe)){
      h.tweaks <- key$twe
      f.tweaks <- key$twe
    }
  }
  
  ###############
  #size text boxes, balance and gap
  #for
  #top and bottom only
  ###############
  h.text <- if(length(h.text) < 3)  c(rep("", 3-length(h.text)), h.text) else
    h.text[1:3]
  h.slots <- ifelse(as.character(h.text) != "", h.slot, 0)
  f.text <- c(f.text, rep("", 3))[1:3]
  f.slots <- ifelse(as.character(f.text) != "", f.slot, 0)
  if(sum(h.slots) > sum(f.slots) & "balance" %in% f.tweaks)
    f.slots[3] <- f.slots[3] + sum(h.slots) - sum(f.slots)
  if(sum(f.slots) > sum(h.slots) & "balance" %in% h.tweaks)
    h.slots[1] <- h.slots[1] + sum(f.slots) - sum(h.slots)
  g.slots <- c(if("gap" %in% h.tweaks & sum(c(h.slots,f.slots))>0) h.slot else 0,
               if("gap" %in% f.tweaks & sum(c(h.slots,f.slots))>0) f.slot else 0)
  
  #############
  #scale fit
  #scale, soft and all
  #default all
  #############
  s.slot <- 1 - sum(c(h.slots,f.slots,g.slots))
  s.offsets <- c(0, 0)
  if(!is.null(key$fit)) {
    if(is.character(key$fit)){
      if(key$fit=="soft")
        s.slot <- 1 - (sum(c(h.slots,f.slots,g.slots))/2)
      if(key$fit=="scale"){
        s.slot <- 1
        s.offsets <- c(sum(c(h.slots,g.slots[1])),
                       sum(c(f.slots,g.slots[2])))
      }
    } else {
      stop("In drawOpenKey(...) unexpected fit structure in key",
           "\n\tplease check 'fit' structure",
           "\n\tor see 'fit' in ?drawOpenKey",
           call. = FALSE)
    }
  }
  
  ############
  #paddle style
  #recwd rescaling
  #############
  recwd <- if("paddle" %in% key$plot.style)
    recwd <- seq(0.2, 1, length.out = length(key$at) - 1) else
      recwd <- rep(1, length(key$at) - 1)
  
  #####################
  #right scale
  #size checks text see sac struff
  #positions
  #adds ticks and borders if requested
  #####################
  if (key$space == "right") {
    h.text <- if(is.character(h.text))
      lapply(h.text, function(x) quickText(x, h.auto.text)) else
        list(h.text[1], h.text[2], h.text[3])
    f.text <- if(is.character(f.text))
      lapply(f.text, function(x) quickText(x, h.auto.text)) else
        list(f.text[1], f.text[2], f.text[3])
    #sac stuff handles spacing needed for headers, scale and footers
    sac.text <- c(labels, f.text[[1]], f.text[[2]], f.text[[3]],
                  h.text[[1]], h.text[[2]], h.text[[3]])
    SacGrob <- grid::textGrob(label = sac.text, x = rep(0, length(sac.text)),
                        y = at, vp = grid::viewport(yscale = atrange), default.units = "native",
                        check.overlap = check.overlap, just = if (rot == -90)
                          c("center", "bottom")
                        else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                       cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                 font)))
    heights.x <- c(((1 -key$height)/2) - (key$height*s.offsets[1]),
                   key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
                   key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2],
                   key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
                   ((1 -key$height)/2) - (key$height*s.offsets[2]))
    heights.units <- rep("null", 11)
    temp <- if("ticks" %in% key$plot.style) 0.6 else 0.3
    widths.x <- c(0.6 * key$width, temp, 1)
    widths.units <- c("lines", "lines", "grobwidth")
    widths.data <- list(NULL, NULL, SacGrob)
    key.layout <- grid::grid.layout(nrow = 11, ncol = 3, heights = grid::unit(heights.x,
                                                                  heights.units), widths = grid::unit(widths.x, widths.units,
                                                                                                data = widths.data), respect = TRUE)
    key.gf <- grid::frameGrob(layout = key.layout, vp = vp)
    add.header.footer <- function(key.gf, text, key.row, key.col){
      keyGrob <- grid::textGrob(label = text, x = c(0),
                          y = c(0.5), vp = grid::viewport(yscale = c(0,1)), default.units = "native",
                          check.overlap = check.overlap, just = if (rot == -90)
                            c("center", "bottom")
                          else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                         cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                   font)))
      grid::placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
    }
    key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 3)
    key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 3)
    key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 3)
    key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 3)
    key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 3)
    key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 3)
    key.gf <- grid::placeGrob(key.gf, grid::textGrob(label = labels, x = rep(0, length(at)),
                                         y = at, vp = grid::viewport(yscale = atrange), default.units = "native",
                                         check.overlap = check.overlap, just = if (rot == -90)
                                           c("center", "bottom")
                                         else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                                        cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                                  font))), row = 6, col = 3)
    key.gf <- grid::placeGrob(key.gf, grid::rectGrob(x = rep(0.5, length(reccentre)),
                                         y = reccentre, default.units = "native", vp = grid::viewport(yscale = atrange),
                                         height = recdim, width = recwd, gp = grid::gpar(fill = key$col, col = "transparent",
                                                                                   alpha = key$alpha)), row = 6, col = 1)
    if("border" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::rectGrob(gp = grid::gpar(col = axis.line$col,
                                                             lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
                                                             fill = "transparent")), row = 6, col = 1)
    if("ticks" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::segmentsGrob(x0 = rep(0,
                                                                length(labscat)), y0 = labscat, x1 = rep(0.4, length(labscat)),
                                                       y1 = labscat, vp = grid::viewport(yscale = atrange), default.units = "native",
                                                       gp = grid::gpar(col = axis.line$col, lty = axis.line$lty,
                                                                 lwd = axis.line$lwd)), row = 6, col = 2)
  }
  
  #####################
  #left scale
  #size checks text see sac struff
  #positions
  #adds ticks and borders if requested
  #####################
  else if (key$space == "left") {
    h.text <- if(is.character(h.text))
      lapply(h.text, function(x) quickText(x, h.auto.text)) else
        list(h.text[1], h.text[2], h.text[3])
    f.text <- if(is.character(f.text))
      lapply(f.text, function(x) quickText(x, h.auto.text)) else
        list(f.text[1], f.text[2], f.text[3])
    #sac stuff handles spacing needed for headers, scale and footers
    sac.text <- c(labels, f.text[[1]], f.text[[2]], f.text[[3]],
                  h.text[[1]], h.text[[2]], h.text[[3]])
    SacGrob <- grid::textGrob(label = sac.text, x = rep(0, length(sac.text)),
                        y = at, vp = grid::viewport(yscale = atrange), default.units = "native",
                        check.overlap = check.overlap, just = if (rot == 90)
                          c("center", "bottom")
                        else c("right", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                        cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                  font)))
    heights.x <- c(((1 -key$height)/2) - (key$height*s.offsets[1]),
                   key$height * h.slots[1], key$height * h.slots[2], key$height * h.slots[3],
                   key$height * g.slots[1], (key$height * s.slot), key$height * g.slots[2],
                   key$height * f.slots[1], key$height * f.slots[2], key$height * f.slots[3],
                   ((1 -key$height)/2) - (key$height*s.offsets[2]))
    heights.units <- rep("null", 11)
    temp <- if("ticks" %in% key$plot.style) 0.6 else 0.3
    widths.x <- c(1, temp, 0.6 * key$width)
    widths.units <- c("grobwidth", "lines", "lines")
    widths.data <- list(SacGrob, NULL, NULL)
    key.layout <- grid::grid.layout(nrow = 11, ncol = 3, heights = grid::unit(heights.x,
                                                                  heights.units), widths = grid::unit(widths.x, widths.units,
                                                                                                data = widths.data), respect = TRUE)
    key.gf <- grid::frameGrob(layout = key.layout, vp = vp)
    add.header.footer <- function(key.gf, text, key.row, key.col){
      keyGrob <- grid::textGrob(label = text, x = c(1),
                          y = c(0.5), vp = grid::viewport(yscale = c(0,1)), default.units = "native",
                          check.overlap = check.overlap, just = if (rot == 90)
                            c("center", "bottom")
                          else c("right", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                          cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                    font)))
      grid::placeGrob(key.gf, keyGrob, row = key.row, col = key.col)
    }
    key.gf <- add.header.footer(key.gf, h.text[[1]], 2, 1)
    key.gf <- add.header.footer(key.gf, h.text[[2]], 3, 1)
    key.gf <- add.header.footer(key.gf, h.text[[3]], 4, 1)
    key.gf <- add.header.footer(key.gf, f.text[[1]], 8, 1)
    key.gf <- add.header.footer(key.gf, f.text[[2]], 9, 1)
    key.gf <- add.header.footer(key.gf, f.text[[3]], 10, 1)
    key.gf <- grid::placeGrob(key.gf,
                        grid::textGrob(label = labels, x = rep(1, length(at)),
                                 y = at, vp = grid::viewport(yscale = atrange), default.units = "native",
                                 check.overlap = check.overlap, just = if (rot == 90)
                                   c("center", "bottom")
                                 else c("right", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                                 cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                           font)))
                        , row = 6, col = 1)
    key.gf <- grid::placeGrob(key.gf, grid::rectGrob(x = rep(0.5, length(reccentre)),
                                         y = reccentre, default.units = "native", vp = grid::viewport(yscale = atrange),
                                         height = recdim, width = recwd, gp = grid::gpar(fill = key$col, col = "transparent",
                                                                                   alpha = key$alpha)), row = 6, col = 3)
    if("border" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::rectGrob(gp = grid::gpar(col = axis.line$col,
                                                             lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
                                                             fill = "transparent")), row = 6, col = 3)
    if("ticks" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::segmentsGrob(x0 = rep(0.5,
                                                                length(labscat)), y0 = labscat, x1 = rep(1, length(labscat)),
                                                       y1 = labscat, vp = grid::viewport(yscale = atrange), default.units = "native",
                                                       gp = grid::gpar(col = axis.line$col, lty = axis.line$lty,
                                                                 lwd = axis.line$lwd)), row = 6, col = 2)
  }
  
  #####################
  #top scale
  #positions
  #adds ticks and borders if requested
  #####################
  else if (key$space == "top") {
    f.text <- f.text[as.character(f.text) != ""]
    f.text <- if(is.character(f.text)) quickText(paste(f.text, collapse="  "), f.auto.text) else
      as.expression(parse(text=paste(f.text, collapse="~~")))
    h.text <- h.text[as.character(h.text) != ""]
    h.text <- if(is.character(h.text)) quickText(paste(h.text, collapse="  "), f.auto.text) else
      as.expression(parse(text=paste(h.text, collapse="~~")))
    labelsGrob <- grid::textGrob(label = labels, y = rep(0, length(at)),
                           x = at, vp = grid::viewport(xscale = atrange), default.units = "native",
                           check.overlap = check.overlap, just = if (rot == 0)
                             c("center", "bottom")
                           else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                          cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                    font)))
    keyGrob <- grid::textGrob(label = f.text, y = c(0),
                        x = c(0.5), vp = grid::viewport(xscale = c(0,1)), default.units = "native",
                        check.overlap = check.overlap, just = if (rot == 0)
                          c("center", "bottom")
                        else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                       cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                 font)))
    keyGrob2 <- grid::textGrob(label = h.text, y = c(0),
                         x = c(0.5), vp = grid::viewport(xscale = c(0,1)), default.units = "native",
                         check.overlap = check.overlap, just = if (rot == 0)
                           c("center", "bottom")
                         else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                        cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                  font)))
    widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
    widths.units <- rep("null", 3)
    temp <- c(0,0,0,0.3)
    if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])==0)
      temp[1:3] <- c(0,1,1.5)
    if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])>0)
      temp[1:3] <- c(1,0,1.5)
    if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])>0)
      temp[1:3] <- c(1,1.5,1.5)
    if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])==0)
      temp[1:3] <- c(1,1,1)
    if("ticks" %in% key$plot.style)
      temp[4] <- 0.6
    heights.x <- c(temp[1], temp[2], temp[3], temp[4], 0.6 * key$width)
    heights.units <- c("grobheight", "grobheight", "grobheight", "lines", "lines")
    heights.data <- list(keyGrob2, keyGrob, labelsGrob, NULL, NULL)
    key.layout <- grid::grid.layout(nrow = 5, ncol = 3, heights = grid::unit(heights.x,
                                                                 heights.units, data = heights.data), widths = grid::unit(widths.x,
                                                                                                                    widths.units), respect = TRUE)
    key.gf <- grid::frameGrob(layout = key.layout, vp = vp)
    key.gf <- grid::placeGrob(key.gf, grid::rectGrob(y = rep(0.5, length(reccentre)),
                                         x = reccentre, default.units = "native", vp = grid::viewport(xscale = atrange),
                                         width = recdim, height = recwd, gp = grid::gpar(fill = key$col, col = "transparent",
                                                                                   alpha = key$alpha)), row = 5, col = 2)
    if("border" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::rectGrob(gp = grid::gpar(col = axis.line$col,
                                                             lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
                                                             fill = "transparent")), row = 5, col = 2)
    if("ticks" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::segmentsGrob(y0 = rep(0,
                                                                length(labscat)), x0 = labscat, y1 = rep(0.4, length(labscat)),
                                                       x1 = labscat, vp = grid::viewport(xscale = atrange), default.units = "native",
                                                       gp = grid::gpar(col = axis.line$col, lty = axis.line$lty,
                                                                 lwd = axis.line$lwd)), row = 4, col = 2)
    key.gf <- grid::placeGrob(key.gf, labelsGrob, row = 3, col = 2)
    key.gf <- grid::placeGrob(key.gf, keyGrob, row = 2, col = 2)
    key.gf <- grid::placeGrob(key.gf, keyGrob2, row = 1, col = 2)
  }
  
  #####################
  #bottom scale
  #positions
  #adds ticks and borders if requested
  #####################
  else if (key$space == "bottom") {
    f.text <- f.text[as.character(f.text) != ""]
    f.text <- if(is.character(f.text)) quickText(paste(f.text, collapse="  "), f.auto.text) else
      as.expression(parse(text=paste(f.text, collapse="~~")))
    h.text <- h.text[as.character(h.text) != ""]
    h.text <- if(is.character(h.text)) quickText(paste(h.text, collapse="  "), f.auto.text) else
      as.expression(parse(text=paste(h.text, collapse="~~")))
    temp <- c(0.3,1,0,0)
    if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])==0)
      temp[2:4] <- c(1,1,1.5)
    if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])>0)
      temp[2:4] <- c(1,1.5,1)
    if(sum(f.slots[1:2])>0 & sum(h.slots[2:3])>0)
      temp[2:4] <- c(1,1.5,1.5)
    if(sum(f.slots[1:2])==0 & sum(h.slots[2:3])==0)
      temp[2:4] <- c(1,1,1)
    if("ticks" %in% key$plot.style)
      temp[1] <- 0.6
    labelsGrob <- grid::textGrob(label = labels, y = rep(0, length(at)),
                           x = at, vp = grid::viewport(xscale = atrange), default.units = "native",
                           check.overlap = check.overlap, just = if (rot == 0)
                             c("center", "bottom")
                           else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                          cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                    font)))
    keyGrob <- grid::textGrob(label = h.text, y = c(0),
                        x = c(0.5), vp = grid::viewport(xscale = c(0,1)), default.units = "native",
                        check.overlap = check.overlap, just = if (rot == 0)
                          c("center", "bottom")
                        else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                       cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                 font)))
    keyGrob2 <- grid::textGrob(label = f.text, y = c(0),
                         x = c(0.5), vp = grid::viewport(xscale = c(0,1)), default.units = "native",
                         check.overlap = check.overlap, just = if (rot == 0)
                           c("center", "bottom")
                         else c("left", "center"), rot = rot, gp = grid::gpar(col = col,
                                                                        cex = cex, fontfamily = fontfamily, fontface = chooseFace(fontface,
                                                                                                                                  font)))
    widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
    widths.units <- rep("null", 3)
    heights.x <- c(0.6 * key$width, temp[1], temp[2], temp[3], temp[4])
    heights.units <- c("lines", "lines", "grobheight", "grobheight", "grobheight")
    heights.data <- list(NULL, NULL, labelsGrob, keyGrob, keyGrob2)
    key.layout <- grid::grid.layout(nrow = 5, ncol = 3, heights = grid::unit(heights.x,
                                                                 heights.units, data = heights.data), widths = grid::unit(widths.x,
                                                                                                                    widths.units), respect = TRUE)
    key.gf <- grid::frameGrob(layout = key.layout, vp = vp)
    key.gf <- grid::placeGrob(key.gf, grid::rectGrob(y = rep(0.5, length(reccentre)),
                                         x = reccentre, default.units = "native", vp = grid::viewport(xscale = atrange),
                                         width = recdim, height = recwd, gp = grid::gpar(fill = key$col, col = "transparent",
                                                                                   alpha = key$alpha)), row = 1, col = 2)
    if("ticks" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::segmentsGrob(y0 = rep(1,
                                                                length(labscat)), x0 = labscat, y1 = rep(0.6, length(labscat)),
                                                       x1 = labscat, vp = grid::viewport(xscale = atrange), default.units = "native",
                                                       gp = grid::gpar(col = axis.line$col, lty = axis.line$lty,
                                                                 lwd = axis.line$lwd)), row = 2, col = 2)
    if("border" %in% key$plot.style)
      key.gf <- grid::placeGrob(frame = key.gf, grid::rectGrob(gp = grid::gpar(col = axis.line$col,
                                                             lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha,
                                                             fill = "transparent")), row = 1, col = 2)
    key.gf <- grid::placeGrob(key.gf, labelsGrob, row = 3, col = 2)
    key.gf <- grid::placeGrob(key.gf, keyGrob, row = 4, col = 2)
    key.gf <- grid::placeGrob(key.gf, keyGrob2, row = 5, col = 2)
  }
  
  ##############
  #outputs
  ##############
  if (draw)
    grid.draw(key.gf)
  key.gf
}


## from lattice
chooseFace <- function (fontface = NULL, font = 1)
{
  if (is.null(fontface))
    font
  else fontface
}


