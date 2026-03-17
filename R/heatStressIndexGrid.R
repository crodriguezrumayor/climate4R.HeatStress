#' 
#' @title Heat Stress Indices in climate4R
#' @description Calculation of 9 heat stress indices. The function is a wrapper of the package \pkg{HeatStress} 
#' for its seamless integration with climate4R objects.
#' @param index.code Character string, indicating the specific code of the heat stress index to be computed (see Details).
#' @param tas A climate4R dataset of air temperature (degrees C)
#' @param dewp A climate4R dataset of dew point temperature (degrees C)
#' @param hurs A climate4R dataset of relative humidity (%)
#' @param wind A climate4R dataset of wind speed (m s-1)
#' @param radiation A climate4R dataset of radiation (W m-2)
#' @param parallel Logical. Enable parallel processing (default = FALSE)
#' @param max.ncores Integer. Maximum number of cores to use (default = 16)
#' @param ncores Integer. Specific number of cores to use
#' @return A climate4R object with the computed index.
#' 
#' @import transformeR
#' @importFrom parallel stopCluster
#' @importFrom magrittr %>% %<>% 
#' @importFrom utils head
#' @importFrom convertR udConvertGrid
#' @importFrom udunits2 ud.are.convertible ud.convert
#' @import HeatStress
#' @details \code{\link{indexShow}} will display on screen a full list of heat stress indices, their codes and the required input 
#' variables. The names of the internal functions calculating each index are also displayed, whose help files can aid in 
#' the definition of index-specific arguments.
#' 
#' @examples \dontrun{
#' require(climate4R.HeatStress)
#'
#' # Example 1: Heat Index (tas + hurs)
#' data("ERA5_day_t2m", package = "climate4R.HeatStress")
#' data("ERA5_day_hurs", package = "climate4R.HeatStress")
#' hi <- heatStressIndexGrid("hi", tas = ERA5_day_t2m, hurs = ERA5_day_hurs)
#'
#' # Example 2: WBGT shade (tas + dewp)
#' data("ERA5_day_d2m", package = "climate4R.HeatStress")
#' wbgt_shade <- heatStressIndexGrid("wbgt_shade", tas = ERA5_day_t2m, dewp = ERA5_day_d2m)
#'
#' # Example 3: WBGT sun (tas + dewp + wind + radiation)
#' data("ERA5_day_sfcwind", package = "climate4R.HeatStress")
#' data("ERA5_day_ssrd", package = "climate4R.HeatStress")
#' wbgt_sun <- heatStressIndexGrid("wbgt_sun", tas = ERA5_day_t2m, dewp = ERA5_day_d2m,
#'                                  wind = ERA5_day_sfcwind, radiation = ERA5_day_ssrd)
#' }
#' 
#' @author C. Rodriguez-Rumayor
#' @export

heatStressIndexGrid <- function(index.code,
                                tas = NULL,
                                dewp = NULL,
                                hurs = NULL,
                                wind = NULL,
                                radiation = NULL, 
                                parallel = FALSE,
                                max.ncores = 16,
                                ncores = NULL) {
    index.code <- match.arg(index.code,
                            choices = c("wbt", "wbgt_shade", "wbgt_sun",
                                        "swbgt", "apparentTemp", "effectiveTemp",
                                        "humidex", "discomInd", "hi"))
    aux <- read.master()
    metadata <- aux[grep(paste0("^", index.code, "$"), aux$code, fixed = FALSE), ]    
    a <- c(!is.null(tas), !is.null(dewp), !is.null(hurs), !is.null(wind), !is.null(radiation)) %>% as.numeric()
    b <- metadata[ , 4:8] %>% as.numeric()
    if (any(b - a > 0)) {
        stop("The required input variable(s) for ", index.code,
             " index calculation are missing\nType \'?",
             metadata$indexfun, "\' for help", call. = FALSE)
    }
     # Remove any possible uneeded input grid
    if (any(a - b > 0)) {
        ind <- which((a - b) > 0)
        rem <- c("tas", "dewp", "hurs", "wind", "radiation")[ind]
        for (x in rem) assign(x, NULL, envir = environment())
        message("NOTE: some input grids provided for ", index.code,
                " index calculation are not required and were removed")
    }

    # Basic object validation
    stopifnot(all(sapply(list(tas, dewp, hurs, wind, radiation), function(x) is.null(x) | isGrid(x))))
    if (any(sapply(list(tas, dewp, hurs, wind, radiation), isMultigrid))) {
        stop("Multigrids are not an allowed input")
    }

    # Convert inputs to required units (if needed)
    if(!is.null(tas)) {
        tas.u <- getGridUnits(tas)
        if (tolower(tas.u) %in% c("degrees celsius", "degree celsius")) {
            attr(tas$Variable, "units") <- "degC"
            tas.u <- "degC"
        }
        if (tolower(tas.u) %in% c("degrees fahrenheit", "degree fahrenheit")) {
            attr(tas$Variable, "units") <- "degF"
            tas.u <- "degF"
        }
        if (ud.are.convertible(tas.u, "degC")) {
            if (ud.convert(1, tas.u, "degC") != 1) { 
                message("[", Sys.time(), "] Converting air temperature units ...")
                tas %<>% udConvertGrid(new.units = "degC") 
            }  
        } else {
            stop("Non compliant tas units (", tas.u, " is not convertible to degC)")
        }
    }
    if(!is.null(dewp)) {
        dewp.u <- getGridUnits(dewp)
        if (tolower(dewp.u) %in% c("degrees celsius", "degree celsius")) {
            attr(dewp$Variable, "units") <- "degC"
            dewp.u <- "degC"
        }
        if (tolower(dewp.u) %in% c("degrees fahrenheit", "degree fahrenheit")) {
            attr(dewp$Variable, "units") <- "degF"
            dewp.u <- "degF"
        }
        if (ud.are.convertible(dewp.u, "degC")) {
            if (ud.convert(1, dewp.u, "degC") != 1) { 
                message("[", Sys.time(), "] Converting dew point temperature units ...")
                dewp %<>% udConvertGrid(new.units = "degC") 
            }  
        } else {
            stop("Non compliant dewp units (", dewp.u, " is not convertible to degC)")
        }
    }
    if(!is.null(hurs)) {
        hurs.u <- getGridUnits(hurs)
        if (tolower(hurs.u) %in% c("percentage")) {
            attr(hurs$Variable, "units") <- "%"
            hurs.u <- "%"
        }
        if (ud.are.convertible(hurs.u, "%")) {
            if (ud.convert(1, hurs.u, "%") != 1) { 
                message("[", Sys.time(), "] Converting relative humidity units ...")
                hurs %<>% udConvertGrid(new.units = "%") 
            }  
        } else {
            stop("Non compliant hurs units (", hurs.u, " is not convertible to %)")
        }
    }
    if(!is.null(wind)) {
        wind.u <- getGridUnits(wind)
        if (ud.are.convertible(wind.u, "m s-1")) {
            if (ud.convert(1, wind.u, "m s-1") != 1) { 
                message("[", Sys.time(), "] Converting wind speed units ...")
                wind %<>% udConvertGrid(new.units = "m s-1") 
            }  
        } else {
            stop("Non compliant wind units (", wind.u, " is not convertible to m s-1)")
        }
    }
    # Allow conversion of radiation from energy flux to power flux (e.g. J m-2 to W m-2) 
    if (!is.null(radiation)) {
        rad.u <- getGridUnits(radiation)
        if (ud.are.convertible(rad.u, "W m-2")) {
            if (ud.convert(1, rad.u, "W m-2") != 1) {
                message("[", Sys.time(), "] Converting radiation units ...")
                radiation %<>% udConvertGrid(new.units = "W m-2")
            } 
        } else if (ud.are.convertible(rad.u, "J m-2")) {
            if (ud.convert(1, rad.u, "J m-2") != 1) {
                radiation %<>% udConvertGrid(new.units = "J m-2")
            }
            message("[", Sys.time(), "] Converting radiation units ...")
            time_step <- difftime(as.POSIXct(getRefDates(radiation)[2], tz = "UTC"),
                                  as.POSIXct(getRefDates(radiation)[1], tz = "UTC"),
                                  units = "secs") %>% as.numeric()
            radiation$Data <- radiation$Data / time_step
            attr(radiation$Variable, "units") <- "W m-2"
        } else {
            stop("Non compliant radiation units (", rad.u, " is not convertible to 'W m-2' or 'J m-2')")
        } 
    }

    # Sanity check on hurs
    if (!is.null(hurs) && any(hurs$Data < 0 | hurs$Data > 100, na.rm = TRUE)) {
        warning("Some relative humidity values are outside the expected [0, 100] range")
    }

  
    # Ensure member is present in data structures 
    if (!is.null(tas)) tas %<>% redim(member = TRUE)
    if (!is.null(dewp)) dewp %<>% redim(member = TRUE)
    if (!is.null(hurs)) hurs %<>% redim(member = TRUE)
    if (!is.null(wind)) wind %<>% redim(member = TRUE)
    if (!is.null(radiation)) radiation %<>% redim(member = TRUE)

    # Consistency checks
    non_null_grids <- list(tas, dewp, hurs, wind, radiation)[!sapply(list(tas, dewp, hurs, wind, radiation), is.null)]
    grid_types <- sapply(non_null_grids, typeofGrid)

    if (length(unique(grid_types)) > 1) {
        stop("All input variables must be of the same type (either grid or station).")
    }
    suppressMessages(do.call(checkDim, c(non_null_grids, list(dimensions = c("time", "lat", "lon")))))

    station <- unique(grid_types) == "station"

    # Get reference grid
    refGridName <- c("tas","dewp","hurs", "wind", "radiation")[
        which(c(!is.null(tas), !is.null(dewp), !is.null(hurs), !is.null(wind), !is.null(radiation)) %>% as.numeric() != 0)
        ] %>% head(1)
    refGrid <- get(refGridName)
    n.mem <- getShape(refGrid, "member")
    n.loc <- ifelse(station, nrow(getCoordinates(refGrid)), prod(getShape(refGrid)[c("lat", "lon")]))
    
    # Get index function for generic calculation (considering exceptions)
    if (!index.code %in% c("wbgt_shade", "wbgt_sun")) {
        index_func <- get(trimws(metadata$indexfun), asNamespace("HeatStress"))
    }

    # Generic wbgt_sun settings
    if (index.code == "wbgt_sun") {
        flat_coords <- get2DmatCoordinates(refGrid)
        ref_dates <- format(as.POSIXct(getRefDates(refGrid), tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    }

    message("[", Sys.time(), "] Calculating ", index.code, " ...")
    
    # Setup parallel processing
    if (n.mem > 1) {
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        apply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "lapply")
        if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
    } else {
        if (isTRUE(parallel)) message("NOTE: Parallel processing was skipped (unable to parallelize one single member)")
        apply_fun <- lapply
    }
    
    # Process each member
    out.list <- apply_fun(1:n.mem, function(x) {
        # Extract member data and convert to 2D matrix (time x location)
        tas.mem <- if (!is.null(tas)) {
            tmp <- subsetGrid(tas, members = x, drop = TRUE)[["Data"]]
            if (station) tmp else array3Dto2Dmat(tmp)
        } else NULL
        dewp.mem <- if (!is.null(dewp)) {
            tmp <- subsetGrid(dewp, members = x, drop = TRUE)[["Data"]]
            if (station) tmp else array3Dto2Dmat(tmp)
        } else NULL
        hurs.mem <- if (!is.null(hurs)) {
            tmp <- subsetGrid(hurs, members = x, drop = TRUE)[["Data"]]
            if (station) tmp else array3Dto2Dmat(tmp)
        } else NULL
        wind.mem <- if (!is.null(wind)) {
            tmp <- subsetGrid(wind, members = x, drop = TRUE)[["Data"]]
            if (station) tmp else array3Dto2Dmat(tmp)
        } else NULL
        radiation.mem <- if (!is.null(radiation)) {
            tmp <- subsetGrid(radiation, members = x, drop = TRUE)[["Data"]]
            if (station) tmp else array3Dto2Dmat(tmp)
        } else NULL
        
        # Initialize output matrix
        n.time <- nrow(Filter(Negate(is.null), list(tas.mem, dewp.mem, hurs.mem, wind.mem, radiation.mem))[[1]])
        index.data <- matrix(NA, nrow = n.time, ncol = n.loc)
        
        # Calculate index for each location
        for (i in 1:n.loc) {
            # Extract time series at location i
            tas.col <- if (!is.null(tas.mem)) tas.mem[, i] else NULL
            dewp.col <- if (!is.null(dewp.mem)) dewp.mem[, i] else NULL
            hurs.col <- if (!is.null(hurs.mem)) hurs.mem[, i] else NULL
            wind.col <- if (!is.null(wind.mem)) wind.mem[, i] else NULL
            radiation.col <- if (!is.null(radiation.mem)) radiation.mem[, i] else NULL
            
            # Calculate based on index type
            if (index.code == "wbgt_shade") {
                aux <- HeatStress::wbgt.Bernard(tas.col, dewp.col)
                index.data[, i] <- aux$data
            } else if (index.code == "wbgt_sun") {
                aux <- HeatStress::wbgt.Liljegren(tas.col, dewp.col, wind.col, radiation.col,
                                                   dates = ref_dates,
                                                   lon = flat_coords$x[i],
                                                   lat = flat_coords$y[i],
                                                   hour = TRUE)
                index.data[, i] <- aux$data
            } else {
                # Generic calculation for all other indices
                index.data[, i] <- do.call(index_func,      
                                          Filter(Negate(is.null), list(tas.col, dewp.col, hurs.col, wind.col, radiation.col)))
            }
        }
        
        # Build output grid
        out.grid <- refGrid
        if (station) {
            out.grid[["Data"]] <- index.data
            attr(out.grid[["Data"]], "dimensions") <- c("time", "loc")
        } else {
            xy <- getCoordinates(refGrid)
            out.grid[["Data"]] <- mat2Dto3Darray(index.data, x = xy$x, y = xy$y)
        }

        # Update variable metadata
        out.grid[["Variable"]] <- list(varName = metadata$code, level = NULL)
        attr(out.grid[["Variable"]], "longname") <- metadata$longname
        attr(out.grid[["Variable"]], "units") <- metadata$units
        
        return(out.grid)
    })
    
    # Combine members if more than one
    out <- if (length(out.list) == 1) {
        out.list[[1]]
    } else {
        do.call("bindGrid", c(out.list, dimension = "member"))
    }

    # Final redimensioning for station data
    if (station) out %<>% redim(drop = FALSE, loc = TRUE, member = FALSE)
    
    message("[", Sys.time(), "] Done")
    return(out)
}


#' @title List all the 9 heat stress indices
#' @description Print a table with a summary of the 9 heat stress indices
#' @return Print a table on the screen with the following columns:
#' \itemize{
#' \item \strong{code}: Code of the index. This is the character string used as input value
#' for the argument \code{index.code} in \code{\link{heatStressIndexGrid}}
#' \item \strong{longname}: Long description of the index
#' \item \strong{indexfun}: The name of the internal function from package \pkg{\link{HeatStress}} used to calculate it
#' \item \strong{tas,dewp,hurs,wind,radiation}: A logical value (0/1) indicating the input variables required for index calculation
#' \item \strong{units}: The units of the index 
#' }
#' @author J. Bedia (original)
#' @export

indexShow <- function() {
    read.master()
}



#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom utils read.table

read.master <- function() {
    system.file("master", package = "HeatStress") %>% read.table(header = TRUE,
                                                                 sep = ";",
                                                                 stringsAsFactors = FALSE,
                                                                 na.strings = "")
}
