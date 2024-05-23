#' Function to simulate ecological data
#'
#' This function creates a simulated dataset representing ecological observations
#' over a specified spatial and temporal range. It allows for the generation of species
#' occurrence data along with environmental variables across a defined spatial grid.
#' Optionally, the grid can be saved to the global environment.
#'
#' @import terra
#' @import ggplot2
#' @importFrom dplyr filter select mutate
#' @param startDate The starting date for the simulated data in "YYYY-MM-DD" format (default "2020-01-01").
#' @param endDate The ending date for the simulated data in "YYYY-MM-DD" format (default "2020-12-31").
#' @param yMin The minimum latitude (southern limit) of the spatial extent (default -25.7).
#' @param yMax The maximum latitude (northern limit) of the spatial extent (default -22.3).
#' @param xMin The minimum longitude (western limit) of the spatial extent (default 30.7).
#' @param xMax The maximum longitude (eastern limit) of the spatial extent (default 32.2).
#' @param speciesList A vector of species names to be included in the simulation (default c("species_1" to "species_10")).
#' @param classList A vector indicating the classification of each species as either "alien" or "native" (default c("alien", "native")).
#' @param envVars The number of environmental variables to simulate (default 4).
#' @param spatialRes The spatial resolution in kilometers (default 10 km).
#' @param returnOption An integer that determines the type of output:
#'   - 1 for a summary table of species by site,
#'   - 2 for a SpatRaster object with the full dataset,
#'   - 3 for a distance matrix of sites.
#'   Other options can be added as needed.
#' @param plot A logical indicating whether to plot the results; `TRUE` generates a plot of species counts across the spatial grid (default FALSE).
#' @param globalSave A logical indicating whether to save the grid data to the global environment; `TRUE` saves the grid to .GlobalEnv (default FALSE).
#'
#' @return Depending on the `returnOption` parameter, the function can return:
#'   - A summary table (data.frame) of species occurrences by site.
#'   - A `SpatRaster` object representing the full dataset.
#'   - A distance matrix of sites.
#' If `plot` is TRUE, a plot will be generated for visualizing the distribution of species counts.
#'
#' @export
#'
#' @examples
#' set.seed(123)  # For reproducibility
#' simData_df = simData(returnOption = 1, plot = TRUE)
#' head(simData_df)
#' # To check the grid data saved to global environment
#' head(simData_grid)

# Function to simulate ecological data and plot
simData = function(startDate = "2020-01-01", endDate = "2020-12-31",
                    yMin = -25.7, yMax = -22.3, xMin = 30.7, xMax = 32.2,
                    speciesList = paste0('species_', 1:10), classList = c("alien", "native"),
                    envVars = 4, spatialRes = 10, returnOption = 1, plot = FALSE,
                    globalSave = TRUE) {  # Default for globalSave added here
  # Define temporal range
  dates = seq(as.Date(startDate), as.Date(endDate), by = "day")

  # Create a spatial grid
  xCoords = seq(xMin, xMax, by = spatialRes / 111.32)  # Approx. conversion from km to degrees
  yCoords = seq(yMin, yMax, by = spatialRes / 111.32)
  grid = expand.grid(x = xCoords, y = yCoords, time = dates)

  # Simulate species data
  numRecords = nrow(grid)
  grid$species = sample(speciesList, numRecords, replace = TRUE)
  grid$count = sample(1:100, numRecords, replace = TRUE)
  grid$class = sample(classList, numRecords, replace = TRUE, prob = c(0.2, 0.8))

  # Simulate environmental variables
  for (i in 1:envVars) {
    grid[[paste0("envVar", i)]] = rnorm(numRecords)
  }

  # Generate a unique siteID from x and y coordinates
  grid$siteID = as.factor(paste0("Site_", as.integer(factor(grid$x)), "_", as.integer(factor(grid$y))))

  # Create SpatRaster from the simulated data
  r = terra::rast(nrow = length(yCoords), ncol = length(xCoords), vals = 1:numRecords)
  terra::ext(r) = terra::ext(xMin, xMax, yMin, yMax)
  terra::res(r) = c(spatialRes / 111.32, spatialRes / 111.32)
  terra::crs(r) = "EPSG:4326"
  terra::values(r) = grid$count  # Example to fill with counts, adjust as needed

  # if (numRecords == ncell(r)) {
  #   terra::values(r) = grid$count  # Ensure this matches the number of cells
  # } else {
  #   stop("Mismatch in the number of grid values and raster cells.")
  # }

  # Determine the output based on returnOption
  output = switch(returnOption,
                   `1` = tapply(grid$count, grid[, c("siteID", "species")], sum),  # site by species
                   `2` = r,  # full SpatRaster object, could further process as needed
                   `3` = dist(r)  # Calculates distance matrix if appropriate
                   # Add more options as necessary
  )

  # Plotting if requested
  if (plot) {
    if (returnOption == 1) {
      # Use ggplot2 for visualizing species count across the spatial grid
      p = ggplot(grid, aes(x = x, y = y, fill = count)) +
        geom_tile() +
        facet_wrap(~species) +
        scale_fill_viridis_c() +
        theme_minimal() +
        labs(title = "Species count across spatial grid", x = "Longitude", y = "Latitude")
      print(p)
    } else if (returnOption == 2) {
      # Use terra's plot method for visualizing the SpatRaster object
      plot(r, main = "Raster Map of Species Counts")
    }
  }

  # Conditionally save the grid to the global environment
  if (globalSave) {
    .GlobalEnv$simData_grid = grid
  }

  return(output)
}
