#•--------------------------------------------------------------------
# SECTION 1   : COURSE AND ASSIGNMENT DETAILS
# --------------------------------------------------------------------
# Course      : LIS-4317-Week-13
# Assignment  : Final Project
# URL         : http://45.55.78.82:8006/
# Filename    : LIS-4317-Week-13.R
# Purpose     : Analyze pulsar and supernova data with animated visualizations
# Author      : Steven Barden
# Email       : StevenBarden@usf.edu
# Created     : 2025-05-03-2215-00
# Updated     : 2025-05-03-2355-00
# License     : The Stuningly Free Unlicense
# Description : Loads ATNF pulsar and DES supernova data
#             : Creates animated scatter plots and histograms
#             : Outputs visualizations for Visual Peer Review

# --------------------------------------------------------------------
# SECTION 2: SETTINGS or CONFIGUATIONS
# --------------------------------------------------------------------

show_comments <- TRUE
baseDir <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4317\Final\Code)"
supernovaeData <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4317\Final\Code\Data\supernovaeData.csv)"
pulsarData <- r"(C:\Users\Steve\OneDrive\College\_____DESKTOP ICONS\Remeye\Classes\4317\Final\Code\Data\pulsarData.csv)"

# --------------------------------------------------------------------
# SECTION 2: ENVIRONMENT SETUP
# --------------------------------------------------------------------

tryCatch({
  print(paste("Current working directory:", getwd()))
  if (!dir.exists(baseDir)) stop("Directory does not exist: ", baseDir)
  setwd(baseDir)
  print(paste("Working directory successfully set to:", baseDir))
}, error = function(e) {
  stop("Directory setup failed: ", e$message)
})

tryCatch({
  options(width = 80)
}, error = function(e) {
  print("Could not set terminal width.")
})

# --------------------------------------------------------------------
# SECTION 3: DEPENDENCIES & INSTALLATION
# --------------------------------------------------------------------

required_packages <- c("readr", "dplyr", "ggplot2", "plotly", "animation")

tryCatch({
  for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg, dependencies = TRUE)
      if (!require(pkg, character.only = TRUE)) {
        stop("Failed to load package after installation: ", pkg)
      }
      cat("Successfully loaded:", pkg, "\n")
    } else {
      cat("Package already loaded:", pkg, "\n")
    }
  }
}, error = function(e) {
  stop("Library setup failed: ", e$message)
})

# --------------------------------------------------------------------
# SECTION 4: DATA UTILITY FUNCTIONS
# --------------------------------------------------------------------

load_data <- function(filePath) {
  if (show_comments) {
    cat("Attempting to load data from:", filePath, "\n")
  }
  tryCatch({
    data <- read.csv(filePath)
    return(data)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

clean_data <- function(data) {
  if (show_comments) {
    cat("Cleaning data: removing duplicates, handling missing values...\n")
  }
  tryCatch({
    data <- unique(data)
    data <- na.omit(data)
    return(data)
  }, error = function(e) {
    stop("Error cleaning data: ", e$message)
  })
}

transform_data <- function(data) {
  if (show_comments) {
    cat("Applying transformations: mutating, summarizing...\n")
  }
  tryCatch({
    data_transformed <- data %>%
      mutate(OBS_DATE = suppressWarnings(as.numeric(gsub("OBS:,", "", DES)))) %>%
      filter(!is.na(OBS_DATE)) %>%
      group_by(SURVEY.) %>%
      summarize(mean_obs_date = mean(OBS_DATE, na.rm = TRUE))
    return(data_transformed)
  }, error = function(e) {
    stop("Error transforming data: ", e$message)
  })
}

check_missing_values <- function(data) {
  missing_count <- sum(is.na(data))
  if (show_comments) {
    if (missing_count > 0) {
      cat("Warning:", missing_count, "missing values found.\n")
    } else {
      cat("No missing values found in the dataset.\n")
    }
  }
  return(missing_count)
}

summarize_data <- function(data) {
  if (show_comments) {
    cat("Displaying summary statistics and data dimensions...\n")
  }
  tryCatch({
    print(summary(data))
    cat("Dimensions:", dim(data)[1], "rows by", dim(data)[2], "columns\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error summarizing data: ", e$message)
  })
}

aggregate_data <- function(data, group_col, value_col) {
  if (show_comments) {
    cat("Aggregating data by group column:", group_col, "\n")
  }
  tryCatch({
    aggregated_data <- data %>%
      group_by(.data[[group_col]]) %>%
      summarize(result = mean(.data[[value_col]], na.rm = TRUE))
    return(aggregated_data)
  }, error = function(e) {
    stop("Error aggregating data: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 5: DATA I/O HANDLERS
# --------------------------------------------------------------------

read_data_file <- function(filePath, fileType = "csv") {
  tryCatch({
    if (fileType == "csv") {
      data <- read.csv(filePath)
    } else {
      stop("Unsupported file type:", fileType)
    }
    if (show_comments) {
      cat("Successfully read", fileType, "file from", filePath, "\n")
    }
    return(data)
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })
}

load_sample_data <- function() {
  if (show_comments) {
    cat("Creating sample data...\n")
  }
  data <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 20, 15)
  )
  return(data)
}

# --------------------------------------------------------------------
# SECTION 6: DATA PROCESSING WORKFLOWS
# --------------------------------------------------------------------

process_data <- function(data) {
  tryCatch({
    if (show_comments) cat("Processing pipeline started...\n")
    cleaned_data <- clean_data(data)
    transformed_data <- transform_data(cleaned_data)
    check_missing_values(transformed_data)
    if (show_comments) cat("Pipeline completed.\n")
    return(transformed_data)
  }, error = function(e) {
    stop("Error in processing pipeline: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 7: DATABASE OPERATIONS (SQLite Placeholder)
# --------------------------------------------------------------------

connect_sqlite <- function(dbPath) {
  if (show_comments) cat("Connecting to SQLite DB at:", dbPath, "\n")
  db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbPath)
  return(db_connection)
}

create_table <- function(db_connection, schema) { }
insert_record <- function(db_connection, table_name, record_data) { }
read_records <- function(db_connection, query) { }
update_record <- function(db_connection, table_name, condition, new_values) { }
delete_record <- function(db_connection, table_name, condition) { }

# --------------------------------------------------------------------
# SECTION 8: ANALYSIS FUNCTIONS
# --------------------------------------------------------------------

analyze_data <- function(data) {
  if (show_comments) cat("Analyzing data...\n")
  tryCatch({
    analysis_result <- data %>%
      group_by(SURVEY.) %>%
      summarize(mean_obs_date = mean(mean_obs_date, na.rm = TRUE))
    
    if (show_comments) cat("Analysis completed successfully.\n")
    return(analysis_result)
  }, error = function(e) {
    stop("Error during analysis: ", e$message)
  })
}

# --------------------------------------------------------------------
# SECTION 9: VISUALIZATION FUNCTIONS
# --------------------------------------------------------------------


visualize_data <- function(pulsar_data, supernova_data) {
  if (show_comments) cat("Creating visualization...\n")
  tryCatch({
    pulsar_data <- pulsar_data %>%
      mutate(
        P0 = as.numeric(trimws(gsub("[^0-9.e+-]", "", P0))),
        Gl = as.numeric(trimws(gsub("[^0-9.e+-]", "", Gl)))
      ) %>%
      filter(!is.na(P0), P0 > 0, P0 <= 0.006, !is.na(Gl), Gl >= 0, Gl <= 360)
    
    ani.options(interval = 0.5)
    saveGIF({
      for (i in 1:10) {
        # Calculate dynamic x-axis limits for zoom effect
        x_min <- 0.002 + (i-1) * 0.0004  # Start at 0.002, increase by 0.0004 each frame
        x_max <- 0.006 - (i-1) * 0.0002  # Start at 0.006, decrease by 0.0002 each frame
        plot <- ggplot(pulsar_data, aes(x = P0, y = Gl)) +
          geom_point(aes(color = as.factor(i %% 2)), size = 1, alpha = 0.3) +
          scale_x_log10(limits = c(x_min, x_max), breaks = seq(0.002, 0.006, by = 0.002)) +
          scale_y_continuous(limits = c(0, 360)) +
          scale_color_manual(name = "Frame", values = c("red", "blue"), labels = c("Red", "Blue")) +
          theme_minimal() +
          labs(
            title = "Pulsar Spin Period vs. Galactic Longitude (Animated)",
            x = "Spin Period (s, log scale)",
            y = "Galactic Longitude (deg)"
          )
        print(plot)
      }
    }, movie.name = "pulsar_animation.gif")
    
    saveGIF({
      for (i in 1:10) {
        plot <- ggplot(supernova_data, aes(x = mean_obs_date)) +
          geom_histogram(bins = 50, fill = ifelse(i %% 2 == 0, "blue", "red"), alpha = 0.7) +
          scale_x_continuous(limits = c(56500, 56700)) +
          theme_minimal() +
          labs(
            title = "Supernova Observation Date Distribution (Animated)",
            x = "Mean Observation Date (MJD)",
            y = "Count"
          )
        print(plot)
      }
    }, movie.name = "supernova_animation.gif")
    
    if (show_comments) cat("Visualization completed successfully.\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Error creating visualization: ", e$message)
  })
}


# --------------------------------------------------------------------
# SECTION 10: MAIN EXECUTION BLOCK
# --------------------------------------------------------------------

main <- function() {
  if (show_comments) cat("Starting script execution...\n")
  tryCatch({
    if (show_comments) cat("Step 1: Loading data...\n")
    pulsar_raw <- read_data_file(pulsarData)
    supernova_raw <- read_data_file(supernovaeData)
    summarize_data(pulsar_raw)
    summarize_data(supernova_raw)
    
    if (show_comments) cat("Step 2: Processing data...\n")
    supernova_processed <- process_data(supernova_raw)
    
    if (show_comments) cat("Step 3: Analyzing data...\n")
    supernova_analysis <- analyze_data(supernova_processed)
    
    if (show_comments) cat("Step 4: Visualizing results...\n")
    visualize_data(pulsar_raw, supernova_analysis)
    
    if (show_comments) cat("Script execution completed successfully.\n")
    return(invisible(NULL))
  }, error = function(e) {
    stop("Script execution failed: ", e$message)
  })
}

# Call the main function to execute the script
main()
# --------------------------------------------------------------------
# SECTION 11: VERSION HISTORY
# --------------------------------------------------------------------
# Version History:
# - Version 1.0 (2025-05-03-2215-00): Initial template.
# - Version 1.1 (2025-05-03-2230-00): Updated baseDir and file paths.
# - Version 1.2 (2025-05-03-2355-00): Fixed transform_data and analyze_data for missing REDSHIFT_FINAL.

# --------------------------------------------------------------------
# SECTION 12: ADDITIONAL NOTES
# --------------------------------------------------------------------

# Best Practices:
# - Ensure secure handling of API keys and credentials.
# - Keep code modular and organized for maintainability.
# - Validate data inputs to prevent unexpected errors.
# - Use consistent naming conventions for variables and functions.
# - Include appropriate documentation and comments.
# - Test functions with small datasets before full execution.