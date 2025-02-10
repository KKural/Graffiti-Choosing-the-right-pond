# call library for required fonts----- 
library(patchwork)
library(extrafont)
# Load fonts into R
suppressMessages(loadfonts(device = "win"))

# Function to create a folder with a date argument---------
make_folder <- function(date = Sys.Date()) {
  
  # Convert the provided date to "YYYYMMDD" format
  folder_name <- format(as.Date(date), "%Y%m%d")
  
  # Define the full folder name with additional text
  full_folder_name <- paste0(folder_name, "_Output")
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(here::here(full_folder_name))) {
    dir.create(here::here(full_folder_name))
    message("Folder created: ", full_folder_name)
  } else {
    message("Folder already exists: ", full_folder_name)
  }
  
  return(full_folder_name)  # Return the folder name to use later
}

# Create the folder
folder_name <- make_folder()

# Function: quick-and-easy ggplot and png saver ----------------------------------------
ggsave_png <- function(ggp, output = "folder_name", width = 8, height = 6, dpi = 1200, units = "in") {
  ggplot2::ggsave(
    filename = paste0(deparse(substitute(ggp)), ".png", sep = ""),
    device = "png",
    plot = ggp,
    path = output,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    limitsize = TRUE
    
  )
}

# plot function-----------------------------------------------------
# Define a custom theme to be used in ggplot2 visualizations
custom_theme <- ggplot2::theme_minimal() +
  # Use a minimal base theme and add some custom modifications
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA), # Set the plot background to white
    axis.text = ggplot2::element_blank(), # Remove axis text
    axis.ticks = ggplot2::element_blank() # Remove axis ticks
  )


# Define the main plotting function
plot_mhi <- function(permutation_data, observed_data, 
                     graffiti_type,         # Filter value for "Graffiti Types"
                     x_limits,              # Vector: c(xmin, xmax)
                     seg_linewidth = 0.5,   # segment line width for observed MHI line
                     text_size = 1.5,       # text size for annotations
                     observed_y = -5,
                     title = "") {
  
  # Filter permutated and observed data
  permutation <- permutation_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Permutated MHI Value`)
  
  observed_MHI <- observed_data |>
    dplyr::filter(`Log Transformations` == "Log Transformation + 1",
                  `Graffiti Types` == graffiti_type) |>
    dplyr::pull(`Observed MHI Value`)
  
  # Calculate permutated mean
  permutated_mean <- mean(permutation)
  
  # Set bin width and breaks for histogram
  bin_width <- 0.005
  breaks <- seq(min(permutation) - bin_width, max(permutation) + bin_width, by = bin_width)
  
  # Get max y-value from histogram counts (for setting annotation line end)
  max_y <- max(hist(permutation, plot = FALSE, breaks = breaks)$counts) * 1.1
  
  # Create plot
  p <- ggplot2::ggplot(data.frame(permutation), ggplot2::aes(x = permutation)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count)), 
                            fill = "grey", color = "black", binwidth = bin_width) +
    # Observed MHI line
    ggplot2::annotate("segment", x = observed_MHI, xend = observed_MHI, y = 0, yend = max_y, 
                      color = "black", linetype = "dashed", linewidth = seg_linewidth) +
    
    # Observed MHI and permutation Mean labels
    ggplot2::annotate("text", x = observed_MHI, y = observed_y, 
                      label = paste("Observed MHI:", round(observed_MHI, 3)),
                      color = "black", hjust = 0.3, size = text_size, family = "Times New Roman") +
    ggplot2::annotate("text", x = permutated_mean, y = observed_y, 
                      label = sprintf("Permutated Mean MHI: %.3f", permutated_mean),
                      color = "black", hjust = 0.5, size = text_size, family = "Times New Roman") +
    
    # Point at permutated mean
    ggplot2::annotate("point", x = permutated_mean, y = 0, color = "black", size = 0.5) +
    
    # Set x-axis limits, title, labels, and apply custom theme
    ggplot2::xlim(x_limits[1], x_limits[2]) +
    ggplot2::labs(title = title, x = "MHI Value", y = "Frequency") +
    ggplot2::theme(
      legend.position = "none",  # Remove legend
      plot.background = ggplot2::element_rect(fill = "white", color = NA),  # White background
      text = ggplot2::element_text(family = "Times New Roman", size = 5),  # Times New Roman font for all text
      axis.title = ggplot2::element_text(size = 5, family = "Times New Roman", face = "bold"),  # Bold axis titles
      axis.text = ggplot2::element_text(size = 5, family = "Times New Roman"),  # Consistent axis text font size
      axis.line = ggplot2::element_line(color = "black"),  # Black lines for x and y axes
      plot.title = ggplot2::element_text(family = "Times New Roman", size = 5, face = "bold")  # Bold, centered title
    )
  
  return(p)
}

# A helper: if a value is NULL, use the default from the main function.
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# Compute MHI for multiple transformations
compute_MHI <- function(data_matrix) {
  transformations <- list(
    "No Transformation" = data_matrix,
    "Square Root Transformation" = sqrt(data_matrix),
    "Log Transformation + 0.5" = log(data_matrix + 0.5),
    "Log Transformation + 1" = log1p(data_matrix)  # log1p(x) = log(x + 1)
  )
  
  transformations <- purrr::map(transformations, ~ {
    transformed_data <- .x
    transformed_data[transformed_data < 0] <- 0  # Set negatives to zero
    transformed_data
  })
  # Compute MHI for each transformation
  MHI_matrices <- purrr::map(transformations, ~ {
    MHI_matrix <- 1 - as.matrix(vegan::vegdist(t(.x), method = "horn"))
    list(matrix = round(MHI_matrix, 3))
  })
  
  return(MHI_matrices)
}

# Randomize function------------------------------------------------------------
# fix_street_segment = TRUE, fix_graffiti_types = FALSE is street segment remains unchanges
# graffiti types are shuffled randomly
randomize <- function(df, fix_street_segment = TRUE, fix_graffiti_types = FALSE) {
  df |>
    dplyr::mutate(
      graffiti_types = sample(graffiti_types, replace = !fix_graffiti_types),
      street_segment = sample(street_segment, replace = !fix_street_segment)
    )
}


# Function to permutate MHI under different transformations
permutate_MHI_dataset <- function(data, n_iterations) {
  
  permutations <- purrr::map(1:n_iterations, ~ {
    
    # Randomize graffiti types but keep street segments fixed
    randomized_data <- randomize(data, fix_street_segment = TRUE, fix_graffiti_types = FALSE)
    
    # Compute graffiti counts per street segment
    counts <- randomized_data |>
      dplyr::group_by(street_segment, graffiti_types) |>
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') |>
      tidyr::complete(street_segment, graffiti_types, fill = list(count = 0))
    
    # Convert to wide format for MHI calculation
    counts_wide <- counts |>
      tidyr::pivot_wider(names_from = graffiti_types, values_from = count, values_fill = 0)
    
    # Convert to matrix
    data_matrix_sim <- as.matrix(counts_wide[, -1])
    rownames(data_matrix_sim) <- counts_wide$street_segment
    data_matrix_sim[data_matrix_sim < 0] <- 0
    
    # Compute MHIs for all transformations
    MHI_transformed <- compute_MHI(data_matrix_sim)
    
    # Convert each transformation's MHI matrix into a data frame
    MHI_data <- purrr::map_df(names(MHI_transformed), function(transformation) {
      as.data.frame(as.table(MHI_transformed[[transformation]]$matrix)) |>
        dplyr::filter(Var1 != Var2) |>
        dplyr::transmute(
          `Log Transformations` = transformation,
          `Graffiti Types` = paste(Var1, "vs", Var2),
          `Permutated MHI Value` = round(Freq, 3)
        )
    })
    
    return(MHI_data)
  })
  
  # Combine all iterations into one data frame
  permutations_df <- dplyr::bind_rows(permutations, .id = 'iteration')
  return(permutations_df)
}

# Function to calculate p-value
calculate_p_value <- function(simulated_data, observed_value) {
  proportion <- sum(simulated_data <= observed_value) / length(simulated_data)
  return(round(proportion, 3))
}