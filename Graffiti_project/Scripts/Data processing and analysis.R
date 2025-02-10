rm(list = ls())

# load the fucntions file 
source(here::here("Scripts", "Functions.R"))

# Read the data ------------------
df_graffiti_data_clean <- read.csv(here::here("./data/data_anonymized.csv")) |>
  # select only the required columns
  dplyr::select(street_segment, observer_id, graffiti_types)

unique_observers <- dplyr::n_distinct(df_graffiti_data_clean$observer_id)|>
  print()

# Summarize graffiti presence per street segment into three categories
graffiti_status <- df_graffiti_data_clean |>
  dplyr::group_by(street_segment) |>
  dplyr::summarize(
    no_graffiti = all(graffiti_types == "No graffiti"),  # "No graffiti"
    only_others = all(graffiti_types == "Others"),  # Segments that only have "Others" graffiti
    has_graffiti = any(graffiti_types != "No graffiti" & graffiti_types != "Others"),  # Graffiti present but excluding "Others"
    .groups = "drop"
  )


# Read, process, and merge graffiti status into street segment data 
sf_ghent_street_segments <- dplyr::bind_rows(
  sf::st_read(dsn = here::here("Data", "WBN3.shp")),
  sf::st_read(dsn = here::here("Data", "BRUGGEN 2.shp"))
) |>
  # Select relevant columns and exclude unnecessary ones
  dplyr::select(-TYPE, -OPNDATUM, -VORM, -LBLVORM) |>
  # Translate and convert segment types directly to factors
  dplyr::mutate(
    LBLTYPE = factor(
      dplyr::case_match(
        LBLTYPE,
        "kruispuntzone" ~ "intersection",
        "wegsegment" ~ "street segment",
        "overbrugging" ~ "bridge",
        .default = NA_character_
      ),
      levels = c("street segment", "intersection", "bridge") # Set factor levels directly
    )
  ) |>
  # Merge graffiti status data
  dplyr::left_join(graffiti_status, by = c("UIDN" = "street_segment")) |>
  # Categorize graffiti presence
  dplyr::mutate(
    observed_status = dplyr::case_when(
      has_graffiti ~ "At least with one graffiti",
      only_others ~ "Only Others",
      no_graffiti ~ "Zero graffiti",
      TRUE ~ "Not observed"
    )
  ) |>
  # Drop temporary columns
  dplyr::select(-no_graffiti, -has_graffiti, -only_others)

# Create a ggplot2 map visualization of the Ghent street segments
Appendix_2 <- ggplot2::ggplot() +
  # Use the spatial dataframe to plot each segment with its corresponding type color
  ggplot2::geom_sf(data = sf_ghent_street_segments, ggplot2::aes(fill = LBLTYPE)) +
  ggplot2::scale_fill_manual(
    # Manually specify the colors for each segment type
    values = c(
      "street segment" = "grey60",
      "intersection" = "grey40",
      "bridge" = "grey20"
    ),
    labels = c("Street segment", "Intersection", "Bridge"), # Rename legend labels
    name = "Segment type" # Legend title
  ) +
  # Apply the custom theme defined earlier
  custom_theme  + 
  ggplot2::theme(
    # Ensure all text is in Times New Roman
    text = ggplot2::element_text(family = "Times New Roman"),
    
      # Plot title
    plot.title = ggplot2::element_text(size = 14, family = "Times New Roman", face = "bold", hjust = 0.5),
    
    # Legend text and title
    legend.text = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.title = ggplot2::element_text(size = 12, family = "Times New Roman", face = "bold"),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.1, 0.1),
    legend.justification = c(0, 0),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) +
  # Add a scale bar at the bottom left
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)  

Appendix_2

# save the files
ggsave_png(Appendix_2, output = folder_name)

# Count categories for legend
unique_observers <- dplyr::n_distinct(df_graffiti_data_clean$observer_id)
count_total_segments <- nrow(sf_ghent_street_segments)  # Total number of segments (2392)
count_segments_with_six_graffiti <- sum(sf_ghent_street_segments$observed_status == "At least with one graffiti", na.rm = TRUE)
count_only_others <- sum(sf_ghent_street_segments$observed_status == "Only Others", na.rm = TRUE)
count_zero_graffiti <- sum(sf_ghent_street_segments$observed_status == "Zero graffiti", na.rm = TRUE)
count_observed_segments <- count_segments_with_six_graffiti + count_only_others + count_zero_graffiti
count_not_observed <- sum(sf_ghent_street_segments$observed_status == "Not observed", na.rm = TRUE)
print(table(sf_ghent_street_segments$observed_status))

# plot the observed and not observed street segments
Appendix_3 <- ggplot2::ggplot(sf_ghent_street_segments) +
  ggplot2::geom_sf(ggplot2::aes(fill = observed_status)) +
  ggplot2::scale_fill_manual(
    values = c(
      "At least with one graffiti" = "black",
      "Only Others" = "grey30",
      "Not observed" = "grey70",
      "Zero graffiti" = "grey95"
    ),
    name = "Street segment",
    labels = c(
      base::paste0("With classified graffiti (", count_segments_with_six_graffiti, ")"),
      base::paste0("With only 'Other' graffiti (", count_only_others, ")"),
      base::paste0("With no Graffiti (", count_zero_graffiti, ")"),
      base::paste0("Not observed (", count_not_observed, ")") 
    )
  ) +
  ggplot2::labs(title = "Appendix 3: Overview of Graffiti Observation (N = 2392)") + 
  custom_theme + 
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 10, family = "Times New Roman"),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.2, 0.2),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent"),
    text = ggplot2::element_text(family = "Times New Roman", size = 12),
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

print(Appendix_3)

ggsave_png(Appendix_3, output = folder_name)

# Calculate for each street segment the number of
#   graffiti items per graffiti type, excluding
df_graffiti_summary  <- df_graffiti_data_clean |> 
  dplyr::filter(graffiti_types != "No graffiti") |>
  janitor::tabyl(graffiti_types) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1)|>
  dplyr::arrange(n)
df_graffiti_summary

# Drop others and No graffiti street segments
df_graffiti_data_final <- df_graffiti_data_clean |> 
  dplyr::filter(graffiti_types != "Others",
                graffiti_types != "No graffiti") 

# Calculate for each street segment the number of
#   graffiti items per graffiti type, excluding others
df_graffiti_summary_no_others  <- df_graffiti_data_final |> 
  janitor::tabyl(graffiti_types) |>
  janitor::adorn_totals("row") |>
  janitor::adorn_pct_formatting(digits = 1)|>
  dplyr::arrange(n)
df_graffiti_summary_no_others


# Summarize graffiti counts per street segment and graffiti type
df_graffiti_summary_stats <- df_graffiti_data_final |> 
   dplyr::group_by(street_segment, graffiti_types) |> 
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |> 
  dplyr::ungroup() |>
  tidyr::complete(street_segment, graffiti_types, fill = list(count = 0)) |>  
  dplyr::group_by(graffiti_types) |> 
  
  # min, max, mean, sd, and total per graffiti type
  dplyr::summarize(
    min = min(count),     
    max = max(count),     
    mean = round(mean(count), 2), 
    sd = round(sd(count), 2),     
    total = sum(count),
    .groups = "drop"
  ) |>
  
  # total count for all graffiti types
  dplyr::mutate(
    percentage = round((total / sum(total)) * 100, 2)  
  ) |>
  
  dplyr::arrange(total)  # Sort by total count

print(df_graffiti_summary_stats)



df_street_segment_counts <-
  df_graffiti_data_final |>
  # Aggregate by segment and graffiti type
  dplyr::group_by(street_segment, graffiti_types) |>
  dplyr::summarize(count = dplyr::n(), .groups = "drop") |> 
  # The next function inserts a zero-count
  #   row for unobserved graffiti types
  tidyr::complete(street_segment, graffiti_types, 
                  fill = list(count = 0))

head(df_street_segment_counts)

# Create data frame in which street segments are in rows
#   and graffiti counts in columns
df_street_segment_counts_wide <-
  df_street_segment_counts |>
  # covert to wide format with segments in rows and graffiti types in columns
  tidyr::pivot_wider(names_from = "graffiti_types", 
                     values_from = "count")|>
  # convert street segment column to numeric
  dplyr::mutate(street_segment = as.numeric(street_segment))

head(df_street_segment_counts_wide)


# Calculate MH Index---------------------------
# Prepare the data matrix
data_matrix_observed <- as.matrix(df_street_segment_counts_wide[,-1])
rownames(data_matrix_observed) <- df_street_segment_counts_wide$street_segment


# Define target comparisons and transformation order
target_comparisons <- c(
  "Masterpiece vs Tag",
  "Masterpiece vs SITS",
  "Tag vs SITS"
)

transformation_order <- c("No Transformation", "Square Root Transformation", 
                          "Log Transformation + 0.5", "Log Transformation + 1")

# Compute observed MHI values for all transformations
observed_MHI <- compute_MHI(data_matrix_observed)

df_observed_MHI <- dplyr::bind_rows(
  lapply(names(observed_MHI), function(name) {
    as.data.frame(as.table(observed_MHI[[name]]$matrix)) |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::transmute(
        `Log Transformations` = name,  # Assign transformation name
        `Graffiti Types` = paste(Var1, "vs", Var2),
        `Observed MHI Value` = round(Freq, 3)
      ) |>
      dplyr::filter(`Graffiti Types` %in% target_comparisons)  # Keep only target comparisons
  }), .id = NULL) |>
  dplyr::mutate(
    `Log Transformations` = factor(`Log Transformations`, 
                                   levels = transformation_order),
    # Factor `Graffiti Types` according to the custom order:
    `Graffiti Types` = factor(`Graffiti Types`, 
                              levels = target_comparisons)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)

print(df_observed_MHI)


# Define the file path for transformed MHI Permutation
permutation_file <- here::here(folder_name, "df_permutated_MHI.rds")

# Check if the file exists to avoid unnecessary re-runs
if (file.exists(permutation_file)) {
  # Load existing permutation results
  permutated_MHI <- readRDS(permutation_file)
  message("permutation data loaded from ", permutation_file)
  
} else {
  # If file does not exist, run the permutation
  set.seed(1234)  # Ensure reproducibility
  n_iterations <- 1000
  
  # Simulate dataset using the transformation-aware function
  permutated_MHI <- permutate_MHI_dataset(df_graffiti_data_final, n_iterations)
  
  # Save the permutation results to an RDS file inside the output folder
  saveRDS(permutated_MHI, permutation_file)
  message("permutation completed and saved to ", permutation_file)
}


# Compute summary statistics for permutated data
df_permutation_summary <- permutated_MHI |>
  dplyr::group_by(`Log Transformations`, `Graffiti Types`) |>
  dplyr::summarise(
    `Mean Permutated MHI` = round(mean(`Permutated MHI Value`), 3),
    `SD Permutated MHI` = round(sd(`Permutated MHI Value`), 3),
    .groups = "drop"
  )

print(df_permutation_summary)

## Merge observed MHI values with Permutated summary
df_combined <- df_observed_MHI |>
  dplyr::left_join(permutated_MHI, by = c("Log Transformations", "Graffiti Types")) |>
  dplyr::group_by(`Log Transformations`, `Graffiti Types`) |>
  dplyr::summarize(
    `Observed MHI Value` = dplyr::first(`Observed MHI Value`),  # Get observed MHI
    `Mean Permutated MHI` = round(mean(`Permutated MHI Value`, na.rm = TRUE), 3),
    `SD Permutated MHI` = round(sd(`Permutated MHI Value`, na.rm = TRUE), 3),
    `P Value` = calculate_p_value(`Permutated MHI Value`, dplyr::first(`Observed MHI Value`)), # Compute p-value
    .groups = "drop"
  ) |>
  dplyr::mutate(
    `P Value` = dplyr::case_when(
      is.na(`P Value`) ~ "N/A",
      `P Value` == 0 ~ "< 0.001",
      TRUE ~ as.character(`P Value`)
    ),
    # Ensure order matches df_observed_MHI
    `Log Transformations` = factor(`Log Transformations`, levels = transformation_order),
    `Graffiti Types` = factor(`Graffiti Types`, levels = target_comparisons)
  ) |>
  dplyr::arrange(`Log Transformations`, `Graffiti Types`)  # Arrange to match df_observed_MHI

print(df_combined)


# Plot histogram------------------------
MHI_plot_1 <- plot_mhi(
  permutation_data = permutated_MHI,
  observed_data = df_observed_MHI,
  graffiti_type = "Masterpiece vs Tag",
  x_limits = c(0.1, 0.32),
  title = "Masterpiece vs Tags",
  observed_y = -8,
)
print(MHI_plot_1)
ggsave_png(MHI_plot_1, output = folder_name)

MHI_plot_2 <- plot_mhi(
  permutation_data = permutated_MHI,
  observed_data = df_observed_MHI,
  graffiti_type = "Masterpiece vs SITS",
  x_limits = c(0.15, 0.39),
  title = "Masterpiece vs SITSs",
  observed_y = -5,
)
print(MHI_plot_2)
ggsave_png(MHI_plot_2, output = folder_name)


MHI_plot_3 <- plot_mhi(
  permutation_data = permutated_MHI,
  observed_data = df_observed_MHI,
  graffiti_type = "Tag vs SITS",
  x_limits = c(0.675, 0.84),
  title = "Tags vs SITSs",
  observed_y = -13,
)
print(MHI_plot_3)
ggsave_png(MHI_plot_3, output = folder_name)

Figure_1 <- 
  MHI_plot_1 + MHI_plot_2 + MHI_plot_3 +
  plot_layout(ncol = 1)

Figure_1 

ggsave_png(Figure_1, output = folder_name, 
           width = 8, 
           height = 12, units = "cm")

# Combine all results in on excel file----------------------------- 
# create list of data frames to be imported in the manuscript text using R markdown file
observation_details <- data.frame(
  Label = c(
    "Total Observers",
    "Total Street Segments",
    "Observed Street Segments",
    "Not Observed Street Segments",
    "Zero Graffiti Street Segments",
    "Street Segments with at least One of the Six Graffiti Types",
    "Street Segments Only with Other"
  ),
  Count = c(
    unique_observers,
    count_total_segments,
    count_observed_segments,
    count_not_observed,
    count_zero_graffiti,
    count_segments_with_six_graffiti,
    count_only_others
    
  )
)

# Save all dataframes into a list
all_results <- list(
  "Observation Details" = observation_details,
  "Raw Graffiti Summary" = df_graffiti_summary,
  "Graffiti Stats" = df_graffiti_summary_stats,
  "Observed and Permutated values" = df_combined
)

file_path_all_results <- here::here(folder_name, "all_results.xlsx")
writexl::write_xlsx(all_results, file_path_all_results)
