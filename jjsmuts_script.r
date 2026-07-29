##########################################################################################################################################################################################################################################################################
pacman::p_load(dplyr, ggplot2, tidyr, berdata, econdatar, logger, here, forcats)

sarb_palette <- c(
  headline = "#1B2A4A",
  core     = "#B8860B",
  foodnab  = "#8C2D2D",
  fuel     = "#3D6B72",
  grid     = "#8A8580",
  bg       = "#F7F5F0"
)

sarb_theme <- theme_minimal(base_family = "Georgia") +
  theme(
    # backgrounds
    plot.background    = element_rect(fill = "#F7F6F2", color = NA),
    panel.background   = element_rect(fill = "#F7F6F2", color = NA),

    # grid
    panel.grid.major.y = element_line(color = "#E2E0DA", linewidth = 0.6),
    panel.grid.minor.y = element_line(color = "#E2E0DA", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    # axis
    axis.line.x        = element_line(color = "#BFBDB5", linewidth = 0.5),
    axis.ticks.x       = element_line(color = "#BFBDB5", linewidth = 0.4),
    axis.text          = element_text(color = "#5F5E5A", size = 10,
                                      family = "Georgia"),
    axis.title         = element_text(color = "#2C2C2A", size = 12,
                                      family = "Georgia", face = "bold.italic"),

    # titles
    plot.title         = element_text(color = "black", size = 16,
                                      family = "Georgia", face = "bold",
                                      margin = margin(b = 4)),
    plot.subtitle       = element_text(color = "dimgrey", size = 8, 
                                      family = "Georgia", hjust = 0, 
                                      margin = margin(t = 10), face = "bold"), 
    plot.caption       = element_text(color = "dimgrey", size = 8, 
                                      family = "Georgia", hjust = 0), 

    # legend
    legend.position    = "right",
    legend.justification = "center",
    legend.direction = "vertical",
    legend.text        = element_text(color = "#2C2C2A", size = 12,
                                      family = "Georgia"),
    legend.title       = element_blank(),
    legend.key.width   = unit(0.5, "cm"),
    legend.key.height  = unit(0.35, "cm"),
    legend.key.spacing.y = unit(0.6,"cm"),
    legend.background  = element_rect(fill = "#F7F6F2", color = NA),
    legend.margin      = margin(0, 0, 4, 0),

    # margins
    plot.margin        = margin(16, 20, 12, 14)
  )

##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 1: KEY SERIES PLOT ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################

df1 <- tibble(date_col = as.Date(character()))


series <- list(codes = c("CPS00000", "CPS00014", "CPS01000",  "CPS00007"), 
                cols = c("headline", "core", "foodnab", "services"),
                names = c("Headline", "Core", "Services"))

for (i in 1:length(series$codes)){

    code_append <- series$codes[i]
    tscode <- paste0("P0141-", code_append)
    var_code <- series$cols[i]
    yoy <- paste0(var_code, "_yoy")

    temp_df <- get_data(time_series_code = tscode,
        output_format = "codes") %>%
        dplyr::rename(!!var_code := .data[[tscode]]) %>%
        dplyr::mutate(!!yoy := (.data[[var_code]] - lag(.data[[var_code]], n = 12)) / lag(.data[[var_code]], n = 12) * 100) 

    df1 <- full_join(df1, temp_df, by = "date_col")
}


sarb_target <- tibble(date_col = seq(as.Date("2025-01-01"), 
                                        as.Date(max(plot1_df$date_col)), 
                                        by = "month")) %>%
                mutate(target1 = ifelse(date_col >= as.Date("2025-11-01"), 3, NA),
                        target2 = ifelse(date_col < as.Date("2025-11-01"), 4.5, NA)) 

df1 <- left_join(df1, sarb_target, by = "date_col")

df1_plot <- df1 %>%
        dplyr::select(date_col, dplyr::matches("^target"), dplyr::matches("_yoy$")) %>%
        pivot_longer(-date_col) %>%
        filter(date_col > as.Date("2025-05-01")) 


plot1 <- ggplot() +
    geom_line(data = df1_plot %>% filter(!(name %in% c("target1", "target2", "fuel_yoy"))), 
                aes(x = date_col, y = value, colour = fct_reorder2(name, date_col, value, .desc = TRUE)), linewidth = 1) +
    scale_colour_manual(values = c("headline_yoy" = "#1B2A4A", 
                                    "core_yoy" = "#B8860B",
                                    "foodnab_yoy"  = "#8C2D2D",
                                    "services_yoy"     = "#3D6B72"), 
                        labels = c("headline_yoy" = "Headline CPI", 
                                    "core_yoy" = "Core CPI", 
                                    "foodnab_yoy" = "Food & Non-\nAlcoholic Beverages",
                                    "services_yoy" = "Services")) + 
    geom_line(data = df1_plot %>% filter(name == "target1"), aes(x = date_col, y = value),
          linetype = "dashed", 
          colour = "black", 
          linewidth = 0.8) +
    geom_line(data = df1_plot %>% filter(name == "target2"), aes(x = date_col, y = value),
          linetype = "dashed", 
          colour = "black", 
          linewidth = 0.8, na.rm = ) +
    scale_y_continuous(limits = c(-1, 6),
                        breaks = seq(-1, 6, by = 2), 
                        minor_breaks = seq(-1, 6, by = 1), 
                        expand = c(0, 0)) +
    scale_x_date(date_labels = "%m-%Y", 
                date_breaks = "1 months",
                limits = c(as.Date("2026-01-01"), max(plot1_df$date_col)), expand = c(0, 0)) +
  annotate("text", 
             x = as.Date("2026-01-01"), y = 2.7, 
             label = "MPC target", 
             colour = "grey40", family = "Georgia", size = 3.5, 
             fontface = "italic", hjust = 0) +
  geom_hline(yintercept = 0, 
            colour = "black",
            linewidth = 0.4) +
  labs(title = "Selected Consumer Price Series Changes, January - June 2026",
  caption = "Source: BER, Statistics South Africa\nNote: Rates calculated as year-on-year changes.", 
  y = "% Change (Y-o-Y)", 
  x = "") +
  sarb_theme 

ggsave(file = here::here("select_rates.png"), plot = plot1, width = 10, height = 6, dpi = 300)



##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 2: FUEL PLOT ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################

##Disaggregated Fuel and lubricants data not available from BER, donwloaded from StatsSA


df2 <- tibble(date_col = as.Date(character()))

fuel_series <- list(codes = c("CPS07221"), 
                cols = c("fuel"))

for (i in 1:length(fuel_series$codes)){

    code_append <- fuel_series$codes[i]
    tscode <- paste0("P0141-", code_append)
    var_code <- fuel_series$cols[i]
    mom <- paste0(var_code, "_mom")

    temp_df <- get_data(time_series_code = tscode,
        output_format = "codes") %>%
        dplyr::rename(!!var_code := .data[[tscode]]) %>%
        dplyr::mutate(!!mom := (.data[[var_code]] - lag(.data[[var_code]], n = 1)) / lag(.data[[var_code]], n = 1) * 100) 

    df2 <- full_join(df2, temp_df, by = "date_col")
}


fuel_series_xl <- list(codes = c("07221101", "07222101", "07224101"), 
                cols = c("petrol", "diesel", "car_lubr"))

xl_df <- readxl::read_excel(here("coicop.xlsx")) %>%
    dplyr::select(c(`Eight digit code`, matches("^M20"))) %>%
    dplyr::rename(digit = `Eight digit code`) %>%
    dplyr::filter(digit %in% c("07221101", "07222101", "07224101")) %>%
    pivot_longer(cols = matches("^M20"), names_to = "date_col", values_to = "value") %>%
    pivot_wider(names_from = digit, values_from = value) %>%
    dplyr::mutate(date_col = as.Date(paste0(stringr::str_sub(date_col, 2), "01"), format = "%Y%m%d")) %>%
    dplyr::rename("petrol" = "07221101", "diesel" = "07222101", "car_lubr" = "07224101") 

for (i in 1:length(fuel_series_xl$codes)){

    var_code <- fuel_series_xl$cols[i]
    mom <- paste0(var_code, "_mom")

    xl_df <- xl_df %>%
        dplyr::mutate(!!mom := (.data[[var_code]] - lag(.data[[var_code]], n = 1)) / lag(.data[[var_code]], n = 1) * 100) 
}


df2 <- left_join(df2, xl_df, by = "date_col")


df2_plot <- df2 %>%
        dplyr::select(date_col, dplyr::matches("_mom$")) %>%
        pivot_longer(-date_col) %>%
        filter(date_col > as.Date("2025-05-01"), name != "car_lubr_mom") 

df2_plot <- df2_plot %>%
    arrange(desc(date_col)) %>%
    filter(date_col > as.Date("2025-12-01")) %>%
    mutate(month_label = format(date_col, "%m-%Y"))

plot2 <- ggplot() +
    geom_col(data = df2_plot, 
                aes(x = month_label, y = value, fill = fct_reorder2(name, date_col, value, .desc = TRUE)),
                position = position_dodge2(width = 20, preserve = "single")) +
    scale_fill_manual(values = c("fuel_mom" = "#1B2A4A", 
                                    "petrol_mom" = "#B8860B",
                                    "diesel_mom"  = "#8C2D2D"), 
                        labels = c("fuel_mom" = "Fuel and Lubricants", 
                                    "petrol_mom" = "Petrol", 
                                    "diesel_mom" = "Diesel")) + 
    scale_y_continuous(breaks = seq(-10, 40, by = 10), 
                         #minor_breaks = seq(-8, 40, by = ), 
                         expand = c(0.05, 0.05)) +
    # scale_x_(date_labels = "%m-%Y", 
    #             date_breaks = "1 months",
    #             limits = c(as.Date("2026-01-01"), max(plot1_df$date_col)), expand = c(0, 0)) +
  labs(title = "Fuel and Lubricants Price Changes, January - June 2026",
  caption = "Source: BER, Statistics South Africa\nNote: Rates calculated as month-on-month changes.", 
  y = "% Change (M-o-M)", 
  x = "") +
  sarb_theme 

ggsave(file = here::here("fuel.png"), plot = plot2, width = 10, height = 6, dpi = 300)


##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 2: FUEL PLOT ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################

##Retrieve weights from https://www.statssa.gov.za/cpi/documents/CPI%20Basket%20and%20weights%20update%20280125.pdf , 
## and codes from https://dataguide.beranalytics.co.za/datasets/statssa/chapters/prices.html#cpi-analytical-series
## prompted claude to create a df


cpi_contribution <- tibble(
  category = c(
    "Food and non-alcoholic beverages",
    "Alcoholic beverages and tobacco",
    "Clothing and footwear",
    "Housing and utilities",
    "Furnishings, household equipment and maintenance",
    "Health",
    "Transport",
    "Information and communication",
    "Recreation, sport and culture",
    "Education services",
    "Restaurants and accommodation services",
    "Insurance and financial services",
    "Personal care and miscellaneous services"
  ),
  code = c(
    "CPS01000",
    "CPS02000",
    "CPS03000",
    "CPS04000",
    "CPS05000",
    "CPS06000",
    "CPS07000",
    "CPS08000",
    "CPS09000",
    "CPS10000",
    "CPS11000",
    "CPS12000",
    "CPS13000"
  ),
  weight_2023 = c(18.23, 4.64, 3.90, 24.10, 3.33, 1.78, 13.89, 5.47, 2.94, 2.41, 6.12, 10.41, 2.78)
)

apr_jun <- list()

for (i in 1:length(cpi_contribution$code)){

    code_append <- cpi_contribution$code[i]
    tscode <- paste0("P0141-", code_append)
    yoy <- paste0(code_append, "_yoy")

    print(paste0("Processing ", code_append))

    temp_df <- get_data(time_series_code = tscode,
        output_format = "codes") %>%
        dplyr::rename(!!code_append := .data[[tscode]]) %>%
        dplyr::mutate(value = (.data[[code_append]] - lag(.data[[code_append]], n = 12)) / lag(.data[[code_append]], n = 12) * 100) %>%
        dplyr::filter(date_col >= as.Date("2026-04-01")) %>%
        dplyr::mutate(code = code_append, 
                    date_col = lubridate::month(date_col, label = TRUE, abbr = TRUE)) %>%
        dplyr::select(!code_append)

    apr_jun[[i]] <- temp_df
}   


apr_jun <- dplyr::bind_rows(apr_jun) 

apr_jun <- apr_jun %>%
    pivot_wider(names_from = date_col, values_from = value)


cpi_contribution <- left_join(cpi_contribution, apr_jun, by = "code") 

cpi_contribution <- cpi_contribution %>%
        dplyr::select(!code) %>%
        dplyr::rename("Basket Weight" = "weight_2023", 
                        "Category" = "category") %>%
        arrange(desc(`Basket Weight`))

writexl::write_xlsx(cpi_contribution, here::here("categories.xlsx"))
