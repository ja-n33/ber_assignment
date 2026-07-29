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
    #panel.grid.minor.y = element_line(color = "#E2E0DA", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
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


series <- list(codes = c("CPS00000", "CPS00014", "CPS01000",  "CPS00007", "CPS00006"), 
                cols = c("headline", "core", "foodnab", "services", "goods"),
                names = c("Headline", "Core", "Services", "Goods"))

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
                aes(x = date_col, y = value, colour = fct_reorder2(name, date_col, value, .desc = TRUE)),
                 linewidth = 1) +
    geom_point(data = df1_plot %>% filter(!(name %in% c("target1", "target2", "fuel_yoy"))), 
                aes(x = date_col, y = value, colour = fct_reorder2(name, date_col, value, .desc = TRUE)), 
                stat = "identity", 
                show.legend = FALSE) +
    geom_text(data = df1_plot %>% filter(!(name %in% c("target1", "target2", "fuel_yoy"))) %>% group_by(name) %>% filter(date_col == as.Date("2026-06-01")) %>% ungroup(), 
                aes(x = date_col, y = value, colour = fct_reorder2(name, date_col, value, .desc = TRUE), label = round(value, 2)), 
                stat = "identity", 
                show.legend = FALSE,
                position = "nudge", vjust = 0.3, hjust = -0.18) +          
    scale_colour_manual(values = c("headline_yoy" = "#1B2A4A", 
                                    "core_yoy" = "#B8860B",
                                    "foodnab_yoy"  = "#8C2D2D",
                                    "services_yoy"     = "#3D6B72",
                                    "goods_yoy" = "#8A8580"), 
                        labels = c("headline_yoy" = "Headline CPI", 
                                    "core_yoy" = "Core CPI", 
                                    "foodnab_yoy" = "Food & Non-Alcoholic Beverages",
                                    "services_yoy" = "Services",
                                    "goods_yoy" = "Goods")) + 
    geom_line(data = df1_plot %>% filter(name == "target1"), aes(x = date_col, y = value),
          linetype = "dashed", 
          colour = "black", 
          linewidth = 0.8) +
    geom_line(data = df1_plot %>% filter(name == "target2"), aes(x = date_col, y = value),
          linetype = "dashed", 
          colour = "black", 
          linewidth = 0.8, na.rm = ) +
    scale_y_continuous(limits = c(0, 6),
                        breaks = seq(0, 6, by = 2), 
                        #minor_breaks = seq(0, 6, by = 1), 
                        expand = c(0, 0)) +
    scale_x_date(date_labels = "%m-%Y", 
                date_breaks = "1 months",
                limits = c(as.Date("2026-01-01"), max(plot1_df$date_col)), expand = expansion(mult = c(0.01, 0.055))) +
  annotate("text", 
             x = as.Date("2026-01-01"), y = 2.8, 
             label = "MPC target (3%)", 
             colour = "grey40", family = "Georgia", size = 3.5, 
             fontface = "italic", hjust = 0) +
  geom_hline(yintercept = 0, 
            colour = "black",
            linewidth = 0.4) +
    guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Selected Consumer Price Series Changes, January - June 2026",
  caption = "Source: BER, Statistics South Africa\nNote: Rates calculated as year-on-year changes.", 
  y = "% Change (Y-o-Y)", 
  x = "") +
  sarb_theme +
  theme(legend.position = "bottom")

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

plot2 <- ggplot(data = df2_plot) +
    geom_col(aes(x = month_label, y = value, fill = fct_reorder2(name, date_col, value, .desc = TRUE)),
    position = position_dodge2(width = 20, preserve = "single")) +
     geom_text(aes(x = month_label, y = value, colour = fct_reorder2(name, date_col, value, .desc = TRUE),label = round(value, 2), vjust = ifelse(value >= 0 , -0.5, 1)), 
                 size = 3,
                 show.legend = FALSE,
                position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("fuel_mom" = "#1B2A4A", 
                                    "petrol_mom" = "#B8860B",
                                    "diesel_mom"  = "#8C2D2D"), 
                        labels = c("fuel_mom" = "Fuel and Lubricants", 
                                    "petrol_mom" = "Petrol", 
                                    "diesel_mom" = "Diesel")) + 
    scale_colour_manual(values = c("fuel_mom" = "#1B2A4A", 
                                    "petrol_mom" = "#B8860B",
                                    "diesel_mom"  = "#8C2D2D"), 
                        labels = c("fuel_mom" = "Fuel and Lubricants", 
                                    "petrol_mom" = "Petrol", 
                                    "diesel_mom" = "Diesel")) +
    scale_y_continuous(breaks = seq(-10, 40, by = 10), 
                         #minor_breaks = seq(-8, 40, by = ), 
                         expand = expansion(mult = c(0.01, 0.055))) +
    guides(fill = guide_legend(nrow = 1)) +
    # scale_x_(date_labels = "%m-%Y", 
    #             date_breaks = "1 months",
    #             limits = c(as.Date("2026-01-01"), max(plot1_df$date_col)), expand = c(0, 0)) +
  labs(title = "Fuel and Lubricants Price Changes, January - June 2026",
  caption = "Source: BER, Statistics South Africa\nNote: Rates calculated as month-on-month changes.", 
  y = "% Change (M-o-M)", 
  x = "") +
  sarb_theme +
    theme(legend.position = "bottom")


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


##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 4: EXPECTATIONS PLOT ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################


expectations <- tibble(date_col = as.Date(character()))

for (sheet in c("Professionals", "Analysts", "Trade_unions", "Businesses")){
    lab <- tolower(stringr::str_sub(sheet, 1, 3))

    t0 <- paste0(lab, "_t0")
    t1 <- paste0(lab, "_t1")
    t2 <- paste0(lab, "_t2")

    temp_df <- readxl::read_excel(here::here("inflexp.xlsx"), sheet = sheet) %>%
                dplyr::select(Date, `CPI t0`, `CPI t1`, `CPI t2`) %>%
                dplyr::mutate(date_col = lubridate::yq(Date)) %>%
                dplyr::select(-Date) %>%
                dplyr::filter(date_col > as.Date("2025-09-01"),
                                 date_col < as.Date("2026-07-01")) %>%
                dplyr::rename(!!t0 := "CPI t0" , !!t1 := "CPI t1" , !!t2 := "CPI t2")

    expectations <- full_join(expectations, temp_df, by = "date_col")

}

expectations <- expectations %>%
            pivot_longer(-date_col, names_to = "name") 


sarb_qpm <- tibble(date_col = rep(seq(as.Date("2025-10-01"), 
                                        as.Date("2026-04-01"), 
                                        by = "3 months"), each = 3),
                    name = rep(c("qpm_t0", "qpm_t1", "qpm_t2") , times = 3), 
                    value = as.numeric(c("3.3", "3.5", "3.1", "3.7", "3.3", "3.0", "4.0", "3.8", "3.1")))

expectations <- bind_rows(expectations, sarb_qpm) %>%
                dplyr::mutate(group = stringr::str_sub(name, 1, 3),
                                    t = stringr::str_sub(name, 5, 6)) 

expectations <- expectations %>%
                dplyr::mutate(period = paste0(lubridate::year(as.Date(date_col)),"-" , lubridate::quarter(as.Date(date_col))),
                            xval = case_when(date_col < as.Date("2026-01-01") & t == "t0" ~ "2025",
                                            date_col < as.Date("2026-01-01") & t == "t1" ~ "2026",
                                            date_col < as.Date("2026-01-01") & t == "t2" ~ "2027",
                                            date_col >= as.Date("2026-01-01") & t == "t0" ~ "2026",
                                            date_col >= as.Date("2026-01-01") & t == "t1" ~ "2027",
                                            date_col >= as.Date("2026-01-01") & t == "t2" ~ "2028", 
                                            TRUE ~ NA_character_))
                                            


df3_plot <- expectations %>%
        dplyr::select(-date_col) %>%
        dplyr::mutate(xval = as.numeric(xval))


plot3 <- ggplot(data = df3_plot %>% dplyr::filter(period != "2026-3", xval != 2025)) +
    geom_line(aes(x = xval, y = value, colour = fct_reorder2(group, xval, value, .desc = TRUE), linetype = period),
                 linewidth = 1) +
    geom_point(aes(x = xval, y = value, colour = fct_reorder2(group, xval, value, .desc = TRUE)), 
                stat = "identity", 
                show.legend = FALSE) +
    geom_text(data = df3_plot %>% dplyr::filter(period == "2026-2"), aes(x = xval, y = value, colour = fct_reorder2(group, xval, value, .desc = TRUE), label = round(value, 2),
                vjust = ifelse((xval == 2027 & group == "ana" & period == "2026-2"), 1.9, -0.4), 
                hjust = ifelse((xval == 2026) | (xval == 2027 & group == "bus" & period == "2026-1") , 1.25, -0.18)
                ), 
                stat = "identity", 
                show.legend = FALSE) +          
    scale_colour_manual(values = c("pro" = "#1B2A4A", 
                                    "ana" = "#B8860B",
                                    "tra"  = "#8C2D2D",
                                    "bus"     = "#3D6B72",
                                    "qpm" = "#8A8580"),
                        labels = c("pro" = "Professionals",
                                    "ana" = "Analysts",
                                    "tra"  = "Trade Unions",
                                    "bus"     = "Businesses",
                                    "qpm" = "SARB QPM")) + 
    scale_linetype_manual(values = c("2025-4" = "dotted",
                                    "2026-1" = "dashed",
                                    "2026-2" = "solid"),
                            labels = c("2025-4" = "2025Q4",
                                    "2026-1" = "2026Q1",
                                    "2026-2" = "2026Q2")) +
    scale_y_continuous(limits = c(3, 4.75),
                        breaks = seq(3, 4.75, by = 0.35), 
                        #minor_breaks = seq(0, 6, by = 1), 
                        expand = c(0, 0)) +
    scale_x_continuous(limits = c(2026, 2028), 
                        breaks = seq(2026, 2028, by = 1),
                        expand = expansion(mult = c(0.055, 0.055))) +
  annotate("text", 
             x = 2026, y = 3.05, 
             label = "MPC target (3%)", 
             colour = "grey40", family = "Georgia", size = 3.5, 
             fontface = "italic", hjust = 0) +
  geom_hline(yintercept = 3, 
            colour = "black",
            linewidth = 0.4) +
    guides(colour = guide_legend(nrow = 1), linetype = "none") +
  labs(title = "Inflation Expecatations by Survey Groups",
  caption = "Source: BER, SARB\nNote: SARB QPM values obtained from MPC forecast reports for November 2025, March 2026 and July 2026.\n           Dotted lines represent 2025 Q4 forecasts, dashed 2026 Q1 and solid the most recent.", 
  y = "Expected CPI (%)", 
  x = "") +
  sarb_theme +
  theme(legend.position = "bottom")

ggsave(file = here::here("forecast_paths.png"), plot = plot3, width = 10, height = 6, dpi = 300)



##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 4: CURRENCY CHANGES ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################

currency_df <- tibble(date_col = as.Date(character()))


for (currency in c("dollar", "yen", "yuan", "euro", "gbp")){
    temp_file <- paste0(currency, ".csv")
    prefixed <- paste0(currency, "_chng")

    if (currency %in% c("dollar", "euro", "gbp")){
        temp_df <- readr::read_csv(here::here(temp_file), skip = 3) %>%
                dplyr::mutate(date_col = as.Date(Date), 
                                Value = as.numeric(1 / Value)) %>%
                arrange(date_col) %>%
                dplyr::mutate(!!prefixed := ((Value - lag(Value, n = 1)) / lag(Value, n = 1) * 100)) %>%
                dplyr::filter(date_col >= as.Date("2026-07-20")) %>%
                dplyr::select(-c("Date", "Value"))
    } else {
        temp_df <- readr::read_csv(here::here(temp_file), skip = 3) %>%
                dplyr::mutate(date_col = as.Date(Date), 
                                Value = as.numeric(Value)) %>%
                arrange(date_col) %>%
                dplyr::mutate(!!prefixed := ((Value - lag(Value, n = 1)) / lag(Value, n = 1) * 100)) %>%
                dplyr::filter(date_col >= as.Date("2026-07-20")) %>%
                dplyr::select(-c("Date", "Value")) 
    }

currency_df <- full_join(currency_df, temp_df, by = "date_col")
                
}


currency_tbl <- currency_df %>%
        dplyr::filter(date_col >= as.Date("2026-07-20"), date_col <= as.Date("2026-07-24")) %>%
        pivot_longer(-date_col, names_to = "currency") %>%
        dplyr::mutate(Currency = case_when(currency == "dollar_chng" ~ "US Dollar",
                                            currency == "yen_chng" ~ "Japanese Yen",
                                            currency == "yuan_chng" ~ "Chinese Yuan",
                                            currency == "gbp_chng" ~ "GB Pound",
                                            currency == "euro_chng" ~ "Euro"),
                        Day = case_when(date_col == as.Date("2026-07-20") ~ "Jul 20",
                                        date_col == as.Date("2026-07-21") ~ "Jul 21",
                                        date_col == as.Date("2026-07-22") ~ "Jul 22",
                                        date_col == as.Date("2026-07-23") ~ "Jul 23",
                                        date_col == as.Date("2026-07-24") ~ "Jul 24")) %>%
        pivot_wider(id_cols = Currency, names_from = "Day", values_from = value) 


writexl::write_xlsx(currency_tbl, here::here("currencies.xlsx"))



##########################################################################################################################################################################################################################################################################

################################################################### FIGURE 7: FOOD PLOT ##############################################################################################################################################################

##########################################################################################################################################################################################################################################################################


food_items <- tibble::tribble(
  ~label, ~code,
  "Rice", "01111001",
  "Loaf of white bread", "01112001",
  "Loaf of brown bread", "01112002",
  "Sweet biscuits", "01112003",
  "Savoury biscuits", "01112004",
  "Bread rolls", "01112005",
  "Rusks", "01112301",
  "Spaghetti", "01113001",
  "Macaroni", "01113002",
  "Pasta (excl spaghetti, macaroni)", "01113003",
  "Instant noodles (e.g. 2 minute noodles)", "01113004",
  "Cake or tart", "01114001",
  "Frozen pastry products (pizza or pies)", "01114002",
  "Cake flour", "01116001",
  "Bread flour", "01116002",
  "Maize meal", "01116003",
  "Cereals", "01116005",
  "Super maize", "01116008",
  "Special maize", "01116009",
  "Hot cereals (porridge) incl instant porridge", "01116010",
  "Ready-mix flour", "01116011",
  "Samp", "01116012",
  "Beef mince", "01121005",
  "Beef offal", "01121010",
  "Beef steak", "01121011",
  "Stewing beef/brisket/chuck", "01121012",
  "Pork - combined", "01122099",
  "Lamb/mutton - combined", "01123099",
  "Whole chicken - fresh", "01124001",
  "Chicken portions - fresh", "01124002",
  "IQF chicken portions", "01124005",
  "Chicken portions frozen - non IQF", "01124006",
  "Chicken giblets (neck, gizzards, hearts, etc)", "01124007",
  "Polony", "01125004",
  "Ham", "01125005",
  "Biltong", "01125006",
  "Bacon", "01125007",
  "Sausage", "01125009",
  "Beef extract", "01126002",
  "Corned beef", "01126005",
  "Hake - frozen", "01131001",
  "Fish fingers - frozen", "01134001",
  "Tuna - tinned", "01134002",
  "Fish (excl tuna) - tinned", "01134003",
  "Full cream milk - fresh", "01141001",
  "Full cream milk - long life", "01141002",
  "Low fat milk - fresh", "01142001",
  "Low fat milk - long life", "01142002",
  "Powdered milk", "01143001",
  "Whiteners", "01143002",
  "Condensed milk", "01143003",
  "Plain yogurt", "01144001",
  "Flavoured yogurt", "01144002",
  "Cheddar cheese", "01145001",
  "Gouda cheese", "01145002",
  "Cheese spread", "01145003",
  "Feta cheese", "01145004",
  "Cream - fresh", "01146001",
  "Sour milk", "01146002",
  "Custard - prepared", "01146003",
  "Maize based food drink (e.g. mageu)", "01146004",
  "Flavoured milk", "01146005",
  "Eggs", "01147001",
  "Margarine spread (in a tub)", "01152001",
  "Brick margarine", "01152002",
  "Peanut butter", "01152003",
  "Sunflower oil (incl canola oil)", "01154001",
  "Bananas", "01162001",
  "Apples", "01163001",
  "Seasonal fruit", "01167099",
  "Peanuts", "01168004",
  "Lettuce", "01171001",
  "Spinach/morogo", "01171002",
  "Cabbage", "01172001",
  "Cauliflower", "01172002",
  "Broccoli", "01172003",
  "Tomatoes", "01173001",
  "Pumpkin", "01173002",
  "Green/red/yellow pepper", "01173003",
  "Vegetables - frozen", "01173004",
  "Cucumber", "01173006",
  "Onions", "01174001",
  "Carrots", "01174002",
  "Beetroot", "01174003",
  "Mushrooms", "01174005",
  "Beans - dried", "01175002",
  "Baked beans - tinned", "01176002",
  "Prepared salads", "01176005",
  "Atchar", "01176006",
  "Mixed vegetables - tinned", "01176007",
  "Potatoes", "01177001",
  "Sweet potatoes", "01178001",
  "Potato chips - frozen", "01178002",
  "Potato crisps", "01178003",
  "Corn chips", "01178004",
  "White sugar", "01181001",
  "Brown sugar", "01181002",
  "Jam", "01182001",
  "Chocolate slab", "01183001",
  "Chocolate bar", "01183002",
  "Sweets", "01184001",
  "Chewing gum", "01184002",
  "Ice cream", "01185001",
  "Vinegar", "01191001",
  "Chutney", "01191002",
  "Tomato sauce", "01191004",
  "Mayonnaise", "01191005",
  "Salad dressing", "01191006",
  "Salt", "01192001",
  "Spices (excl salt and curry powder)", "01192002",
  "Curry powder", "01192003",
  "Baby food - cereal", "01193001",
  "Baby food - milk formula", "01193003",
  "Instant yeast", "01193004",
  "Baking powder", "01193005",
  "Soup powder", "01193006",
  "Baby food - pureed bottled/pouched", "01193008",
  "Instant coffee", "01211001",
  "Ground coffee or coffee beans", "01211002",
  "Cappucino sachets", "01211003",
  "Ceylon/black tea", "01212001",
  "Rooibos tea", "01212002",
  "Drinking chocolate (e.g. milo, cocoa)", "01213001",
  "Mineral water - sparkling or still", "01221001",
  "Fizzy drinks - can", "01222001",
  "Fizzy drinks - bottle", "01222002",
  "Other soft drinks (e.g. energy drinks and iced tea)", "01222003",
  "Fruit juice", "01223001",
  "Fruit juice concentrates", "01223002",
  "Dairy blends/mixtures", "01223003"
)

coicop_df <- readxl::read_excel(here("coicop.xlsx")) %>%
    dplyr::select(c(`Old code`, matches("^M20"))) %>%
    dplyr::rename(digit = `Old code`)

food_list <- list()

for (i in 1:length(food_items$code)){
        code <- food_items$code[i]

        temp_df <- coicop_df %>%
            dplyr::filter(digit == code) %>%
            dplyr::mutate(yoy = (M202606 - M202506) / M202506 * 100) %>%
            dplyr::select(digit, yoy)

    food_list[[i]] <- temp_df
}

food_df <- dplyr::bind_rows(food_list)

top5 <- food_df %>%
        arrange(desc(yoy)) %>%
        head(5)

bottom5 <- food_df %>%
        arrange(yoy) %>%
        head(5)

df4_plot <- dplyr::bind_rows(top5, bottom5) %>%
        dplyr::rename("code" = "digit") %>%
        left_join(food_items, by = "code")


plot4 <- ggplot(data = df4_plot, aes(x = fct_reorder(label, yoy, .desc = FALSE), y = yoy, fill = (yoy > 0))) +
        geom_col() +
        scale_fill_manual(values = c("TRUE" = "#1B2A4A", "FALSE" = "#8C2D2D")) +
        coord_flip() +
        labs(title = "Top Food Price Changes",
            caption = "Source: Statistics South Africa\nNote: Rates calculated as year-on-year changes.", 
            y = "", 
        x = "% Change (Y-o-Y)") +
        sarb_theme +
        theme(panel.grid = element_blank(),
                legend.position = "none")

ggsave(file = here::here("food_changes.png"), plot = plot4, width = 10, height = 6, dpi = 300)




