library(readxl)

df <- read_excel("/Users/ziarehman/HIES_Employment_Project/data_raw/TABLE_10-1.xls")

hies_income <- read_excel("/Users/ziarehman/HIES_Employment_Project/data_raw/TABLE_10-1.xls")

dim(hies_income)
head(hies_income, 20)
names(hies_income)
str(hies_income)


library(dplyr)
library(janitor)
library(readr)

income_clean <- hies_income |>
  clean_names()

names(income_clean) <- c(
  "occupation",
  "avg_income",
  "total",
  "q1",
  "q2",
  "q3",
  "q4",
  "q5"
)

income_clean <- income_clean |>
  mutate(
    avg_income = parse_number(avg_income),
    total = parse_number(total),
    q1 = parse_number(q1),
    q2 = parse_number(q2),
    q3 = parse_number(q3),
    q4 = parse_number(q4),
    q5 = parse_number(q5)
  )

glimpse(income_clean)

which(income_clean$occupation == "PAKISTAN")

which(income_clean$occupation == "PUNJAB")

which(income_clean$occupation == "SINDH")

income_clean |>
  filter(
    !is.na(occupation),
    is.na(avg_income),
    is.na(total),
    is.na(q1),
    is.na(q2),
    is.na(q3),
    is.na(q4),
    is.na(q5)
  ) |>
  select(occupation) |>
  distinct() |>
  print(n = 50)

which(income_clean$occupation %in% c(
  "PAKISTAN",
  "PAKISTAN URBAN",
  "PAKISTAN RURAL",
  "PUNJAB",
  "PUNJAB URBAN",
  "PUNJAB RURAL",
  "SINDH",
  "SINDH URBAN",
  "SINDH RURAL",
  "KHYBER PAKHTUNKHWA",
  "KP URBAN",
  "KP RURAL",
  "BALOCHISTAN",
  "BALOCHISTAN URBAN",
  "BALOCHISTAN RURAL"
))


which(income_clean$occupation == "Both Sex")
which(income_clean$occupation == "Male")
which(income_clean$occupation == "Female")


library(purrr)

region_names <- c(
  "PAKISTAN",
  "PAKISTAN URBAN",
  "PAKISTAN RURAL",
  "PUNJAB",
  "PUNJAB URBAN",
  "PUNJAB RURAL",
  "SINDH",
  "SINDH URBAN",
  "SINDH RURAL",
  "KHYBER PAKHTUNKHWA",
  "KP URBAN",
  "KP RURAL",
  "BALOCHISTAN",
  "BALOCHISTAN URBAN",
  "BALOCHISTAN RURAL"
)

starts <- c(
  7,44,81,118,155,
  192,229,266,303,
  340,377,414,
  451,488,525
)

income_master <- map2_df(
  starts,
  region_names,
  function(start_row, reg){
    
    block <- income_clean[start_row:(start_row+36), ]
    
    block$region <- reg
    
    block$sex_group <- NA_character_
    block$sex_group[1:12] <- "Both Sex"
    block$sex_group[13:24] <- "Male"
    block$sex_group[25:37] <- "Female"
    
    block
  }
)

dim(income_master)


table(income_master$sex_group)


income_master |>
  select(occupation) |>
  distinct() |>
  print(n = 50)

valid_occupations <- c(
  "Legislators, Senior Officials etc",
  "Professionals",
  "Technical & Associate Professionals",
  "Clerks",
  "Service & Shop Market Sale Worker",
  "Skilled Agricultural & Fishery Workers",
  "Craft & Related Trade Workers",
  "Plant & Machine Operator Assembler",
  "Elementry Occupations",
  "Armed Forces"
)

income_analysis <- income_master |>
  filter(occupation %in% valid_occupations)

dim(income_analysis)


table(income_analysis$sex_group)
table(income_analysis$region)


library(tidyr)

income_long <- income_analysis |>
  pivot_longer(
    cols = c(total, q1, q2, q3, q4, q5),
    names_to = "quintile",
    values_to = "share"
  )


pak_income <- income_master[1:37, ]

print(
  pak_income[, c("occupation","sex_group")],
  n = 37
)

dim(income_long)




income_master <- map2_df(
  starts,
  region_names,
  function(start_row, reg){
    
    block <- income_clean[start_row:(start_row+36), ]
    
    block$region <- reg
    block$sex_group <- NA_character_
    
    # Correct assignment
    block$sex_group[3:13] <- "Both Sex"
    block$sex_group[15:25] <- "Male"
    block$sex_group[27:37] <- "Female"
    
    block
  }
)


income_analysis <- income_master |>
  filter(occupation %in% valid_occupations)


table(income_analysis$sex_group)
dim(income_analysis)



income_long <- income_analysis |>
  pivot_longer(
    cols = c(total,q1,q2,q3,q4,q5),
    names_to = "quintile",
    values_to = "share"
  )

dim(income_long)


write.csv(
  income_analysis,
  "data_clean/income_analysis.csv",
  row.names = FALSE
)

write.csv(
  income_long,
  "data_clean/income_long.csv",
  row.names = FALSE
)


income_analysis |>
  group_by(sex_group, occupation) |>
  summarise(
    mean_income = mean(avg_income, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(mean_income)) |>
  print(n = 50)



gender_gap <- income_analysis |>
  filter(sex_group != "Both Sex") |>
  group_by(sex_group, occupation) |>
  summarise(
    income = mean(avg_income, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = sex_group,
    values_from = income
  ) |>
  mutate(
    gap_rupees = Male - Female,
    female_to_male_ratio = Female / Male
  ) |>
  arrange(desc(gap_rupees))

gender_gap


library(ggplot2)

gender_gap |>
  filter(!is.na(gap_rupees)) |>
  ggplot(aes(
    x = reorder(occupation, female_to_male_ratio),
    y = female_to_male_ratio
  )) +
  geom_col(fill = "grey40") +
  coord_flip() +
  labs(
    title = "Female-to-Male Income Ratio by Occupation",
    x = "",
    y = "Female / Male income ratio"
  ) +
  theme_minimal(base_size = 13)



library(dplyr)
library(tidyr)
library(ggplot2)

heat_df <- income_analysis |>
  filter(sex_group != "Both Sex") |>
  select(region, sex_group, occupation, avg_income) |>
  pivot_wider(
    names_from = sex_group,
    values_from = avg_income
  ) |>
  mutate(
    female_male_ratio = Female / Male
  ) |>
  filter(!is.na(female_male_ratio))

dim(heat_df)
head(heat_df)


heat_df |>
  count(occupation, sort = TRUE)



library(ggplot2)
library(forcats)
library(scales)

heat_df |>
  mutate(
    occupation = fct_reorder(
      occupation,
      female_male_ratio,
      .fun = mean,
      .desc = TRUE
    ),
    region = factor(
      region,
      levels = c(
        "PAKISTAN",
        "PAKISTAN URBAN",
        "PAKISTAN RURAL",
        "PUNJAB",
        "PUNJAB URBAN",
        "PUNJAB RURAL",
        "SINDH",
        "SINDH URBAN",
        "SINDH RURAL",
        "KHYBER PAKHTUNKHWA",
        "KP URBAN",
        "KP RURAL",
        "BALOCHISTAN",
        "BALOCHISTAN URBAN",
        "BALOCHISTAN RURAL"
      )
    )
  ) |>
  ggplot(aes(region, occupation, fill = female_male_ratio)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low = "black",
    high = "grey90",
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Female-to-Male Income Ratio by Region and Occupation",
    subtitle = "Darker cells indicate larger gender income inequality",
    x = "",
    y = "",
    fill = "Female / Male"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

install.packages("ggrepel")
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)




bubble_df <- income_analysis |>
  filter(
    sex_group != "Both Sex",
    occupation != "Armed Forces"
  ) |>
  group_by(sex_group, occupation) |>
  summarise(
    avg_income = mean(avg_income, na.rm = TRUE),
    employment_share = mean(total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = sex_group,
    values_from = c(avg_income, employment_share)
  ) |>
  mutate(
    share = (employment_share_Male + employment_share_Female)/2
  )

x_mid <- median(bubble_df$avg_income_Female, na.rm = TRUE)
y_mid <- median(bubble_df$avg_income_Male, na.rm = TRUE)

ggplot(
  bubble_df,
  aes(
    x = avg_income_Female,
    y = avg_income_Male
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_vline(
    xintercept = x_mid,
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = y_mid,
    linetype = "dotted"
  ) +
  geom_point(
    aes(size = share),
    alpha = 0.7
  ) +
  geom_text_repel(
    aes(label = occupation),
    size = 4,
    max.overlaps = Inf,
    box.padding = 0.6,
    point.padding = 0.5
  ) +
  guides(size = "none") +
  labs(
    title = "Occupational Gender Wage Gap Map",
    subtitle = "Diagonal = pay parity | Quadrants separate occupational wage regimes",
    x = "Female average monthly income",
    y = "Male average monthly income"
  ) +
  theme_minimal(base_size = 13)






library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

cleveland_df <- income_analysis |>
  filter(
    sex_group != "Both Sex",
    occupation != "Armed Forces"
  ) |>
  group_by(sex_group, occupation) |>
  summarise(
    avg_income = mean(avg_income, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = sex_group,
    values_from = avg_income
  ) |>
  mutate(
    gap = Male - Female,
    occupation = fct_reorder(occupation, gap)
  )

ggplot(cleveland_df) +
  geom_segment(
    aes(
      x = Female,
      xend = Male,
      y = occupation,
      yend = occupation
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(x = Female, y = occupation),
    size = 4
  ) +
  geom_point(
    aes(x = Male, y = occupation),
    size = 4
  ) +
  labs(
    title = "Gender Wage Gap Across Occupations",
    subtitle = "Line length shows male–female income difference",
    x = "Average monthly income (Rs.)",
    y = ""
  ) +
  theme_minimal(base_size = 13)


unique(income_analysis$region)




urban_rural_df <- income_analysis |>
  filter(
    sex_group == "Both Sex",
    occupation != "Armed Forces"
  ) |>
  mutate(
    area_type = case_when(
      grepl("URBAN", region) ~ "Urban",
      grepl("RURAL", region) ~ "Rural",
      TRUE ~ "Overall"
    ),
    province = case_when(
      grepl("PAKISTAN", region) ~ "Pakistan",
      grepl("PUNJAB", region) ~ "Punjab",
      grepl("SINDH", region) ~ "Sindh",
      grepl("KHYBER|KP", region) ~ "KP",
      grepl("BALOCHISTAN", region) ~ "Balochistan"
    )
  ) |>
  filter(area_type != "Overall") |>
  select(province, area_type, occupation, avg_income)

head(urban_rural_df)
dim(urban_rural_df)


urban_premium <- urban_rural_df |>
  pivot_wider(
    names_from = area_type,
    values_from = avg_income
  ) |>
  mutate(
    urban_premium = Urban / Rural
  ) |>
  arrange(desc(urban_premium))

urban_premium



library(tidyr)
library(ggplot2)
library(dplyr)

pak_premium <- urban_premium |>
  filter(province == "Pakistan") |>
  select(occupation, Urban, Rural) |>
  pivot_longer(
    cols = c(Rural, Urban),
    names_to = "area",
    values_to = "income"
  )

ggplot(
  pak_premium,
  aes(
    x = area,
    y = income,
    group = occupation
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(
    aes(label = occupation),
    size = 4,
    max.overlaps = Inf
  ) +
  labs(
    title = "Urban Wage Premium by Occupation (Pakistan)",
    subtitle = "Slope shows income gain from rural to urban labor markets",
    x = "",
    y = "Average monthly income (Rs.)"
  ) +
  theme_minimal(base_size = 13)



library(ggplot2)
library(dplyr)
library(forcats)

heat_premium <- urban_premium |>
  filter(province != "Pakistan") |>
  mutate(
    occupation = forcats::fct_reorder(
      occupation,
      urban_premium,
      mean
    )
  )

ggplot(
  heat_premium,
  aes(
    x = province,
    y = occupation,
    fill = urban_premium
  )
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = round(urban_premium, 1)),
    size = 4
  ) +
  labs(
    title = "Urban Wage Premium Across Provinces and Occupations",
    subtitle = "Number inside cell = Urban income / Rural income",
    x = "",
    y = "",
    fill = "Premium"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )




prov_income <- income_analysis |>
  filter(
    sex_group == "Both Sex",
    region %in% c(
      "PUNJAB",
      "SINDH",
      "KHYBER PAKHTUNKHWA",
      "BALOCHISTAN"
    ),
    occupation != "Armed Forces"
  ) |>
  select(region, occupation, avg_income)

prov_income



dim(prov_income)


prov_rank <- prov_income |>
  group_by(occupation) |>
  arrange(desc(avg_income), .by_group = TRUE) |>
  mutate(rank = row_number()) |>
  ungroup()

prov_rank |>
  arrange(occupation, rank)


province_winners <- prov_rank |>
  filter(rank == 1) |>
  count(region, sort = TRUE)

province_winners


prov_rank |>
  filter(rank == 1) |>
  arrange(region) |>
  select(region, occupation, avg_income)


unique(income_analysis$sex_group)

employment_analysis


ls()



weoi_base <- income_analysis |>
  filter(
    sex_group %in% c("Male", "Female"),
    occupation != "Armed Forces"
  ) |>
  group_by(region, sex_group) |>
  summarise(
    mean_income = mean(avg_income, na.rm = TRUE),
    mean_share = mean(total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = sex_group,
    values_from = c(mean_income, mean_share)
  ) |>
  mutate(
    female_male_ratio = mean_income_Female / mean_income_Male
  )

weoi_base

dim(weoi_base)



weoi <- weoi_base |>
  mutate(
    z_income = as.numeric(scale(mean_income_Female)),
    z_equity = as.numeric(scale(female_male_ratio)),
    z_participation = as.numeric(scale(mean_share_Female)),
    
    WEOI = z_income + z_equity + z_participation
  ) |>
  arrange(desc(WEOI))

weoi