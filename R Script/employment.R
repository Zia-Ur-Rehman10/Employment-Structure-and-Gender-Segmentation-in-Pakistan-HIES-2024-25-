install.packages(c(
  "readxl",
  "tidyverse",
  "janitor",
  "skimr",
  "data.table",
  "labelled",
  "survey",
  "ggplot2",
  "GGally",
  "Hmisc",
  "naniar"
))

library(readxl)
library(tidyverse)
library(janitor)
library(skimr)
library(data.table)
library(labelled)
library(survey)
library(ggplot2)
library(GGally)
library(Hmisc)
library(naniar)

getwd()

library(readxl)

df <- read_excel("/Users/ziarehman/HIES_Employment_Project/data_raw/TABLE_06-1.xls")

library(readxl)

excel_sheets("data_raw/TABLE_06-1.xls")

read_excel("data_raw/TABLE_06-1.xls")

hies <- read_excel("data_raw/TABLE_06-1.xls")

dim(hies)

names(hies)

str(hies)

clean_hies <- hies %>%
  clean_names()

names(clean_hies)

print(clean_hies[1:30, ], n = 30)

names(clean_hies) <- c(
  "category",
  "total",
  "q1",
  "q2",
  "q3",
  "q4",
  "q5"
)

names(clean_hies)

str(clean_hies)

which(clean_hies$category == "Female")

which(clean_hies$category == "Male")

which(clean_hies$category == "Both Sexes")

library(readr)

clean_hies <- clean_hies |>
  mutate(
    total = parse_number(total),
    q1 = parse_number(q1),
    q2 = parse_number(q2),
    q3 = parse_number(q3),
    q4 = parse_number(q4),
    q5 = parse_number(q5)
  )

which(clean_hies$category == "Both Sexes")
which(clean_hies$category == "Male")
which(clean_hies$category == "Female")

pakistan_block <- clean_hies[6:62, ]
print(pakistan_block, n = 57)

both_sexes <- clean_hies[11:27, ]
print(both_sexes, n = 17)

male <- clean_hies[28:44, ]
print(male, n = 17)

female <- clean_hies[45:61, ]
print(female, n = 17)

pak <- pakistan_block

pak$sex_group <- NA
pak$sex_group[6:22] <- "Both Sexes"
pak$sex_group[23:39] <- "Male"
pak$sex_group[40:56] <- "Female"

library(tidyr)

pak <- pak |>
  fill(sex_group)
print(pak[, c("category","sex_group")], n=57)

analysis_df <- pak |>
  filter(
    !category %in% c(
      "PAKISTAN",
      "Percentage Distribution of Earners",
      "Head/Other than Head and Employment",
      "Status:",
      "Both Sexes",
      "Male",
      "Female",
      "Head of Household",
      "Other than Head"
    )
  ) |>
  filter(!is.na(category))
print(analysis_df, n=50)

##########################
pak$sex_group <- NA_character_

pak$sex_group[1:22]  <- "Both Sexes"
pak$sex_group[23:39] <- "Male"
pak$sex_group[40:57] <- "Female"

table(pak$sex_group)
print(pak[, c("category","sex_group")], n = 57)

analysis_df <- pak |>
  filter(
    !is.na(category),
    !category %in% c(
      "PAKISTAN",
      "Percentage Distribution of Earners by",
      "Head/Other than Head and Employment",
      "Status:",
      "Both Sexes",
      "Male",
      "Female",
      "Head of Household",
      "Other than Head"
    )
  )
print(analysis_df[, c("category","sex_group","total")], n = 50)


analysis_df <- analysis_df |>
  mutate(
    relation = case_when(
      row_number() %in% c(1:6, 12:17, 23:28) ~ "Head",
      row_number() %in% c(7:11, 18:22, 29:33) ~ "Other",
      TRUE ~ "Summary"
    )
  )

nrow(analysis_df)
table(analysis_df$sex_group)


analysis_df <- analysis_df |>
  group_by(sex_group) |>
  mutate(
    row_id = row_number(),
    relation = case_when(
      row_id <= 7 ~ "Head",
      row_id > 7  ~ "Other"
    )
  ) |>
  ungroup()
print(
  analysis_df[, c("sex_group","row_id","relation","category","total")],
  n = 50
)

employment_df <- analysis_df |>
  filter(
    category %in% c(
      "Employer",
      "Self Employed",
      "Contributing Family Worker",
      "Employee",
      "Not Economically Active"
    )
  )

print(employment_df, n = 50)

nrow(employment_df)

library(tidyr)

employment_long <- employment_df |>
  pivot_longer(
    cols = c(total, q1, q2, q3, q4, q5),
    names_to = "quintile",
    values_to = "percentage"
  )

head(employment_long)


library(ggplot2)

ggplot(
  employment_long |> filter(quintile == "total"),
  aes(
    x = category,
    y = percentage,
    fill = sex_group
  )
) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  facet_wrap(~relation) +
  labs(
    title = "Employment Status by Gender in Pakistan",
    x = "",
    y = "Percentage",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


employment_long$quintile <- factor(
  employment_long$quintile,
  levels = c("q1","q2","q3","q4","q5")
)

ggplot(
  employment_long |>
    filter(category == "Employee",
           quintile != "total"),
  aes(
    x = quintile,
    y = percentage,
    group = sex_group,
    linetype = sex_group
  )
) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  facet_wrap(~relation) +
  labs(
    title = "Employee Status Across Income Quintiles (Pakistan)",
    subtitle = "Head vs Other Household Members",
    x = "Income Quintile",
    y = "Percentage",
    linetype = "Sex"
  ) +
  theme_minimal(base_size = 12)


ggplot(
  employment_long |>
    filter(category == "Employee",
           quintile != "total"),
  aes(
    x = quintile,
    y = percentage,
    fill = sex_group
  )
) +
  geom_col(position = "dodge") +
  facet_wrap(~relation) +
  labs(
    title = "Employee Share by Income Quintile",
    x = "Quintile",
    y = "Percentage"
  ) +
  theme_minimal(base_size = 12)

library(dplyr)

df_clean <- analysis_df %>%
  filter(category %in% c("Employer",
                         "Self Employed",
                         "Contributing Family Worker",
                         "Employee")) %>%
  filter(relation == "Head") %>%
  filter(!is.na(total))

df_percent <- df_clean %>%
  group_by(sex_group) %>%
  mutate(percent = total / sum(total) * 100)

ggplot(df_percent, aes(x = category, y = percent, fill = sex_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment Distribution by Sex Group",
       x = "Category",
       y = "Percentage") +
  theme_minimal()

####Second Pie Chart
library(dplyr)
library(ggplot2)

df_clean <- analysis_df %>%
  filter(category %in% c("Employer",
                         "Self Employed",
                         "Contributing Family Worker",
                         "Employee")) %>%
  filter(relation == "Head") %>%
  filter(!is.na(total)) %>%
  group_by(sex_group) %>%
  mutate(percent = total / sum(total) * 100)

df_clean <- df_clean %>%
  mutate(label = paste0(category, "\n", round(percent, 1), "%"))

ggplot(df_clean, aes(x = "", y = percent, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  
  # Labels inside slices (well-positioned)
  geom_text(aes(label = paste0(round(percent,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  
  facet_wrap(~sex_group) +
  
  # Clean professional colors
  scale_fill_brewer(palette = "Set1") +
  
  labs(
    title = "Employment Composition by Sex Group",
    subtitle = "Distribution of Employment Status among Household Heads",
    fill = "Employment Category"
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

######### AGAIN
nrow(clean_hies) / 57

clean_hies |>
  filter(
    !is.na(category),
    total %>% is.na(),
    q1 %>% is.na(),
    q2 %>% is.na(),
    q3 %>% is.na(),
    q4 %>% is.na(),
    q5 %>% is.na()
  ) |>
  select(category) |>
  distinct() |>
  print(n = 200)

which(clean_hies$category == "PAKISTAN")

clean_hies |>
  filter(str_detect(category, "Urban|Rural|Punjab|Sindh|Khyber|Baloch")) |>
  select(category) |>
  distinct()

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

starts <- which(clean_hies$category %in% region_names)

starts

length(starts)


library(dplyr)
library(tidyr)
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

starts <- c(6,63,120,177,234,291,348,405,462,519,576,633,690,747,804)

master <- map2_df(
  starts,
  region_names,
  function(start_row, reg){
    
    block <- clean_hies[start_row:(start_row+56), ]
    
    block$region <- reg
    
    block$sex_group <- NA_character_
    block$sex_group[1:22]  <- "Both Sexes"
    block$sex_group[23:39] <- "Male"
    block$sex_group[40:57] <- "Female"
    
    block
  }
)

dim(master)

master_clean <- master |>
  filter(
    !is.na(category),
    category %in% c(
      "Average No. of  Earners Per HH",
      "Total (Head & Other than Head)",
      "Total(Head & Other than Head)",
      "Total",
      "Employer",
      "Self Employed",
      "Contributing Family Worker",
      "Employee",
      "Not Economically Active"
    )
  )

table(master_clean$region)

master_clean <- master_clean |>
  group_by(region, sex_group) |>
  mutate(
    row_id = row_number(),
    relation = case_when(
      row_id <= 7 ~ "Head",
      row_id > 7  ~ "Other"
    )
  ) |>
  ungroup()

employment_master <- master_clean |>
  filter(
    category %in% c(
      "Employer",
      "Self Employed",
      "Contributing Family Worker",
      "Employee",
      "Not Economically Active"
    )
  )

dim(employment_master)


employment_long_master <- employment_master |>
  pivot_longer(
    cols = c(total,q1,q2,q3,q4,q5),
    names_to = "quintile",
    values_to = "percentage"
  )

dim(employment_long_master)


female_penalty <- employment_long_master |>
  filter(
    category == "Employee",
    sex_group %in% c("Male","Female"),
    quintile != "total"
  ) |>
  select(region, relation, quintile, sex_group, percentage) |>
  pivot_wider(
    names_from = sex_group,
    values_from = percentage
  ) |>
  mutate(
    gender_gap = Male - Female
  )

head(female_penalty)

ggplot(
  female_penalty,
  aes(
    x = quintile,
    y = gender_gap,
    group = region,
    linetype = region
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~relation) +
  labs(
    title = "Female Employment Penalty Across Regions",
    x = "Income Quintile",
    y = "Male − Female Employee Share"
  ) +
  theme_minimal()

employment_long_master <- employment_long_master |>
  mutate(
    area_type = case_when(
      grepl("URBAN", region) ~ "Urban",
      grepl("RURAL", region) ~ "Rural",
      TRUE ~ "Overall"
    ),
    province = case_when(
      grepl("PUNJAB", region) ~ "Punjab",
      grepl("SINDH", region) ~ "Sindh",
      grepl("KHYBER|KP", region) ~ "KP",
      grepl("BALOCH", region) ~ "Balochistan",
      grepl("PAKISTAN", region) ~ "Pakistan"
    )
  )

table(employment_long_master$province,
      employment_long_master$area_type)


province_gap <- employment_long_master |>
  filter(
    category == "Employee",
    sex_group %in% c("Male","Female"),
    quintile != "total",
    area_type == "Overall",
    province != "Pakistan"
  ) |>
  select(province, relation, quintile, sex_group, percentage) |>
  pivot_wider(
    names_from = sex_group,
    values_from = percentage
  ) |>
  mutate(
    gender_gap = Male - Female
  )

print(province_gap)


ggplot(
  province_gap,
  aes(
    x = quintile,
    y = province,
    fill = gender_gap
  )
) +
  geom_tile(color = "white", linewidth = 1) +
  facet_wrap(~relation) +
  labs(
    title = "Gender Employment Gap Across Provinces",
    subtitle = "Male Employee Share − Female Employee Share",
    x = "Income Quintile",
    y = "",
    fill = "Gap"
  ) +
  theme_minimal(base_size = 13)

urban_rural_gap <- employment_long_master |>
  filter(
    category == "Employee",
    sex_group %in% c("Male","Female"),
    quintile != "total",
    area_type %in% c("Urban","Rural")
  ) |>
  select(province, area_type, relation, quintile, sex_group, percentage) |>
  pivot_wider(
    names_from = sex_group,
    values_from = percentage
  ) |>
  mutate(
    gender_gap = Male - Female
  )

ggplot(
  urban_rural_gap,
  aes(x = quintile, y = province, fill = gender_gap)
) +
  geom_tile(color = "white") +
  facet_grid(area_type ~ relation) +
  labs(
    title = "Urban–Rural Gender Employment Gap",
    fill = "Gap"
  ) +
  theme_minimal(base_size = 12)

female_index <- employment_long_master |>
  filter(
    sex_group == "Female",
    quintile == "total",
    category %in% c(
      "Employee",
      "Employer",
      "Not Economically Active"
    )
  ) |>
  group_by(region, category) |>
  summarise(
    percentage = mean(percentage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = category,
    values_from = percentage
  ) |>
  mutate(
    female_inclusion = Employee + Employer - `Not Economically Active`
  ) |>
  arrange(desc(female_inclusion))

print(female_index, n = 20)

female_index2 <- employment_long_master |>
  filter(
    sex_group == "Female",
    quintile == "total"
  ) |>
  group_by(region, category) |>
  summarise(
    percentage = mean(percentage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = category,
    values_from = percentage
  ) |>
  mutate(
    inclusion_index =
      Employee +
      Employer +
      `Self Employed` -
      `Not Economically Active` -
      `Contributing Family Worker`
  ) |>
  arrange(desc(inclusion_index))

print(female_index2, n = 20)

ggplot(
  employment_long_master |>
    filter(
      quintile == "total",
      sex_group == "Female",
      relation == "Other",
      area_type == "Overall",
      province != "Pakistan"
    ),
  aes(
    x = province,
    y = percentage,
    fill = category
  )
) +
  geom_col() +
  labs(
    title = "Female Labor Composition by Province",
    y = "Percent"
  ) +
  theme_minimal(base_size = 12)


ggplot(
  employment_long_master |>
    filter(
      category == "Self Employed",
      quintile != "total",
      area_type == "Overall",
      province != "Pakistan"
    ),
  aes(
    x = quintile,
    y = percentage,
    group = province,
    linetype = province
  )
) +
  geom_line(linewidth = 1.2) +
  facet_grid(sex_group ~ relation) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Self Employment Across Income Quintiles"
  )

write.csv(
  employment_long_master,
  "outputs/hies_employment_clean.csv",
  row.names = FALSE
)

cluster_df <- employment_long_master |>
  filter(
    quintile == "total",
    relation == "Other",
    sex_group == "Female"
  ) |>
  select(region, category, percentage) |>
  group_by(region, category) |>
  summarise(
    percentage = mean(percentage),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = category,
    values_from = percentage
  )

print(cluster_df)

cluster_matrix <- cluster_df |>
  column_to_rownames("region")

cluster_matrix

cluster_scaled <- scale(cluster_matrix)


set.seed(123)

km <- kmeans(cluster_scaled, centers = 4)

km$cluster

colSums(is.na(cluster_df))

cluster_df[is.na(cluster_df)] <- 0

library(tibble)

cluster_matrix <- cluster_df |>
  column_to_rownames("region")

cluster_scaled <- scale(cluster_matrix)


cluster_scaled <- cluster_scaled[, apply(cluster_scaled, 2, sd) > 0]


set.seed(123)

km <- kmeans(cluster_scaled, centers = 4)

km$cluster

cluster_results <- data.frame(
  region = rownames(cluster_matrix),
  cluster = factor(km$cluster)
)

print(cluster_results)



cluster_df$cluster <- factor(km$cluster)

cluster_profile <- cluster_df |>
  group_by(cluster) |>
  summarise(across(where(is.numeric), mean))

print(cluster_profile)


write.csv(
  cluster_results,
  "outputs/regional_clusters.csv",
  row.names = FALSE
)

write.csv(
  cluster_profile,
  "outputs/cluster_profiles.csv",
  row.names = FALSE
)


write.csv(
  employment_long_master,
  "outputs/employment_long_master.csv",
  row.names = FALSE
)

