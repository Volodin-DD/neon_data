library(data.table) # data wrangling
library(ggplot2) # data visualization
library(ggchicklet) # rounded bars

# read data of videogame sales
games_sales <- fread("Games_Sales/Data/vgsales.csv")

# exclude anomalies, group by publisher and year, calculate overall sales for each group
grouped_sales <- games_sales[
  ! Year %in% c("N/A", "2016", "2017", "2020"),
  .(sales = sum(Global_Sales * 1000000L)),
  by = .(Year, Publisher)
]

# calculate total sales for each year
total_sales <- grouped_sales[
  ,
  .(sales = sum(sales)),
  by = .(Year)
]

# keep only top-1 publisher by sales for each year
grouped_sales <- grouped_sales[
  ,
  .SD[order(sales, decreasing = TRUE)[1]],
  by = Year
]

# merge total sales and leading publisher sales
grouped_sales <- merge.data.table(
  x = grouped_sales,
  y = total_sales,
  by = "Year"
)

# calculate the ratio of leading publisher sales
grouped_sales[
  ,
  ratio := sales.x / sales.y
]

# Plotting function
ggplot(grouped_sales, aes(x = as.integer(Year), y = ratio, fill = Publisher))+
  geom_chicklet(position = "stack", alpha = 0.8)+
  geom_text(inherit.aes = FALSE,
            aes(x = as.integer(Year), y = 0.01, label = paste0(Year, " : ", Publisher)),
            col = "#eef6ff", angle = 90, hjust = 0, size = 3)+
  scale_y_continuous(labels = scales::percent_format())+ # convert the numeric format of ratio to percent format
  scale_fill_manual(values = colorRampPalette(c("#fe7f11", "#6f198c", "#1c80d3"))(5))+ # give the fill variables custom colors, based on "retrowave" pallette
  labs(
    x = "Year",
    y = "Leading publisher ratio of total sales",
    fill = "Leading publisher",
    title = "Leading videogame publisher by years",
    subtitle = "with ratio of total sales.",
    caption = "Data source: https://www.kaggle.com/gregorut/videogamesales"
  )+
  theme(
    plot.background = element_rect(fill = "#150a46"),
    text = element_text(family = "Ubuntu", colour = "#eef6ff"),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", colour = "#98c6e6"),
    legend.position = "none",
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = "Ubuntu", face = "bold", size = 10, colour = "#eef6ff"),
    axis.text.x = element_blank()
  )
