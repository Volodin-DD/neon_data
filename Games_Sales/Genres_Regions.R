library(data.table) # data wrangling
library(ggplot2) # data visualization
library(ggchicklet) # rounded bars

# read data of videogame sales
games_sales <- fread("Games_Sales/Data/vgsales.csv")

# exclude anomalies
games_sales <- games_sales[
  ! Year %in% c("N/A", "2016", "2017", "2020")
]

# drop total sales variable
games_sales[, Global_Sales := NULL]

# rename region sales variables
colnames(games_sales)[7:10] <- c("North America", "Europe", "Japan", "Other")

# pivot longer
games_sales <- melt(games_sales, id.vars = 1:6, variable.name = "Region", value.name = "Sales")

# convert sales format
games_sales[, Sales := Sales * 1000000L]

# group by genre, region and year, calculate overall sales for each group
grouped_sales <- games_sales[
  ,
  .(Sales = sum(Sales)),
  by = .(Year, Genre, Region)
]

# calculate total sales for each year and region
total_sales <- grouped_sales[
  ,
  .(Sales = sum(Sales)),
  by = .(Year, Region)
]

# keep only top-1 genre by sales for each year and region
grouped_sales <- grouped_sales[
  ,
  .SD[order(Sales, decreasing = TRUE)[1]],
  by = .(Year, Region)
]

# merge total sales and leading genre sales
grouped_sales <- merge.data.table(
  x = grouped_sales,
  y = total_sales,
  by = c("Year", "Region")
)

# calculate the ratio of leading genre sales
grouped_sales[
  ,
  Ratio := Sales.x / Sales.y
]

# Plotting function
ggplot(grouped_sales, aes(x = Year, y = Ratio, fill = Genre))+
  geom_chicklet(position = "stack", alpha = 0.8)+
  scale_y_continuous(labels = scales::percent_format())+ # convert the numeric format of ratio to percent format
  scale_fill_manual(values = colorRampPalette(c("#fe7f11", "#6f198c", "#1c80d3"))(9))+ # give the fill variables custom colors, based on "retrowave" pallette
  facet_wrap(. ~ Region)+
  labs(
    x = "Year",
    y = "Leading genre ratio of total sales",
    fill = "Leading genre",
    title = "Leading videogame genre by years by region",
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
    legend.background = element_rect(fill = "#150a46"),
    legend.position = "bottom",
    legend.key = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(family = "Ubuntu", face = "bold", size = 10, colour = "#eef6ff"),
    axis.text.x = element_text(family = "Ubuntu", face = "bold", size = 10, colour = "#eef6ff",
                               angle = 30, vjust = 2, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(family = "Ubuntu", face = "bold", size = 10, colour = "#eef6ff")
  )
