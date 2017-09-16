library(readr)
library(ggplot2)
library(ggthemes)
library(CausalImpact)

# Download last 5 years of KeepCup weekly search trends data in Australia from Google Trends
# Use the term "reusable coffee cup" as control
searchTrends <- read_csv("data/keepcup_Google_trends.csv", 
                         col_types = cols(week = col_date(format = "%Y-%m-%d")))

# Plot data
ggplot() +
  geom_line(data = searchTrends, aes(x = week, y = keep_cup_index, color = "Keep Cup")) +
  geom_line(data = searchTrends, aes(x = week, y = reusable_coffee_cup_index, color = "Reusable Coffee Cup")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") + 
  xlab("Week") + ylab("Search Index") +
  labs(title="Keep Cup Search Trends in Australia",
       caption="Source: Google Trends") +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank())

# Transformn data into time-series
time.points <- seq.Date(as.Date("2012-09-02"), by = 7, length.out = 260)
data <- zoo(searchTrends[, 2:3], time.points)
head(data)

# To estimate a causal effect, we begin by specifying which period in the data should be used for training the model 
# (pre-intervention period) and which period for computing a counterfactual prediction (post-intervention period).
# Episode 3 of War on Waste at 8.30pm on Tues 30 May, therefore our post period starts from Sunday, May 28th as we 
# would like to capture all search activity that occurred right after the episode went to air.
pre.period <- as.Date(c("2012-09-02", "2017-05-21"))
post.period <- as.Date(c("2017-05-28", "2017-08-20"))

# Build the model using KeepCup and control search index, with 5000 iterations and yearly seasonality of 52 weeks
impact <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 52))
plot(impact)
impact$model
summary(impact)
summary(impact, "report")
