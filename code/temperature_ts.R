# time series forecasting

cherry <- read.csv("../data/kyoto.csv")
# cherry <- read.csv("../data/washingtondc.csv") %>% 
#   bind_rows(read.csv("../data/liestal.csv")) %>% 
#   bind_rows(read.csv("../data/kyoto.csv"))

summary(cherry)
cherry %>% 
  group_by(location) %>% 
  slice_tail(n = 3)
