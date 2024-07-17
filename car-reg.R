library(tidyverse)

url <- "http://www.revenue.ie/en/corporate/documents/statistics/registrations/vehicle-registration.csv"
z <- read_csv(url) %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  mutate(vehicle_category = str_remove(vehicle_category, "^Category [A-Z] "))
  

z %>% group_by(year, vehicle_condition) %>% 
  summarise(total = sum(vehicle_registrations)) %>% 
  pivot_wider(names_from = vehicle_condition, values_from = total)

z %>% group_by(vehicle_category, vehicle_condition) %>% 
  summarise(total = sum(vehicle_registrations)) %>% 
  pivot_wider(names_from = vehicle_condition, values_from = total) %>% 
  rowwise() %>% 
  mutate(Ratio = New / Used)

z %>% group_by(vehicle_category, vehicle_condition) %>% 
  summarise(cars = sum(vehicle_registrations),
            tax = sum(vehicle_receipts)) %>% 
  rowwise() %>% 
  mutate(tax_per_vehicle = tax/cars) %>% 
  select(-cars, -tax) %>% 
  pivot_wider(names_from = vehicle_condition, values_from = tax_per_vehicle) %>% 
  rowwise() %>% 
  mutate(Ratio = New / Used)
