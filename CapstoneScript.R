library(dplyr)
library(readxl)
library(ggplot2)

df <- read_excel("C:/Users/sigma_phi_kappa/Desktop/Simulation_Infections.xlsx")

num_inf <- df %>% group_by(Scenario, Simulation_Iteration) %>% summarize(num_infections = n())
inf_type <- df %>% group_by(Scenario, Simulation_Iteration, Infection_Type) %>% summarize(num_infections = n())
per_type <- df %>% group_by(Scenario, Simulation_Iteration, Person_Type) %>% summarize(num_infections = n())
severity <- df %>% group_by(Scenario, Simulation_Iteration, Person_Type, Severity) %>% summarize(num_infections = n())

inf <- num_inf %>% group_by(Scenario) %>% summarize(total = sum(num_infections), median = median(num_infections), max = max(num_infections), min = min(num_infections), stdev = round(sd(num_infections)))
inftype <- inf_type %>% group_by(Scenario, Infection_Type) %>% summarize(total = sum(num_infections),median = median(num_infections), max = max(num_infections), min = min(num_infections), stdev = round(sd(num_infections)))
pertype <- per_type %>% group_by(Scenario, Person_Type) %>% summarize(total = sum(num_infections),median = median(num_infections), max = max(num_infections), min = min(num_infections), stdev = round(sd(num_infections)))
sev <- severity %>% group_by(Scenario, Person_Type, Severity) %>% summarize(total = sum(num_infections),median = median(num_infections), max = max(num_infections), min = min(num_infections), stdev = round(sd(num_infections)))

sev1 <- sev %>% filter(Scenario == "No Precautions")
sev2 <- sev %>% filter(Scenario == "Minor Precautions")
sev3 <- sev %>% filter(Scenario == "Moderate Precautions")

scenario1 = df %>% filter(df$Scenario == "No Precautions")
scenario2 = df %>% filter(df$Scenario == "Minor Precautions")
scenario3 = df %>% filter(df$Scenario == "Moderate Precautions")

scenario1data <- scenario1 %>% group_by(scenario1$Simulation_Iteration) %>% summarize(infections = n())
scenario1data %>% summarize(total = n(), max=max(infections), min=min(infections), median = median(infections))

scenario2data <- scenario2 %>% group_by(scenario2$Simulation_Iteration) %>% summarize(infections = n())
scenario2data %>% summarize(total = n(), max=max(infections), min=min(infections), median = median(infections))

scenario3data <- scenario3 %>% group_by(scenario3$Simulation_Iteration) %>% summarize(infections = n())
scenario3data %>% summarize(total = n(), max=max(infections), min=min(infections), median = median(infections))

ggplot() + geom_bar(aes(x=scenario1$Infection_Type))
ggplot() + geom_bar(aes(x=scenario2$Infection_Type))
ggplot() + geom_bar(aes(x=scenario3$Infection_Type))

ggplot() + geom_bar(aes(x=scenario1$Severity))
ggplot() + geom_bar(aes(x=scenario2$Severity))
ggplot() + geom_bar(aes(x=scenario3$Severity))
