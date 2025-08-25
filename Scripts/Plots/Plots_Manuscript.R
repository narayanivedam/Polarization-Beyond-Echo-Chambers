library(tidyverse)

# Compile simulation outputs
temp_df <- list.files(path='~/Simulation_Output/') %>%
  lapply(read_csv) %>% bind_rows
# Store the compiled data
write_csv(temp_df,file="final.csv")
final_df <- read_csv("final.csv")

# Extract Affect/IPA/OPA of LL Configuration
ll <- final_df %>% filter(Elite == "Left Majority" & Population == "Left Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Left Majority",Population="Left Majority")

# Extract Affect/IPA/OPA of LM Configuration
lm <- final_df %>% filter(Elite == "Left Majority" & Population == "Moderate") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Left Majority") %>% mutate(Population="Moderate")

# Extract Affect/IPA/OPA of LR Configuration
lr <- final_df %>% filter(Elite == "Left Majority" & Population == "Right Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Left Majority") %>% mutate(Population="Right Majority")

rl <- final_df %>% filter(Elite == "Right Majority" & Population == "Left Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Right Majority") %>% mutate(Population="Left Majority")

# Extract Affect/IPA/OPA of RM Configuration
rm <- final_df %>% filter(Elite == "Right Majority" & Population == "Moderate" | Population=="Moderate<") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Right Majority") %>% mutate(Population="Moderate")

rr <- final_df %>% filter(Elite == "Right Majority" & Population == "Right Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Right Majority") %>% mutate(Population="Right Majority")

ml <- final_df %>% filter(Elite == "Moderate" & Population == "Left Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Moderate") %>% mutate(Population="Left Majority")

# Extract Affect/IPA/OPA of MM Configuration
mm <- final_df %>% filter(Elite == "Moderate" & Population == "Moderate") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Moderate") %>% mutate(Population="Moderate")

# Extract Affect/IPA/OPA of MR Configuration
mr <- final_df %>% filter(Elite == "Moderate" & Population == "Right Majority") %>% group_by(Time,alpha) %>% summarize(mean_a=mean(Affect),sd_a=sd(Affect),mean_i=mean(IPA),sd_i=sd(IPA),mean_o=mean(OPA),sd_o=sd(OPA)) %>% mutate(Elite="Moderate") %>% mutate(Population="Right Majority")

# Merge data
df <- bind_rows(ll,lm,lr,ml,mm,mr,rl,rm,rr)
#Labels for plots
facet_labels <- c("Affective Asymmetry:\n \u03b1=1","Affective Asymmetry:\n \u03b1=5","Affective Asymmetry:\n \u03b1=10")
names(facet_labels) <- c("1","5","10")
pop_labels <- c("Population Composition:\n Left Majority","Population Composition:\n Balanced","Population Composition:\n Right Majority")
names(pop_labels) <- c("Left Majority","Moderate","Right Majority")

# Prepare data to plot
# Create columns with upper and lower limits for Affect/IPA/OPA
df<- df %>% mutate(upper_a=mean_a+sd_a, lower_a=mean_a-sd_a, upper_i=mean_i+sd_i,lower_i=mean_i-sd_i,upper_o=mean_o+sd_o,lower_o=mean_o-sd_o)
df$upper_a[df$upper_a>100] <- 100 # Setting upper bound of Affect
df$upper_i[df$upper_i>100] <- 100 # Setting upper bound of IPA
df$lower_a[df$lower_a<0] <- 0 # Setting upper bound of Affect
df$lower_o[df$lower_o<0] <- 0 # Setting upper bound of OPA

# Plot Affect
df %>% filter(Elite %in% c("Left Majority", "Moderate", "Right Majority")) %>%
  ggplot(aes(
    x = Time,
    color = Elite,
    fill=Elite)) +
  geom_line(aes(y=mean_a),show.legend=TRUE) +
  geom_ribbon(aes(ymin=lower_a,
                  ymax=upper_a),
              alpha=0.2)+
  labs(x="Time steps",y="Mean Affect") +
  facet_grid(Population~alpha,labeller=labeller(Population=pop_labels,alpha=facet_labels))+
  guides(fill=guide_legend(title="Elite Composition"),
         color=guide_legend(title="Elite Composition")) +
  scale_color_manual(labels = c("Left Majority", "Balanced","Right Majority"), values = c( "#8da0cb","#66c2a5","#fc8d62")) +
  scale_fill_manual(labels = c("Left Majority", "Balanced","Right Majority"),values=c("#8da0cb","#66c2a5","#fc8d62")) +
  # scale_colour_manual(values=c("#66c2a5", "#fc8d62","#8da0cb")) +
  theme_bw() +geom_hline(yintercept=90, linetype="dashed", color = "#1f78b4")
ggsave("AP.svg")


# Plot IPA
df %>% filter(Elite %in% c("Left Majority", "Moderate", "Right Majority")) %>%   ggplot(aes(
  x = Time,
  color = Elite,
  fill=Elite)) +
  geom_line(aes(y=mean_i),show.legend=TRUE) +
  geom_ribbon(aes(ymin=lower_i,
                  ymax=upper_i),
              alpha=0.2)+
  labs(x="Time steps",y="Mean In-party Affect") +
  facet_grid(Population~alpha,labeller=labeller(Population=pop_labels,alpha=facet_labels))+
  guides(fill=guide_legend(title="Elite Composition"),
         color=guide_legend(title="Elite Composition")) +
  scale_color_manual(labels = c("Left Majority", "Balanced","Right Majority"), values = c("#8da0cb","#66c2a5","#fc8d62")) +
  scale_fill_manual(labels = c("Left Majority", "Balanced","Right Majority"),values=c( "#8da0cb","#66c2a5","#fc8d62")) +
  # scale_colour_manual(values=c("#66c2a5", "#fc8d62","#8da0cb")) +
  theme_bw()
ggsave("IPA.svg")

# Plot OPA
df %>% filter(Elite %in% c("Left Majority", "Moderate", "Right Majority")) %>%   ggplot(aes(
    x = Time,
    color = Elite,
    fill=Elite)) +
  geom_line(aes(y=mean_o),show.legend=TRUE) +
  geom_ribbon(aes(ymin=lower_o,
                  ymax=upper_o),
              alpha=0.2)+
  labs(x="Time steps",y="Mean Out-party Affect") +
  facet_grid(Population~alpha,labeller=labeller(Population=pop_labels,alpha=facet_labels))+
  guides(fill=guide_legend(title="Elite Composition"),
         color=guide_legend(title="Elite Composition")) +
  scale_color_manual(labels = c("Left Majority", "Balanced","Right Majority"), values = c("#8da0cb","#66c2a5","#fc8d62")) +
  scale_fill_manual(labels = c("Left Majority", "Balanced","Right Majority"),values=c("#8da0cb","#66c2a5","#fc8d62")) +
  # scale_colour_manual(values=c("#66c2a5", "#fc8d62","#8da0cb")) +
  theme_bw()

ggsave("OPA.svg")

# Prepare data to plot time taken to reach 90% affect
temp <- final_df %>% group_by(alpha,iteration,Population,Elite) %>% summarise(mean_t=which.min(abs(Affect-90)))
# NA if mean_time is more than 600
temp$mean_t[temp$mean_t>=600] <- NA
temp$Population[temp$Population=="Moderate"] <- "Balanced"
facet_labels <- c("Affective Asymmetry: \u03b1=1","Affective Asymmetry: \u03b1=5","Affective Asymmetry: \u03b1=10")
names(facet_labels) <- c("1","5","10")
pop_labels <- c("Left Majority","Balanced","Right Majority")
temp %>% group_by(alpha,Population,Elite) %>% summarise(final_t=mean(mean_t,na.rm=TRUE),sd_t=sd(mean_t,na.rm=TRUE)) %>% filter(Elite %in% c("Left Majority","Moderate","Right Majority")) %>%
  ggplot(aes(
    x = Population,
    color = Elite,
    fill = Elite,group=Elite)) +
  geom_point(aes(y=final_t),show.legend=TRUE) +
  geom_line(aes(y=final_t),linetype = "dashed") +
  geom_errorbar(aes(ymin=final_t-sd_t,ymax=final_t+sd_t),width=0.1) +
  scale_x_discrete(limits = pop_labels) +
  labs(x="Population Composition",y="Time (units)") +
  facet_wrap(~alpha,labeller=labeller(alpha=facet_labels),nrow=3)+
  guides(fill=guide_legend(title="Elite Composition"),
         color=guide_legend(title="Elite Composition")) +
  scale_color_manual(labels = c("Left Majority", "Balanced","Right Majority"), values = c("#8da0cb","#66c2a5","#fc8d62")) +
  scale_fill_manual(labels = c("Left Majority", "Balanced","Right Majority"),values=c("#8da0cb","#66c2a5","#fc8d62")) +
  theme_bw()
  ggsave("Time.svg")

# Plot Affective Asymmetric Gains
difference <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
alpha <- seq(0,1,by=0.1)
y_values <- mapply(function(x){-(3*x)*(difference)+x},alpha)
plot_data <-tibble(difference)
for (i in 0:10){
  plot_data[paste("alpha=", alpha[i+1], sep="")] <- y_values[,i+1]
}
plot_data<-plot_data %>% pivot_longer(cols = starts_with("alpha"), names_to = "alpha") %>%
  mutate(alpha = as.numeric(str_replace(alpha, "alpha=", "")))
fig <- plot_data %>%
  filter(alpha %in% c(0,0.001,0.005,0.01)) %>%
  mutate(alpha = as.factor(alpha)) %>%
  ggplot(aes(x=difference,col = alpha, fill = alpha,group=alpha)) +
  guides(fill=guide_legend(title="\u03b1"),
         color=guide_legend(title="\u03b1")) +
  geom_line(aes(y = value, col = alpha),linewidth=1) +
  scale_colour_viridis_d()+
  labs(x="Dissimilarity",y="\U0394",title="Gain profile") +
  theme_bw()+
print(fig)
ggsave(file="gains.svg", plot=fig)
