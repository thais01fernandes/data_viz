
# TidyTuesday - 01/08/2022
# Download dos arquivos: 

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv')
state_name_etymology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/state_name_etymology.csv')


# Baixando o íncone do github para colocar no gráfico

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_auto()

github_icon <- "&#xf09b"
github_username <- "thais01fernandes, Fonte: #TidyTuesday"

social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: black'>{github_username}</span>"
)


# organização dos dados e plot do gráfico

state_name_etymology %>%  filter(language == "Spanish" | 
                                   language == "French" | 
                                   language == "Latin" |
                                   language == "Duth" |
                                   language == "Germanic"|
                                   language == "French (ultimately from Frankish)") %>% 
  mutate(language = case_when(language == "French (ultimately from Frankish)" ~ "French",
                              TRUE ~language)) %>% 
  left_join(states, by = "state") %>% 
  ggplot()+ 
  geom_col(aes(x = reorder(state, -population_2020), y =  population_2020, fill = language)) +
  geom_text_repel(aes(x = state, y = population_2020, label = scales::dollar(prefix = " ", round(population_2020, 1))), size = 3, hjust = 0.5, vjust = 1.4,point.padding = 0.5) +
  ggtitle("Estados dos EUA com nome de origem no francês, germanico, latin e espanhol, 
           e a população atual de cada Estado") +
  geom_label(
    label = "A California é o Estado 
    mais populoso dos EUA!", 
    x = "Florida", 
    y = 3e+07, 
    label.padding = unit(0.30, "lines"), 
    size = 3.5,
    label.size = 0.1, 
    color = "white",
    family = "serif", 
    fill = "#354B45")+
  xlab("") +
  ylab("") +
  theme_solarized() + 
  labs(caption = social_caption) +
  scale_fill_manual(name = "", values = c("#d6746f", "#c8a84e", "#9999cc", "#1aa383"), labels = c("Francês", "Germanico", "Latin", "Espanhol"))+ 
  theme(plot.title = element_text(family = "serif", size = 12, hjust = 0.5, color = "black"),
        plot.caption = element_textbox(family = "serif", size = 10, color = "black"),
        text = element_text(family = "serif", size = 14, color = "black"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 10, family = "serif", color = "black"),
        legend.position = "bottom")

