library(ggplot2)
library(patchwork)
library(tidyverse)

d1 <- read.csv("questionnaire_atelier_codons_01.csv", sep=";")

q1 <- d1[1:4, ]

p1 <- ggplot(data = q1,
       aes(x = Intitule, y = Nombre)) +
  geom_col(width = 0.5, fill = "#66c1bf") +
  geom_text(aes(x = Intitule, y = Nombre + 0.3, label = Nombre),
            colour = "#275662", size = 3) +
  coord_flip() +
  labs(title = "Cet atelier a-t-il répondu à vos attentes ?", 
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8))

q2 <- d1[5:11, ] %>% 
  arrange(Nombre) %>% 
  mutate(Intitule = fct_inorder(Intitule))

p2 <- ggplot(data = q2,
       aes(x = Intitule, y = Nombre)) +
  geom_col(width = 0.5, fill = "#66c1bf") +
  geom_text(aes(x = Intitule, y = Nombre + 0.3, label = Nombre),
            colour = "#275662", size = 3) +
  coord_flip() +
  labs(title = "Parmi les items suivants, avec lesquels vous\nsentez-vous à l'aise à l'issue de cet atelier ?", 
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8))

q3 <- d1[12:46, ] %>% 
  mutate(Intitule = fct_inorder(rev(Intitule)))
  

p3 <- ggplot(data = q3,
       aes(x = Note, y = Intitule, fill = Nombre)) +
  geom_tile(show.legend = FALSE, colour = "#275662") +
  scale_fill_continuous(low = "white", high = "#66c1bf") +
  geom_text(aes(x = Note, y = rev(Intitule), label = Nombre),
            colour = "#275662", size = 3)  +
  labs(title = "Donnez une note aux items suivants entre 1 (très mauvais) et 5 (très bien)", 
       y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(colour = "#275662", hjust = 0.5, size = 10,
                                  margin = margin(b = 15)),
        axis.title.x = element_blank(),
        axis.text.y = element_text(colour = "#275662", size = 8))

p <- p1 + p2 + p3 +
  plot_layout(ncol = 2)

ggsave("reponses.png", p, dpi = 320, width = 12, height = 6)
