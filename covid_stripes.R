library(tidyverse)
library(lubridate)
library(ggformula)
library(classInt)
library(gganimate)



# POINT/SPLINE FINAL ------------------------------------------------------

read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv") %>% 
  filter(Fecha == "Casos nuevos totales") %>% 
  pivot_longer(cols = `2020-03-02`:ncol(.),
               names_to = "fecha",
               values_to = "casos_nuevos_d") %>% 
  rename(tipo = Fecha) %>% 
  mutate(fecha = as_date(fecha)) %>% 
  ggplot(aes(x = fecha,
             y = casos_nuevos_d)) +
  geom_point(colour = "#3A5FCD",
             alpha = 0.8) +
  geom_spline(size = 1.5,
              spar = 0,
              colour = "#CD2990") +
  geom_vline(aes(xintercept = as_date(c("2021-01-01"))),
             colour = "#CD2990",
             size = 0.5) +
  geom_vline(aes(xintercept = as_date(c("2022-01-01"))),
             colour = "#CD2990",
             size = 0.5) +
  geom_vline(aes(xintercept = min(fecha)),
             colour = "#CD2990",
             size = 0.5) +
  geom_text(aes(x = as_date(c("2021-01-01")),
                y = 20000),
            label = "2021") +
  geom_text(aes(x = as_date(c("2022-01-01")),
                y = 20000),
            label = "2022") +
  geom_text(aes(x = min(fecha),
                y = 20000),
            label = "2020") +
  scale_x_date(date_breaks = "1 month",                                    
               date_labels = "%b",
               expand = c(0.05, 0.1)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(y = "Casos nuevos COVID + \n",
       x = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
        )


ggsave(filename = here::here("plots", "covid_spline.png"),
       type = "cairo",
       scale = 1.3,
       bg = "white",
       dpi = 300,
       height = 5,
       width = 10
       )






# COVID STRIPES FINAL -----------------------------------------------------

a <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv") %>% 
  filter(Fecha == "Casos nuevos totales") %>% 
  pivot_longer(cols = `2020-03-02`:ncol(.),
               names_to = "fecha",
               values_to = "casos_nuevos_d") %>% 
  rename(tipo = Fecha) %>% 
  mutate(fecha = as_date(fecha))


# Función para normalizar datos (variables)
# se utiliza para mejorar el ajuste de colores
# a los valores de una variable en particular
min_max <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}



# Se clasifican los valores de una variable ya normalizada
ni <- round(
  classIntervals(var = a$casos_nuevos_d,
                 n = 7,
                 style = "jenks")$brks,
  digits = 0
)



# casos nuevos diarios
ggplot(a, aes(x = fecha,
              y = "y")) +
  geom_tile(aes(fill = (casos_nuevos_d))) +
  geom_vline(aes(xintercept = as_date(c("2021-01-01"))),
             colour = "white",
             size = 0.5) +
  geom_vline(aes(xintercept = as_date(c("2022-01-01"))),
             colour = "white",
             size = 0.5) +
  geom_text(aes(x = as_date(c("2021-01-01")),
                y = 0.47),
            label = "2021") +
  geom_text(aes(x = as_date(c("2022-01-01")),
                y = 0.47),
            label = "2022") +
  geom_text(aes(x = min(fecha),
                y = 0.47),
            label = "2020") +
  scale_fill_gradientn(name = "Casos nuevos COVID + \n",
                       colours = viridis::inferno(n = 100),
                       # breaks = "",
                       values = min_max(ni),
                       labels = scales::label_number(big.mark = "."),
                       # limits = c(0, 5000),
                       # oob = scales::discard,
                       guide = guide_colourbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(65, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = .5,
                         nrow = 1,
                         byrow = T,
                         reverse = F,
                         label.position = "bottom",
                         # raster = FALSE
                       )
  ) +
  scale_x_date(date_breaks = "1 month",
             date_labels = "%b",
             expand = c(0.02, 0.01)
             ) +
  scale_y_discrete(labels = NULL) +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +                                                        
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())



ggsave(filename = here::here("plots", "covid_stripes.png"),
       type = "cairo",
       scale = 1.3,
       bg = "white",
       dpi = 300,
       height = 5,
       width = 10
       )

# casos nuevos media móvil 7 días
# centered rolling mean


# WIP ---------------------------------------------------------------------


read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv") %>% 
  filter(Fecha == "Casos nuevos totales") %>% 
  pivot_longer(cols = `2020-03-02`:ncol(.),
               names_to = "fecha",
               values_to = "casos_nuevos_d") %>% 
  rename(tipo = Fecha) %>% 
  mutate(fecha = as_date(fecha),
         moving_mean_7d = zoo::rollmean(casos_nuevos_d, k = 7, fill = NA)) %>% 
  ggplot(aes(x = fecha,
             y = moving_mean_7d)) +
  # geom_point(colour = "#3A5FCD",
  #            alpha = 0.8) +
  geom_spline(size = 1.5,
              spar = 0,
              colour = "#3A5FCD") +
  geom_vline(aes(xintercept = as_date(c("2021-01-01"))),
             colour = "#CD2990",
             size = 0.5) +
  geom_vline(aes(xintercept = as_date(c("2022-01-01"))),
             colour = "#CD2990",
             size = 0.5) +
  geom_vline(aes(xintercept = min(fecha)),
             colour = "#CD2990",
             size = 0.5) +
  geom_text(aes(x = as_date(c("2021-01-01")),
                y = 20000),
            label = "2021") +
  geom_text(aes(x = as_date(c("2022-01-01")),
                y = 20000),
            label = "2022") +
  geom_text(aes(x = min(fecha),
                y = 20000),
            label = "2020") +
  scale_x_date(date_breaks = "1 month",                                    
               date_labels = "%b",
               expand = c(0.05, 0.1)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(y = "Casos nuevos COVID + \n",
       x = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
  )



ggsave(filename = "moving_average_7d.png",
       type = "cairo",
       scale = 1.3,
       bg = "white",
       dpi = 300,
       height = 5,
       width = 10
)
