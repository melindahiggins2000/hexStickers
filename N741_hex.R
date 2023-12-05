# N741_hex
library(magick)
library(bunny)
library(dplyr)

emory1 <- 
  image_read("./EmoryUniversity vt/EmoryUniversity vt/Shield/jpg_png/EU_shield_vt_280.png")

# emory1 <-
#   emory1 %>%
#   image_negate()

emory_canvas <- 
  image_canvas_hex(border_color="#13216a", 
                   border_size = 2, fill_color = "#ede6f2")

emory_border <- 
  image_canvas_hexborder(border_color="#13216a", 
                         border_size = 4)

emory1_sm <- image_resize(emory1,
                          geometry_size_percent(30))

emory_hex <- 
  image_composite(emory_canvas,
                  emory1_sm,
                  operator = "Over",
                  gravity = "center",
                  offset = "+0+600")
emory_hex

# ===========================
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(palmerpenguins)

# The dataset is provided in the gapminder library
# library(gapminder)
# data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

data <- penguins

# Most basic bubble plot
p1 <- data %>%
  #arrange(desc(pop)) %>%
  #mutate(country = factor(country, country)) %>%
  ggplot(aes(x=bill_length_mm, y=bill_depth_mm, 
             size=body_mass_g, fill=species)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 6), 
             name="Body Mass (g)",
             guide = FALSE) +
  scale_fill_viridis(discrete=TRUE, 
                     guide="legend", 
                     name="",
                     option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("") +
  xlab("") +
  theme(legend.position = "none")

# library(palmerpenguins)
# library(ggplot2)
# bill_len_dep <- ggplot(data = penguins,
#                        aes(x = bill_length_mm,
#                            y = bill_depth_mm,
#                            group = species)) +
#   geom_point(aes(color = species,
#                  shape = species),
#              size = 3,
#              alpha = 0.8) +
#   geom_smooth(method = "lm", se = FALSE, 
#               aes(color = species)) +
#   theme_minimal() +
#   scale_color_manual(values = c("darkorange",
#                                 "purple","cyan4")) +
#   labs(title = "Penguin bill dimensions",
#        subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
#        x = "Bill length (mm)",
#        y = "Bill depth (mm)",
#        color = "Penguin species",
#        shape = "Penguin species") +
#   theme(legend.position = c(0.85, 0.15),
#         legend.background = element_rect(fill = "white", 
#                                          color = NA),
#         plot.title.position = "plot",
#         plot.caption = element_text(hjust = 0, 
#                                     face= "italic"),
#         plot.caption.position = "plot")
# 
# bill_len_dep

p1

ggsave(here::here("input", "pp1.png"), 
       width = 5.5, height=5.5)

pp1 <- 
  image_read(here::here("input", "pp1.png"))

pp1_sm <- image_resize(pp1,
                       geometry_size_percent(50))

emory_hex2 <- 
  image_composite(emory_hex,
                  pp1_sm,
                  operator = "Over",
                  gravity = "center",
                  offset = "+0-400")
emory_hex2

transhex <- 
  image_canvas(emory_border, "transparent")
step1h <- image_composite(transhex, emory_hex2,
                          operator = "Over",
                          gravity = "center")
step2h <- image_composite(step1h, emory_border,
                          operator = "Over",
                          gravity = "center")
img_hex <- step2h
img_hex

p2 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(0.2, 0.1),
                aes(colour = "Group 1"), size = 1.5) +
  stat_function(fun = dnorm, args = list(0.7, 0.05),
                aes(colour = "Group 2"), size = 1.5) +
  scale_x_continuous(name = "Probability",
                     breaks = seq(0, 1, 0.2),
                     limits=c(0, 1)) +
  scale_y_continuous(name = "Frequency") +
  #ggtitle("Normal function curves of probabilities") +
  scale_colour_brewer(palette="Set1") +
  labs(colour = "Groups") +
  theme_tufte()
p2

funcShaded <- function(x) {
  y <- dnorm(x, mean = 0.2, sd = 0.1)
  y[x < 0.2 | x > (0.2 + 4 * 0.1)] <- NA
  return(y)
}

p3 <- p2 + 
  stat_function(fun=funcShaded, 
                geom="area", 
                fill="#84CA72", 
                alpha=0.2)
p3

# https://sebastiansauer.github.io/simple-shading/
p4 <- ggplot(NULL, aes(c(-3,3))) +
  geom_area(stat = "function", 
            fun = dnorm, fill = "#00998a", 
            xlim = c(-3, 0)) +
  geom_area(stat = "function", fun = dnorm, 
            fill = "grey80", xlim = c(0, 3))
p4

p5 <- ggplot(NULL, aes(c(-3,3))) +
  geom_area(stat = "function", fun = dnorm, 
            args = list(mean=0, sd=2),
            fill = "#13216a", xlim = c(-2,2)) +
  #geom_area(stat = "function", fun = dnorm, 
  #          fill = "grey80", xlim = c(2, 3)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = NULL)
p5

#https://www.statology.org/ggplot-transparent-background/
p5 +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend panel
  )

ggsave(here::here("input", "p5.png"), 
       bg="transparent",
       width = 5.5, height=5.5)

p5png <- 
  image_read(here::here("input", "p5.png"))


img_hex2 <- 
  image_composite(img_hex, 
                  image_blank(1200, 10, "#13216a"),
                  operator = "Over",
                  gravity = "center")
img_hex2

img_hex3 <- 
  image_composite(img_hex2, 
                  image_blank(1200, 10, "#13216a"),
                  operator = "Over",
                  gravity = "center",
                  offset = "+0+300")
img_hex3

img_hex4 <- img_hex3 %>%
  image_annotate("NRSG 741", 
                 gravity = "center", location = "+10+150", 
                 size=300, font="Dosis", color="#13216a")
  
img_hex4

# font="Aller"
# Oswald
# Comic Sans
# Bauhaus 93
# Century Gothic
# Impact
# Lato
# Tunga
# Montserrat


image_compose(emory_canvas,
              p5png,
              gravity = "center",
              offset = "+0+400")

# =======================
# creating hex images for stickermule, and pkgdown
# for stickermule
img_hex4 %>%
  image_scale("1200x1200") %>%
  image_write(here::here("input", "n741_hex4.png"), density = 600)

# for pkgdown - set as logo.png
# would go 
img_hex4 %>%
  image_scale("200x200") %>%
  image_write(here::here("input", "logo_n741_hex4.png"), density = 600)

# make github cards
n741_hex_gh <- img_hex4 %>%
  image_scale("400x400")

gh_logo <- bunny::github %>% 
  image_scale("50x50")

#img_hex_gh
#gh_logo

# put pieces together

gh <- image_canvas_ghcard("#ede6f2") %>%
  image_compose(n741_hex_gh, gravity = "East",
                offset = "+100+0") %>%
  image_annotate(
    "Big Data Analytics",
    gravity = "West",
    location = "+100-120",
    color = "#13216a",
    size = 60,
    font = "Dosis",
    weight = 700
  ) %>%
  image_annotate(
    "for Healthcare",
    gravity = "West",
    location = "+100-60",
    color = "#13216a",
    size = 60,
    font = "Dosis",
    weight = 700
  ) %>%
  image_compose(gh_logo,
                gravity = "West",
                offset = "+100+50") %>%
  image_annotate(
    "melindahiggins2000",
    gravity = "West",
    location = "+160+55",
    size = 50,
    font = "Ubuntu Mono"
  ) %>%
  image_compose(gh_logo,
                gravity = "West",
                offset = "+100+110") %>%
  image_annotate(
    "vhertzb",
    gravity = "West",
    location = "+160+115",
    size = 50,
    font = "Ubuntu Mono"
  ) %>%
  image_border_ghcard("#13216a")

gh

# save the png
gh %>% 
  image_write(here::here("input", "n741_ghcard.png"))

