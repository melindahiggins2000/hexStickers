#gganimate_hex example
library(gapminder)
library(tidyverse)
library(ggrepel)
library(hrbrthemes)

lbl_countries <- c("Congo, Dem. Rep.", 
                   "China", "Japan", 
                   "United States", "South Africa")
gapminder2007 <- 
  gapminder %>% filter(year==2007) 

gapminder2007 %>% 
  ggplot(aes(gdpPercap, lifeExp, 
             size = pop, colour = country)) +
  annotate("text", x=4000, y=65, 
           label="2007", size=50, 
           colour="lightgrey", alpha=0.5) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_label_repel(data=filter(gapminder2007, 
                               country %in% lbl_countries), 
                   aes(gdpPercap, lifeExp, label=country), 
                   inherit.aes = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(
    minor_breaks=rep(1:9, 21)*(10^rep(-10:10, each=9))) +
  theme_ipsum_rc(grid_col = "grey90", axis_title_just = 'm')+
  labs( x = 'GDP per capita', y = 'life expectancy')

ggsave(here::here("input", "gapminder2007.png"), 
       width = 5.5, height=5.5)

# remotes::install_github("ropensci/magick")
library(magick)
# remotes::install_github("dmi3kno/bunny")
library(bunny)

canvas_hex <- 
  bunny::image_canvas_hex(border_color = "#523456", 
                          fill_color = "white")
canvas_border <- 
  bunny::image_canvas_hexborder(border_size = 4, 
                                border_color = "#523456")

gap07 <- 
  image_read(here::here("input", "gapminder2007.png"))

gap_logo <- 
  # I added to register the background border size first
  image_canvas(canvas_border, "transparent") %>%
  # then overlay the hex composite
  image_composite(canvas_hex,
                  operator = "Over",
                  gravity = "center") %>%
  # then add the gapminder image
  image_composite(
    #canvas_hex, 
    gap07, 
    gravity = "center", offset = "-30+0") %>% 
  # add the text
  image_annotate("gapminder", 
                 gravity = "center", location = "+10+150", 
                 size=300, font="Aller", color="#523456") %>% 
  # now add the border overlay
  image_composite(canvas_border, gravity = "center") 

gap_logo %>% image_scale("500x500") #for screen only

gap_logo %>% 
  image_scale("1200x1200") %>% 
  image_write(here::here("input", 
                         "gapminder_logo_big.png"), density = 600)

gap_logo %>% 
  image_scale("200x200") %>% 
  image_write(here::here("input", 
                         "gapminder_logo_small.png"), density = 96)

