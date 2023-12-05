# ggplot idea
# https://t-redactyl.io/blog/2016/03/creating-plots-in-r-using-ggplot2-part-9-function-plots.html

library(xkcd)
library(extrafont)
library(ggplot2)

if('xkcd' %in% fonts()) {
  p <- ggplot() + geom_point(aes(x = mpg, y = wt), data = mtcars) +
    theme(text = element_text(size = 16, family = "xkcd"))
} else {
  warning("Not xkcd fonts installed!")
  p <- ggplot() + geom_point(aes(x = mpg, y = wt), data = mtcars)
}
p

#library(extrafont)
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
#extrafont::font_import()

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
system("mkdir ./.fonts")
system("cp xkcd.ttf  ./.fonts")
extrafont::font_import(paths = "./.fonts",
                       pattern = "[X/x]kcd",
                       prompt = FALSE)
fonts()
loadfonts(device = "win")
windowsFonts()
fonttable()

library(grDevices)
library(extrafont)
loadfonts("win", quiet = F)


library(ggthemes)

p9 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
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
  # theme(axis.line = element_line(size=1, colour = "black"),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       plot.title=element_text(size = 20, family="xkcd-Regular"),
  #       text=element_text(size = 16, family="xkcd-Regular"),
  #       axis.text.x=element_text(colour="black", size = 12),
  #       axis.text.y=element_text(colour="black", size = 12))
p9


xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
set.seed(123) # for reproducibility
p <- ggplot() + 
  geom_point(aes(mpg, wt), data=mtcars) +
  xkcdaxis(xrange,yrange) +
  theme(text = element_text(size = 16, family = "xkcd"))
  
p

ggplot() + 
  geom_point(aes(mpg, wt), data=mtcars) +
  ggtitle("a funny title") +
  theme_xkcd()




library("xkcd")
library("ggplot2")
ggplot(data = mtcars, aes(x = mpg, y = wt)) + 
  geom_point() + 
  theme(text = element_text(size = 16, family = "xkcd"))

# ========================================
# more at rgraph gallery
# https://www.r-graph-gallery.com/320-the-basis-of-bubble-plot.html

# Libraries
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)

# =========================
# Libraries
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")

# ================================
# Libraries
library(ggplot2)
library(dplyr)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="Population (M)")

# ===========================
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")


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



