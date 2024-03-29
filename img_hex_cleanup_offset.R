# cleaned up steps to fix offset issues
library(magick)
library(bunny)
library(dplyr)

p2_clouds <- 
  image_read("https://github.com/dmi3kno/bbx/raw/master/data-raw/fantasy-4065903_1920.jpg") %>%  
  image_convert("png")
#p2_clouds

up_hand <- p2_clouds %>% 
  image_crop("1200x900+0+0") %>%          # crop
  image_convert(colorspace = "Gray") %>%  # change blue bg
  image_threshold("white", "40%") %>% # light grey to wh 
  image_threshold("black", "30%") # dark grey to black

down_hand <- p2_clouds %>% 
  image_crop("1200x1200+750+870") %>% 
  image_convert(colorspace = "Gray") %>% 
  image_threshold("white", "40%") %>% 
  image_threshold("black", "30%")

# cleaning up white space and more see blog details
up_hand <- up_hand %>%
  image_negate() %>% # flip b/w
  image_morphology("Erode", "Diamond") %>% 
  image_morphology("Smooth", "Disk:1.2") %>% 
  image_negate() %>% # flip w/b
  image_transparent("white", 10) %>% 
  image_resize(geometry_size_pixels(700), "Lanczos2")

#up_hand

down_hand <- down_hand %>%
  image_negate() %>% 
  image_morphology("Erode", "Diamond") %>% 
  image_morphology("Smooth", "Disk:1.2") %>% 
  image_negate() %>% 
  image_transparent("white", 10) %>% 
  image_resize(geometry_size_pixels(700), "Lanczos2")

#down_hand

# create some shadows
up_hand_shadow <- up_hand %>% 
  image_colorize(50, "grey") %>% 
  image_blur(20,10)

down_hand_shadow <- down_hand %>% 
  image_colorize(50, "grey") %>% 
  image_blur(20,10)

hex_canvas <- 
  image_canvas_hex(border_color="#0d4448", 
                   border_size = 2, fill_color = "#ede6f2")

hex_border <- 
  image_canvas_hexborder(border_color="#0d4448", 
                         border_size = 4)

# composing the graphic
img_hex2a <- hex_canvas %>%
  bunny::image_compose(up_hand_shadow, 
                       offset = "+40+460", 
                       gravity = "northwest") %>%
  bunny::image_compose(down_hand_shadow, 
                       offset = "+30+390", 
                       gravity = "southeast") %>%
  bunny::image_compose(up_hand, 
                       offset = "+20+440", 
                       gravity = "northwest") %>%
  bunny::image_compose(down_hand, 
                       offset = "+20+380", 
                       gravity = "southeast") %>%
  image_annotate(
    "bbox",
    size = 450,
    gravity = "center",
    font = "Aller",
    color = "#0d4448"
  ) 

transhex <- 
  image_canvas(hex_border, "transparent")
step1h <- image_composite(transhex, img_hex2a,
                          operator = "Over",
                          gravity = "center")
step2h <- image_composite(step1h, hex_border,
                          operator = "Over",
                          gravity = "center")
img_hex <- step2h

# creating hex images for stickermule, and pkgdown
# for stickermule
img_hex %>%
  image_scale("1200x1200") %>%
  image_write(here::here("input", "bbox_hex_fix.png"), density = 600)

# for pkgdown - set as logo.png
# would go 
img_hex %>%
  image_scale("200x200") %>%
  image_write(here::here("input", "logo_fix.png"), density = 600)

# make github cards
img_hex_gh <- img_hex %>%
  image_scale("400x400")

gh_logo <- bunny::github %>% 
  image_scale("50x50")

#img_hex_gh
#gh_logo

# put pieces together

gh <- image_canvas_ghcard("#ede6f2") %>%
  image_compose(img_hex_gh, gravity = "East", 
                offset = "+100+0") %>%
  image_annotate(
    "Frame your world",
    gravity = "West",
    location = "+100-30",
    color = "#0d4448",
    size = 60,
    font = "Aller",
    weight = 700
  ) %>%
  image_compose(gh_logo, 
                gravity = "West", 
                offset = "+100+40") %>%
  image_annotate(
    "dmi3kno/bbox",
    gravity = "West",
    location = "+160+45",
    size = 50,
    font = "Ubuntu Mono"
  ) %>%
  image_border_ghcard("#ede6f2")

#gh

# save the png
gh %>% 
  image_write(here::here("input", "bbox_ghcard_fix.png"))