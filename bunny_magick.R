# check out this blog post
# https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/

# install.packages("ggforce") - done
# I also have magick - done
# I had to install bunny from https://github.com/dmi3kno/bunny
remotes::install_github("dmi3kno/bunny")

library(magick)
library(bunny)

# i loaded for %>% use
library(dplyr)

# update image URL at 
# https://github.com/dmi3kno/bbx/raw/master/data-raw/fantasy-4065903_1920.jpg

# load and convert to png
p2_clouds <- image_read("https://github.com/dmi3kno/bbx/raw/master/data-raw/fantasy-4065903_1920.jpg") %>%  
  image_convert("png")
p2_clouds

# save to view elsewhere
image_write(p2_clouds,
            "p2_clouds.png")

# next step - making copies and flipping them
# to place on diagonal spots
up_hand <- p2_clouds %>% 
  image_crop("1200x900+0+0") %>%          # crop
  image_convert(colorspace = "Gray") %>%  # change blue bg
  image_threshold("white", "40%") %>% # light grey to wh 
  image_threshold("black", "30%") # dark grey to black
up_hand

down_hand <- p2_clouds %>% 
  image_crop("1200x1200+750+870") %>% 
  image_convert(colorspace = "Gray") %>% 
  image_threshold("white", "40%") %>% 
  image_threshold("black", "30%")
down_hand

image_write(up_hand,
            "up_hand1.png")
image_write(down_hand,
            "down_hand1.png")

# cleaning up white space and more see blog details
up_hand <- up_hand %>%
  image_negate() %>% # flip b/w
  image_morphology("Erode", "Diamond") %>% # make lines thicker
  image_morphology("Smooth", "Disk:1.2") %>% # clean up edges smoothing
  image_negate() %>% # flip w/b
  image_transparent("white", 10) %>% # set white to transparent
  image_resize(geometry_size_pixels(700), "Lanczos2")

up_hand

# note white is now transparent
# view with non-black background
image_write(up_hand,
            "up_hand2.png")

down_hand <- down_hand %>%
  image_negate() %>% 
  image_morphology("Erode", "Diamond") %>% 
  image_morphology("Smooth", "Disk:1.2") %>% 
  image_negate() %>% 
  image_transparent("white", 10) %>% 
  image_resize(geometry_size_pixels(700), "Lanczos2")

down_hand

image_write(down_hand,
            "down_hand2.png")

# create some shadows
up_hand_shadow <- up_hand %>% 
  image_colorize(50, "grey") %>% 
  image_blur(20,10)

down_hand_shadow <- down_hand %>% 
  image_colorize(50, "grey") %>% 
  image_blur(20,10)

up_hand_shadow
down_hand_shadow

# from bunny - making the hex sticker and border
# https://coolors.co/000000-ede6f2-0d4448-c8c8c8-b3b3b3
hex_canvas <- 
  image_canvas_hex(border_color="#0d4448", 
                   border_size = 2, fill_color = "#ede6f2")

hex_border <- 
  image_canvas_hexborder(border_color="#0d4448", 
                         border_size = 4)

hex_canvas

image_write(hex_canvas,
            "hex_canvas.png")
image_write(hex_border,
            "hex_border.png")

# composing the graphic
img_hex <- hex_canvas %>%
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
  ) %>%
  bunny::image_compose(hex_border, 
                       #gravity = "center", 
                       operator = "Over")
img_hex

image_write(img_hex,
            "img_hex.png")

# look at magick functions w/o bunny
image_composite(hex_canvas,
                hex_border,
                operator = "Over",
                offset = "-50+0")

image_composite(hex_border,
                hex_canvas,
                operator = "Over")

# image_compose(hex_canvas, hex_border,
#               operator = "Over",
#               #gravity = "center",
#               offset = "-5+0")
# 
# geom <- bunny::geometry_parse(offset)[1,]

# We save two copies of the image: one for StickerMule limited to 1200x1200 pixels (it will have to fit within this bounding box) and one for github (which can be used for showing on README page). Second image goes in to man\figures folder of your package and needs to be called logo.png to be picked up by, say, packagedown.

# creating hex images for stickermule, and pkgdown
# for stickermule
img_hex %>%
  image_scale("1200x1200") %>%
  image_write(here::here("input", "bbox_hex.png"), density = 600)

# for pkgdown - set as logo.png
# would go 
img_hex %>%
  image_scale("200x200") %>%
  image_write(here::here("input", "logo.png"), density = 600)

# make github cards
img_hex_gh <- img_hex %>%
  image_scale("400x400")

gh_logo <- bunny::github %>% 
  image_scale("50x50")

img_hex_gh
gh_logo

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

gh

# save the png
gh %>% 
  image_write(here::here("input", "bbox_ghcard.png"))







# check offset
library(magick)
#> Linking to ImageMagick 6.9.10.23
#> Enabled features: fontconfig, freetype, fftw, lcms, pango, webp, x11
#> Disabled features: cairo, ghostscript, rsvg
#> Using 8 threads

# central gravity
image_blank(100, 100, color = "pink") %>%
  image_composite(image_blank(20, 50, "yellow"), 
                  gravity = "center") %>%
  image_composite(image_blank(20, 40, "green"),
                  gravity = "center",
                  offset = "+10+0") %>%
  image_composite(image_blank(20, 30, "red"),
                  gravity = "center",
                  offset = "-20+0")

image_composite(hex_border,
                hex_canvas,
                offset = "+10+5")

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

img_hex2a

image_composite(img_hex2a,
                hex_border,
                gravity = "north")

  bunny::image_compose(hex_border, 
                       #gravity = "center", 
                       operator = "Over")
img_hex2


frink <- 
  image_read("https://jeroen.github.io/images/frink.png")
pkf <- image_canvas(frink, "pink")
frink2 <- image_resize(frink, "50%")
blf2 <- image_canvas(frink2, "blue")

image_composite(pkf, frink)
image_composite(pkf, frink2)
image_composite(pkf, frink2, 
                gravity = "center")
image_composite(blf2, frink2, 
                gravity = "center")
image_composite(blf2, frink)

transbk <- image_canvas(frink, "transparent")
step1 <- image_composite(transbk, blf2,
                         operator = "Over",
                         gravity = "center")
step2 <- image_composite(step1, frink,
                         operator = "Over",
                         gravity = "center")

# try again
# img_hex2a smaller
# hex_border bigger
transhex <- 
  image_canvas(hex_border, "transparent")
step1h <- image_composite(transhex, img_hex2a,
                          operator = "Over",
                          gravity = "center")
step2h <- image_composite(step1h, hex_border,
                          operator = "Over",
                          gravity = "center")

# final cleaned up steps




