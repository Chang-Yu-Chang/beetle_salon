#' Train a CNN model for dorsal views

library(tidyverse)
library(imager)

folder_dorsal <- "~/Dropbox/lab/beetle_salon/data/dorsal/"
folder_processed <- paste0(folder_dorsal, "processed/")
if (!dir.exists(folder_processed)) dir.create(folder_processed, recursive = T)

filenames <- list.files(paste0(folder_dorsal, "raw"))
cleannames <- str_remove(filenames, "-D\\.\\w+")

#i = 10

for (i in 1:length(filenames)) {
# Folder
folder_i <- paste0(folder_dorsal, "processed/", cleannames[i], "/")
if (!dir.exists(folder_i)) dir.create(folder_i)

# Raw image and cut
img <- load.image(paste0(folder_dorsal, "raw/", filenames[i])) %>%
    imsub(y < dim(.)[2]*.7) %>%  # Crop out the lower 30%
    imresize(.5)

save.image(img, paste0(folder_i, "00-raw.png"))

# Extract only one channel
img_gray <- img %>%
    grayscale()
#plot(img_gray)
save.image(img_gray, paste0(folder_i, "01-gray.png"))

# Create a mask
img_mask <- img_gray %>%
    threshold(approx = T, thr = .4) %>%
    {1-.} %>%
    fill(5) %>%
    erode_square(5) %>%
    dilate_square(20) %>%
    # Remove pixel sets that touch the border
    as.pixset() %>%
    px.remove_outer() %>%
    as.cimg()

## Remove small pixelsets
labeled <- label(img_mask)
counts <- table(labeled)
size <- 1000
big_labels <- as.numeric(names(counts)[counts >= size])
filtered_mask <- as.cimg(labeled %in% big_labels, x = width(img_mask), y = height(img_mask))

img_mask <- img_mask * filtered_mask
save.image(img_mask, paste0(folder_i, "02-mask.png"))

# Select only the beetles using the mask and remove unnecessary background
coords <- which(img_mask == 1, arr.ind = T) %>% as_tibble
xmin <- min(coords[,1])
xmax <- max(coords[,1])
ymin <- min(coords[,2])
ymax <- max(coords[,2])

img_cropped <- (img_gray * img_mask) %>%
    imsub(x %in% xmin:xmax, y %in% ymin:ymax)

save.image(img_cropped, paste0(folder_i, "03-cropped.png"))

}

# Consolidate cropped images
if (!dir.exists(paste0(folder_dorsal, "cropped"))) dir.create(paste0(folder_dorsal, "cropped"))

for (i in 1:length(filenames)) {
    file.copy(
        from = paste0(folder_dorsal, "processed/", cleannames[i], "/03-cropped.png"),
        to = paste0(folder_dorsal, "cropped/", cleannames[i], ".png"),
    )
}


if (F) {

    # Rotate the image along the main axis
    ## Detect edges
    edges <- cannyEdges(img_cropped, sigma=1)
    ## Use Hough transform to find lines
    lines <- hough_line(edges, ntheta = 1000, data.frame = T) %>%
        as_tibble %>%
        filter(score != 0)
    ## Find the line that represents the main axis
    votes <- sort(table(lines$theta))
    angle_deg <- as.numeric(last(names(votes))) * (180 / pi)
    img_rotated <- imrotate(img_cropped, angle_deg+90)


    save.image(img_rotated, paste0(folder_i, "04-rotated.png"))
}
