library(httr2)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(leaflet)
library(tidygeocoder)


#USE of APIs
client_id = "kyznT9sgkuDzFBUOuAqoMXfKumC6QhiFkAsdjZ9EAsHTOBFbJv"
client_secret = "t7yuVR6xwrxPJlB8rjb1mlrMLAw9h1TW37vilywl"

tok <- request("https://api.petfinder.com/v2/oauth2/token") |>
  req_body_form(grant_type = "client_credentials", client_id = client_id, client_secret = client_secret) |>
  req_perform() |>
  resp_body_json()

access_token <- tok$access_token

fetch_cats_page <- function(page) {
  stuff <- request("https://api.petfinder.com/v2/animals") |>
    req_auth_bearer_token(access_token) |>
    req_url_query(type = "cat", status="adoptable", location = "68523", distance = 50, sort = "distance", limit = 100, page = page) |>
    req_perform() |>
    resp_body_json()
  if (length(stuff$animals) == 0) return(NULL)
  map_dfr(stuff$animals, function(cat) {
    tibble(
      id = cat$id,
      name = cat$name,
      age = cat$age,
      gender = cat$gender,
      breed = cat$breeds$primary,
      color = cat$colors$primary,
      city = cat$contact$address$city,
      state = cat$contact$address$state,
      postcode = cat$contact$address$postcode,
      address = cat$contact$address$address1,
      url = cat$url,
      spay_neut = cat$attributes$spayed_neutered,
      house_trained = cat$attributes$house_trained,
      declawed = cat$attributes$declawed,
      shots = cat$attributes$shots_current,
      distance = cat$distance,
      org_id = cat$organization_id,
      photo = if(length (cat$photos) >0) cat$photos[[1]]$medium else NA

    )
  })
}

#polite API USE
cat_tbl <- map_dfr(1:7, ~{Sys.sleep(0.3); fetch_cats_page(.x)})


cat_tbl %>% count(color, sort=TRUE)

cat_tbl %>%
  count(color, name = "n") %>%
  filter(!is.na(color)) %>%
  ggplot(aes(x=reorder(color,n), y=n))+
  geom_col() +
  coord_flip() +
  labs(title= "cat color") +
  theme_minimal()

cat_tbl %>%
  count(age, name = "n") %>%
  filter(!is.na(age)) %>%
  ggplot(aes(x=reorder(age,n), y=n, fill = age))+
  geom_col() +
  coord_flip() +
  labs(title= "cat age") +
  theme_minimal()


#functional programming
make_pie_chart <- function(df, var, title = NULL, show_labels = TRUE) {
  tab <- df |>
    dplyr::count({{ var }}) |>
    dplyr::mutate(
      prop = n/sum(n),
      label = paste0(round(prop * 100), "%")
    )
  
  graph <- ggplot(tab, aes(x="", y=prop, fill = {{ var }})) +
    geom_col() +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title=title)
  
  if(show_labels) {
    graph <- graph +
      geom_text(
        aes(label=label),
        position = position_stack(vjust = 0.5),
        size = 3
      )
  }
  graph
}

make_pie_chart(cat_tbl, city, "Cat Colors", show_labels = TRUE)

preferred_ages <- c("Baby", "Young")


#string processing
wall <- cat_tbl %>%
  dplyr::filter(!is.na(photo)) %>%
  dplyr::slice_head(n=12) %>%
  dplyr::mutate(file= file.path("cats", paste0("cat_", id, ".jpg")),
                caption=name, alt=paste(name))

dir.create("cats", showWarnings = FALSE)
for(i in seq_len(nrow(wall))) {
  download.file(wall$photo[i], wall$file[i], mode = "wb", quiet=TRUE)
}

wall$formatted <- sprintf('![%s](%s){fig-alt="%s" width="220"}', wall$caption, wall$file, wall$alt)

yaml <- c(
  "---",
  'title: "CAT IMAGE WALL"',
  'format: html',
  "---"
)
start <- ":::{layout-ncol=3}"
end <- ":::"

lines <- c(yaml, "", start, "", wall$formatted, "", end, "")
writeLines(lines, "cat_wall.qmd")

library(quarto)
quarto::quarto_render("cats_wall.qmd")


#Interesting and interactive graphics
cat_points <- cat_tbl %>%
  mutate(address_string = ifelse(!is.na(address),
                               paste(address, city, state, postcode, "USA", sep = ", "),
                               paste(city, state, postcode, "USA", sep = ", "))) %>%
  geocode(address = address_string, method = "osm", lat=lat, long=long) %>%
  filter(!is.na(lat), !is.na(long))

leaflet(cat_points) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 3,
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste0("<b>", name, "</b><br>",
                                   ifelse(is.na(city), "", paste0(city, ifelse(is.na(state), "", paste0(", ", state)))),
                                   ifelse(is.na(photo), "", paste0("<br><img src='", photo, "' width='140'>"))))

