library(av)

# st_counties <- counties(state = "Alabama")
# 
# fps <- st_counties$COUNTYFP
# 
# files <- glue("vid_plots/Alabama/{fps}.png")
# last <- files[length(files)]
# 
# vec <- c(files, rep(last, 5))
# av::av_encode_video(vec)


d <- list.dirs("vid_plots")
not_dc <- d[which(!str_detect(d, "District of Columbia|vid_plots$"))]

these_dirs <- map_df(not_dc, function(i) {
  t_ <- list.files(i)
  tibble(state = i, county = t_)
})

full <- these_dirs |> 
  group_by(state) |> 
  mutate(count = n(),
         f_name = glue("{state}/{county}"),
         ind = row_number()) |> 
  arrange(count)

first <- full |> 
  filter(ind == 1)

last <- full |> 
  group_by(state) |> 
  filter(ind == max(ind))

final <- full |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(first) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  bind_rows(last) |> 
  arrange(count, state, ind)
  

av::av_encode_video(final$f_name)
