# test cores oficiais galicia
azul_bandeira <- colorspace::polarLAB(L = 70, C = 30, H = 255)
gules_rojo <- colorspace::polarLAB(H = 30, C = 65, L = 37)
azur <- colorspace::polarLAB(H = 270, C = 50, L = 40)
verde <- colorspace::polarLAB(H = 160, C = 41, L = 31)
as(azul_bandeira, "RGB")@coords |> rgb()
as(azul_bandeira, "sRGB")@coords |> rgb()
"#74b2df", "#2C71BC", "#680305", "#AB1926"
as(gules_rojo, "RGB")@coords |> rgb()
as(gules_rojo, "sRGB")@coords |> rgb()
as(azur, "RGB")@coords |> rgb()
as(azur, "sRGB")@coords |> rgb()
as(verde, "RGB")@coords |> rgb()
as(verde, "sRGB")@coords |> rgb()
rgb
