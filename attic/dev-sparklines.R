options(width = 60)

t <- seq(0, 1, l = 201)
sincos <- map(1:5, \(i) cos(2 * pi * i * t)) |> tfd()
sincos

tfb(sincos)

sincos |> tf_sparsify()

sincos[1] <- NA
sincos

options(width = 40, digits = 2)
sincos
