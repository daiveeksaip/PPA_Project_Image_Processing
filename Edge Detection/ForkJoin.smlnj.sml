structure ForkJoin =
struct
  fun par (f, g) = (f (), g ())
end
