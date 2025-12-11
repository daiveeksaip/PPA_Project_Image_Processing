structure ImageSeg =
struct

  (* Utils *)
  fun tokens line = String.tokens Char.isSpace line
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail ("Cannot parse int: " ^ s)

  fun readHeader (filename : string) : int * int * int =
    let val ins = TextIO.openIn filename val lineOpt = TextIO.inputLine ins val _ = TextIO.closeIn ins
        val line = case lineOpt of SOME s => s | NONE => raise Fail "Empty file"
        val toks = tokens line
    in case toks of [hStr, wStr, cStr] => (toInt hStr, toInt wStr, toInt cStr) | _ => raise Fail "Bad header" end

  fun flatten xss = List.concat xss
  fun sum xs = List.foldl (op +) 0 xs
  fun length xs = List.foldl (fn (_, acc) => acc + 1) 0 xs

  type grayImage = int list list

  fun readGrayImage (filename : string) : grayImage =
    let val ins = TextIO.openIn filename
        val _ = case TextIO.inputLine ins of NONE => raise Fail "Missing header" | SOME _ => ()
        fun parseLine line = let val toks = tokens line in List.map toInt toks end
        fun loop rows = case TextIO.inputLine ins of
            NONE => (TextIO.closeIn ins; List.rev rows)
          | SOME line => let val row = parseLine line in if List.null row then loop rows else loop (row :: rows) end
    in loop [] end

  fun segmentGray (img : grayImage) : grayImage =
    let val flat = flatten img val total = sum flat val count = length flat
        val thresh = if count = 0 then 0 else total div count
        fun segPixel p = if p >= thresh then 255 else 0
        fun segRow row = List.map segPixel row
    in List.map segRow img end

  fun writeGrayImage (filename : string, img : grayImage) : unit =
    let val out = TextIO.openOut filename val height = length img
        val width = case img of [] => 0 | row :: _ => length row
        val _ = TextIO.output (out, Int.toString height ^ " " ^ Int.toString width ^ " 1\n")
        fun writeRow [] = TextIO.output (out, "\n")
          | writeRow [x] = (TextIO.output (out, Int.toString x); TextIO.output (out, "\n"))
          | writeRow (x :: xs) = (TextIO.output (out, Int.toString x ^ " "); writeRow xs)
        fun loop [] = () | loop (r :: rs) = (writeRow r; loop rs)
    in loop img; TextIO.closeOut out end

  type rgb = int * int * int
  type rgbImage = rgb list list

  fun readRGBImage (filename : string, width : int) : rgbImage =
    let val ins = TextIO.openIn filename
        val _ = case TextIO.inputLine ins of NONE => raise Fail "Missing header" | SOME _ => ()
        fun parseLine line =
          let val ints = List.map toInt (tokens line)
              fun group [] accPixel accRow = List.rev accRow
                | group (r :: g :: b :: rest) accPixel accRow = group rest (accPixel + 1) ((r, g, b) :: accRow)
                | group _ _ _ = raise Fail "RGB needs groups of 3"
          in group ints 0 [] end
        fun loop rows = case TextIO.inputLine ins of
            NONE => (TextIO.closeIn ins; List.rev rows)
          | SOME line => let val rowPixels = parseLine line in if List.null rowPixels then loop rows else loop (rowPixels :: rows) end
    in loop [] end

  fun intensity (r, g, b) = (r + g + b) div 3

  fun segmentRGB (img : rgbImage) : rgbImage =
    let val flat = flatten img val intensities = List.map intensity flat
        val total = sum intensities val count = length intensities
        val thresh = if count = 0 then 0 else total div count
        fun segPixel pix = if intensity pix >= thresh then (255, 255, 255) else (0, 0, 0)
        fun segRow row = List.map segPixel row
    in List.map segRow img end

  fun writeRGBImage (filename : string, img : rgbImage) : unit =
    let val out = TextIO.openOut filename val height = length img
        val width = case img of [] => 0 | row :: _ => length row
        val _ = TextIO.output (out, Int.toString height ^ " " ^ Int.toString width ^ " 3\n")
        fun writeRow [] = TextIO.output (out, "\n")
          | writeRow [(r, g, b)] = (TextIO.output (out, Int.toString r ^ " " ^ Int.toString g ^ " " ^ Int.toString b);
                                    TextIO.output (out, "\n"))
          | writeRow ((r, g, b) :: xs) = (TextIO.output (out, Int.toString r ^ " " ^ Int.toString g ^ " " ^ Int.toString b ^ " ");
                                          writeRow xs)
        fun loop [] = () | loop (r :: rs) = (writeRow r; loop rs)
    in loop img; TextIO.closeOut out end

  (* Main *)
  val _ =
    let val inputFile = "image_matrix.txt" val outputFile = "segmented_image.txt"
        val (h, w, c) = readHeader inputFile
    in case c of
        1 => let val img = readGrayImage inputFile val seg = segmentGray img in writeGrayImage (outputFile, seg) end
      | 3 => let val img = readRGBImage (inputFile, w) val seg = segmentRGB img in writeRGBImage (outputFile, seg) end
      | _ => raise Fail "Unsupported channels (only 1 or 3)"
    end

end
