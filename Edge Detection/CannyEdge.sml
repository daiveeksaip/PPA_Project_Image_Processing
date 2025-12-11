structure CannyEdge =
struct

  (* Utils *)
  fun tokens line = String.tokens Char.isSpace line
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail ("Cannot parse int: " ^ s)

  fun readHeader (filename : string) : int * int * int =
    let val ins = TextIO.openIn filename val lineOpt = TextIO.inputLine ins val _ = TextIO.closeIn ins
        val line = (case lineOpt of SOME s => s | NONE => raise Fail "Empty file")
        val toks = tokens line
    in case toks of [hStr, wStr, cStr] => (toInt hStr, toInt wStr, toInt cStr) | _ => raise Fail "Bad header" end

  fun length xs = List.foldl (fn (_, acc) => acc + 1) 0 xs
  fun minList (x :: xs) = List.foldl Int.min x xs | minList [] = raise Fail "minList on empty"
  fun maxList (x :: xs) = List.foldl Int.max x xs | maxList [] = raise Fail "maxList on empty"

  type grayImage = int list list

  fun readGrayImage (filename : string) : grayImage =
    let val ins = TextIO.openIn filename
        val _ = (case TextIO.inputLine ins of NONE => raise Fail "Missing header" | SOME _ => ())
        fun parseLine line = let val toks = tokens line in List.map toInt toks end
        fun loop rows = case TextIO.inputLine ins of
            NONE => (TextIO.closeIn ins; List.rev rows)
          | SOME line => let val row = parseLine line in if List.null row then loop rows else loop (row :: rows) end
    in loop [] end

  fun dims v2 = let val h = Vector.length v2
      val w = if h = 0 then 0 else Vector.length (Vector.sub (v2, 0)) in (h, w) end

  fun toIntVec2 (img : grayImage) = Vector.fromList (List.map Vector.fromList img)
  fun toRealVec2 (img : grayImage) = Vector.fromList (List.map (fn row => Vector.fromList (List.map Real.fromInt row)) img)

  (* Blur *)
  fun gaussianBlur (img : grayImage) : grayImage =
    let val v2 = toIntVec2 img val (h, w) = dims v2
        fun getInt (y, x) = if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0 else Vector.sub (Vector.sub (v2, y), x)
        fun blurPixel (y, x) = let
            val p00 = getInt (y-1, x-1) val p01 = getInt (y-1, x) val p02 = getInt (y-1, x+1)
            val p10 = getInt (y, x-1) val p11 = getInt (y, x) val p12 = getInt (y, x+1)
            val p20 = getInt (y+1, x-1) val p21 = getInt (y+1, x) val p22 = getInt (y+1, x+1)
            val sum = 1*p00 + 2*p01 + 1*p02 + 2*p10 + 4*p11 + 2*p12 + 1*p20 + 2*p21 + 1*p22
            val valInt = sum div 16
        in if valInt < 0 then 0 else if valInt > 255 then 255 else valInt end
        fun rowLoop (y, x) = if x = w then [] else blurPixel (y, x) :: rowLoop (y, x+1)
        fun allRows y = if y = h then [] else rowLoop (y, 0) :: allRows (y+1)
    in allRows 0 end

  (* Sobel *)
  fun sobelGradients (img : grayImage) =
    let val v2 = toRealVec2 img val (h, w) = dims v2
        fun getReal (y, x) = if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0 else Vector.sub (Vector.sub (v2, y), x)
        fun compute (y, x) = let
            val p00 = getReal (y-1, x-1) val p01 = getReal (y-1, x) val p02 = getReal (y-1, x+1)
            val p10 = getReal (y, x-1) val p12 = getReal (y, x+1)
            val p20 = getReal (y+1, x-1) val p21 = getReal (y+1, x) val p22 = getReal (y+1, x+1)
            val gx = (~1.0)*p00 + 1.0*p02 + (~2.0)*p10 + 2.0*p12 + (~1.0)*p20 + 1.0*p22
            val gy = 1.0*p00 + 2.0*p01 + 1.0*p02 + ~1.0*p20 + ~2.0*p21 + ~1.0*p22
        in (gx, gy, Math.sqrt (gx*gx + gy*gy)) end
        fun buildRow (y, x, accGx, accGy, accMag) = if x = w then (List.rev accGx, List.rev accGy, List.rev accMag)
          else let val (gx, gy, mag) = compute (y, x) in buildRow (y, x+1, gx::accGx, gy::accGy, mag::accMag) end
        fun rows (y, accGxRows, accGyRows, accMagRows) = if y = h then (List.rev accGxRows, List.rev accGyRows, List.rev accMagRows)
          else let val (rowGx, rowGy, rowMag) = buildRow (y, 0, [], [], [])
               in rows (y+1, rowGx::accGxRows, rowGy::accGyRows, rowMag::accMagRows) end
        val (gxRows, gyRows, magRows) = rows (0, [], [], [])
        fun v2ofRows rs = Vector.fromList (List.map (fn r => Vector.fromList r) rs)
    in (v2ofRows gxRows, v2ofRows gyRows, v2ofRows magRows) end

  (* NMS *)
  fun isZero x = Real.abs x < 1.0E~6
  fun atan2 (y, x) = if x > 0.0 then Math.atan (y / x)
    else if x < 0.0 andalso y >= 0.0 then Math.atan (y / x) + Math.pi
    else if x < 0.0 andalso y < 0.0 then Math.atan (y / x) - Math.pi
    else if isZero x andalso y > 0.0 then Math.pi / 2.0 else if isZero x andalso y < 0.0 then ~(Math.pi / 2.0) else 0.0

  fun quantizeDir (gx, gy) = let val angle = atan2 (gy, gx) * 180.0 / Math.pi
      val a = if angle < 0.0 then angle + 180.0 else angle
  in if a < 22.5 orelse a >= 157.5 then 0 else if a < 67.5 then 1 else if a < 112.5 then 2 else 3 end

  fun nonMaxSuppression (gxV2, gyV2, magV2) =
    let val (h, w) = dims magV2
        fun magAt (y,x) = if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0 else Vector.sub (Vector.sub (magV2, y), x)
        fun dirAt (y,x) = quantizeDir (Vector.sub (Vector.sub (gxV2, y), x), Vector.sub (Vector.sub (gyV2, y), x))
        fun nmsPixel (y, x) = let val m = magAt (y, x) val d = dirAt (y, x)
            val (m1, m2) = case d of 0 => (magAt (y, x-1), magAt (y, x+1))
              | 1 => (magAt (y-1, x+1), magAt (y+1, x-1)) | 2 => (magAt (y-1, x), magAt (y+1, x))
              | _ => (magAt (y-1, x-1), magAt (y+1, x+1))
        in if m >= m1 andalso m >= m2 then m else 0.0 end
        fun buildRow (y, x, acc) = if x = w then List.rev acc else buildRow (y, x+1, nmsPixel (y, x) :: acc)
        fun buildAll (y, rows) = if y = h then List.rev rows else buildAll (y+1, buildRow (y, 0, []) :: rows)
    in Vector.fromList (List.map (fn r => Vector.fromList r) (buildAll (0, []))) end

  (* Threshold *)
  val highRatio = 0.2 val lowRatio = 0.1
  fun maxRealInVec2 v2 = let val (h, w) = dims v2
      fun loop (y, x, current) = if y = h then current else if x = w then loop (y+1, 0, current)
        else let val v = Vector.sub (Vector.sub (v2, y), x) in loop (y, x+1, if v > current then v else current) end
  in if h = 0 orelse w = 0 then 0.0 else loop (0, 0, 0.0) end

  fun classifyEdges (magNMS) =
    let val (h, w) = dims magNMS val maxVal = maxRealInVec2 magNMS
        val highThr = highRatio * maxVal val lowThr = lowRatio * maxVal
        fun classify (y, x) = let val m = Vector.sub (Vector.sub (magNMS, y), x)
        in if m >= highThr then 2 else if m >= lowThr then 1 else 0 end
        fun rowLoop (y, x, acc) = if x = w then List.rev acc else rowLoop (y, x+1, classify (y, x) :: acc)
        fun buildAll (y, rows) = if y = h then List.rev rows else buildAll (y+1, rowLoop (y, 0, []) :: rows)
    in Vector.fromList (List.map (fn r => Vector.fromList r) (buildAll (0, []))) end

  (* Hysteresis *)
  fun hysteresis (labelsV2) : grayImage =
    let val (h, w) = dims labelsV2
        fun getLab (y,x) = if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0 else Vector.sub (Vector.sub (labelsV2, y), x)
        fun isStrongNeighbor (y,x) = let
            val coords = [(y-1,x-1),(y-1,x),(y-1,x+1),(y,x-1),(y,x+1),(y+1,x-1),(y+1,x),(y+1,x+1)]
            fun existsStrong [] = false | existsStrong ((yy,xx)::rest) = if getLab (yy,xx) = 2 then true else existsStrong rest
        in existsStrong coords end
        fun finalVal (y, x) = let val lab = getLab (y,x)
        in if lab = 2 then 255 else if lab = 1 andalso isStrongNeighbor (y,x) then 255 else 0 end
        fun rowLoop (y, x, acc) = if x = w then List.rev acc else rowLoop (y, x+1, finalVal (y,x) :: acc)
        fun buildAll (y, rows) = if y = h then List.rev rows else buildAll (y+1, rowLoop (y, 0, []) :: rows)
    in buildAll (0, []) end

  (* Output *)
  fun writeGrayImage (filename, img : grayImage) =
    let val out = TextIO.openOut filename val height = length img
        val width = (case img of [] => 0 | row :: _ => length row)
        val _ = TextIO.output (out, Int.toString height ^ " " ^ Int.toString width ^ " 1\n")
        fun writeRow [] = TextIO.output (out, "\n")
          | writeRow [x] = (TextIO.output (out, Int.toString x); TextIO.output (out, "\n"))
          | writeRow (x :: xs) = (TextIO.output (out, Int.toString x ^ " "); writeRow xs)
        fun loop [] = () | loop (r :: rs) = (writeRow r; loop rs)
    in loop img; TextIO.closeOut out end

  (* Main *)
  val _ =
    let val inputFile = "image_matrix.txt" val outputFile = "canny_edges.txt"
        val (h, w, c) = readHeader inputFile
        val _ = if c <> 1 then raise Fail "Expected grayscale (C=1)" else ()
        val img = readGrayImage inputFile
        val blurred = gaussianBlur img
        val (gxV2, gyV2, magV2) = sobelGradients blurred
        val magNMS = nonMaxSuppression (gxV2, gyV2, magV2)
        val labelsV2 = classifyEdges magNMS
        val finalEdges = hysteresis labelsV2
    in writeGrayImage (outputFile, finalEdges) end

end
