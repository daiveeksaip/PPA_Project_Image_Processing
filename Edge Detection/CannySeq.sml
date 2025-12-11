structure CannySeq =
struct

  (* Utils *)
  fun tokens s = String.tokens Char.isSpace s
  fun toInt s = case Int.fromString s of SOME n => n | NONE => raise Fail "bad int"
  fun toReal n = Real.fromInt n

  (* IO *)
  fun readImage (file: string) : int array * int * int =
    let
      val ins = TextIO.openIn file
      val hdr = case TextIO.inputLine ins of SOME s => s | NONE => raise Fail "no header"
      val (h, w, c) = case tokens hdr of
                        [a,b,d] => (toInt a, toInt b, toInt d)
                      | _ => raise Fail "bad header"
      val _ = if c <> 1 then raise Fail "grayscale only" else ()
      val n = h * w
      val arr = Array.array (n, 0)
      fun rd i = if i >= n then ()
                 else case TextIO.scanStream (Int.scan StringCvt.DEC) ins of
                        SOME v => (Array.update (arr, i, Int.max(0, Int.min(255, v))); rd (i+1))
                      | NONE => raise Fail "missing pixel"
      val _ = rd 0
      val _ = TextIO.closeIn ins
    in (arr, h, w) end

  fun writeImage (file: string, pix: int array, h: int, w: int) =
    let
      val outs = TextIO.openOut file
      val _ = TextIO.output (outs, Int.toString h ^ " " ^ Int.toString w ^ " 1\n")
      val n = h * w
      fun wr i =
        if i >= n then ()
        else (TextIO.output (outs, Int.toString (Array.sub (pix, i)));
              TextIO.output (outs, if (i + 1) mod w = 0 then "\n" else " ");
              wr (i + 1))
      val _ = wr 0
      val _ = TextIO.closeOut outs
    in () end

  (* Pixel *)
  fun idx (y: int, x: int, w: int) = y * w + x

  fun getPixel (pix: int array, h: int, w: int, y: int, x: int) : int =
    if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0
    else Array.sub (pix, idx (y, x, w))

  fun getPixelReal (pix: real array, h: int, w: int, y: int, x: int) : real =
    if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0
    else Array.sub (pix, idx (y, x, w))

  (* Blur *)
  fun gaussianBlur (pix: int array, h: int, w: int) : int array =
    let
      val n = h * w
      val out = Array.array (n, 0)
      fun blur (y: int, x: int) : int =
        let
          val p00 = getPixel (pix, h, w, y-1, x-1)
          val p01 = getPixel (pix, h, w, y-1, x)
          val p02 = getPixel (pix, h, w, y-1, x+1)
          val p10 = getPixel (pix, h, w, y, x-1)
          val p11 = getPixel (pix, h, w, y, x)
          val p12 = getPixel (pix, h, w, y, x+1)
          val p20 = getPixel (pix, h, w, y+1, x-1)
          val p21 = getPixel (pix, h, w, y+1, x)
          val p22 = getPixel (pix, h, w, y+1, x+1)
          val sum = 1*p00 + 2*p01 + 1*p02 + 2*p10 + 4*p11 + 2*p12 + 1*p20 + 2*p21 + 1*p22
          val v = sum div 16
        in Int.max (0, Int.min (255, v)) end
      fun process i =
        if i >= n then ()
        else let val y = i div w val x = i mod w
             in Array.update (out, i, blur (y, x)); process (i+1) end
    in process 0; out end

  (* Sobel *)
  fun sobelGradients (pix: int array, h: int, w: int) : real array * real array * real array =
    let
      val n = h * w
      val gx = Array.array (n, 0.0)
      val gy = Array.array (n, 0.0)
      val mag = Array.array (n, 0.0)
      fun compute (y: int, x: int) =
        let
          val p00 = toReal (getPixel (pix, h, w, y-1, x-1))
          val p01 = toReal (getPixel (pix, h, w, y-1, x))
          val p02 = toReal (getPixel (pix, h, w, y-1, x+1))
          val p10 = toReal (getPixel (pix, h, w, y, x-1))
          val p12 = toReal (getPixel (pix, h, w, y, x+1))
          val p20 = toReal (getPixel (pix, h, w, y+1, x-1))
          val p21 = toReal (getPixel (pix, h, w, y+1, x))
          val p22 = toReal (getPixel (pix, h, w, y+1, x+1))
          val gxVal = ~1.0*p00 + 1.0*p02 + ~2.0*p10 + 2.0*p12 + ~1.0*p20 + 1.0*p22
          val gyVal = 1.0*p00 + 2.0*p01 + 1.0*p02 + ~1.0*p20 + ~2.0*p21 + ~1.0*p22
          val magVal = Math.sqrt (gxVal*gxVal + gyVal*gyVal)
        in (gxVal, gyVal, magVal) end
      fun process i =
        if i >= n then ()
        else let val y = i div w val x = i mod w
                 val (gxV, gyV, magV) = compute (y, x)
             in Array.update (gx, i, gxV); Array.update (gy, i, gyV);
                Array.update (mag, i, magV); process (i+1) end
    in process 0; (gx, gy, mag) end

  (* NMS *)
  fun isZero (x: real) = Real.abs x < 1.0E~6

  fun atan2 (y: real, x: real) : real =
    if x > 0.0 then Math.atan (y / x)
    else if x < 0.0 andalso y >= 0.0 then Math.atan (y / x) + Math.pi
    else if x < 0.0 andalso y < 0.0 then Math.atan (y / x) - Math.pi
    else if isZero x andalso y > 0.0 then Math.pi / 2.0
    else if isZero x andalso y < 0.0 then ~(Math.pi / 2.0)
    else 0.0

  fun quantizeDir (gxVal: real, gyVal: real) : int =
    let
      val angle = atan2 (gyVal, gxVal) * 180.0 / Math.pi
      val a = if angle < 0.0 then angle + 180.0 else angle
    in
      if a < 22.5 orelse a >= 157.5 then 0
      else if a < 67.5 then 1
      else if a < 112.5 then 2
      else 3
    end

  fun nonMaxSuppression (gx: real array, gy: real array, mag: real array, h: int, w: int) : real array =
    let
      val n = h * w
      val out = Array.array (n, 0.0)
      fun nms (y: int, x: int) : real =
        let
          val i = idx (y, x, w)
          val m = Array.sub (mag, i)
          val d = quantizeDir (Array.sub (gx, i), Array.sub (gy, i))
          val (m1, m2) = case d of
              0 => (getPixelReal (mag, h, w, y, x-1), getPixelReal (mag, h, w, y, x+1))
            | 1 => (getPixelReal (mag, h, w, y-1, x+1), getPixelReal (mag, h, w, y+1, x-1))
            | 2 => (getPixelReal (mag, h, w, y-1, x), getPixelReal (mag, h, w, y+1, x))
            | _ => (getPixelReal (mag, h, w, y-1, x-1), getPixelReal (mag, h, w, y+1, x+1))
        in if m >= m1 andalso m >= m2 then m else 0.0 end
      fun process i =
        if i >= n then ()
        else let val y = i div w val x = i mod w
             in Array.update (out, i, nms (y, x)); process (i+1) end
    in process 0; out end

  (* Threshold *)
  val HIGH_RATIO = 0.2
  val LOW_RATIO = 0.1

  fun findMax (arr: real array) : real =
    let val n = Array.length arr
        fun lp i m = if i >= n then m else lp (i+1) (Real.max (m, Array.sub (arr, i)))
    in lp 0 0.0 end

  fun classifyEdges (magNMS: real array, h: int, w: int) : int array =
    let
      val n = h * w
      val maxVal = findMax magNMS
      val highThr = HIGH_RATIO * maxVal
      val lowThr = LOW_RATIO * maxVal
      val labels = Array.array (n, 0)
      fun classify i =
        if i >= n then ()
        else let val m = Array.sub (magNMS, i)
                 val lab = if m >= highThr then 2 else if m >= lowThr then 1 else 0
             in Array.update (labels, i, lab); classify (i+1) end
    in classify 0; labels end

  fun getLab (labels: int array, h: int, w: int, y: int, x: int) : int =
    if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0
    else Array.sub (labels, idx (y, x, w))

  fun hasStrongNeighbor (labels: int array, h: int, w: int, y: int, x: int) : bool =
    getLab (labels, h, w, y-1, x-1) = 2 orelse getLab (labels, h, w, y-1, x) = 2 orelse
    getLab (labels, h, w, y-1, x+1) = 2 orelse getLab (labels, h, w, y, x-1) = 2 orelse
    getLab (labels, h, w, y, x+1) = 2 orelse getLab (labels, h, w, y+1, x-1) = 2 orelse
    getLab (labels, h, w, y+1, x) = 2 orelse getLab (labels, h, w, y+1, x+1) = 2

  (* Hysteresis *)
  fun hysteresis (labels: int array, h: int, w: int) : int array =
    let
      val n = h * w
      val out = Array.array (n, 0)
      fun hyst i =
        if i >= n then ()
        else let val y = i div w val x = i mod w
                 val lab = Array.sub (labels, i)
                 val v = if lab = 2 then 255
                         else if lab = 1 andalso hasStrongNeighbor (labels, h, w, y, x) then 255 else 0
             in Array.update (out, i, v); hyst (i+1) end
    in hyst 0; out end

  (* Pipeline *)
  fun canny (pix: int array, h: int, w: int) : int array =
    let
      val blurred = gaussianBlur (pix, h, w)
      val (gx, gy, mag) = sobelGradients (blurred, h, w)
      val magNMS = nonMaxSuppression (gx, gy, mag, h, w)
      val labels = classifyEdges (magNMS, h, w)
      val edges = hysteresis (labels, h, w)
    in edges end

  (* Main *)
  fun main () =
    let
      val args = CommandLine.arguments ()
      val (inFile, outFile) = case args of
          [i, outF] => (i, outF)
        | [i] => (i, "canny_edges.txt")
        | _ => ("image_matrix.txt", "canny_edges.txt")
      val _ = print ("Canny Edge Detection (Sequential)\n")
      val _ = print ("  Input:  " ^ inFile ^ "\n")
      val _ = print ("  Output: " ^ outFile ^ "\n")
      val (pix, h, w) = readImage inFile
      val n = h * w
      val _ = print ("  Size:   " ^ Int.toString w ^ "x" ^ Int.toString h ^ " (" ^ Int.toString n ^ " pixels)\n")
      val start = Time.now ()
      val edges = canny (pix, h, w)
      val elapsed = Time.toReal (Time.- (Time.now (), start))
      val _ = print ("  Time:   " ^ Real.fmt (StringCvt.FIX (SOME 4)) elapsed ^ " s\n")
      val _ = writeImage (outFile, edges, h, w)
      val _ = print ("  Done.\n")
    in () end

  val _ = main ()

end
