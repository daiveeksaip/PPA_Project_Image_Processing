structure CannyCorrectness =
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
      val (h, w, c) = case tokens hdr of [a,b,d] => (toInt a, toInt b, toInt d) | _ => raise Fail "bad header"
      val _ = if c <> 1 then raise Fail "grayscale only" else ()
      val n = h * w
      val arr = Array.array (n, 0)
      fun rd i = if i >= n then ()
                 else case TextIO.scanStream (Int.scan StringCvt.DEC) ins of
                        SOME v => (Array.update (arr, i, Int.max(0, Int.min(255, v))); rd (i+1))
                      | NONE => raise Fail "missing pixel"
      val _ = rd 0 val _ = TextIO.closeIn ins
    in (arr, h, w) end

  (* Pixel *)
  fun idx (y: int, x: int, w: int) = y * w + x
  fun getPixel (pix: int array, h: int, w: int, y: int, x: int) : int =
    if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0 else Array.sub (pix, idx (y, x, w))
  fun getPixelReal (pix: real array, h: int, w: int, y: int, x: int) : real =
    if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0.0 else Array.sub (pix, idx (y, x, w))

  (* Sequential *)
  fun gaussianBlurSeq (pix: int array, h: int, w: int) : int array =
    let val n = h * w val out = Array.array (n, 0)
        fun blur (y, x) = let val p00 = getPixel (pix, h, w, y-1, x-1) val p01 = getPixel (pix, h, w, y-1, x)
            val p02 = getPixel (pix, h, w, y-1, x+1) val p10 = getPixel (pix, h, w, y, x-1)
            val p11 = getPixel (pix, h, w, y, x) val p12 = getPixel (pix, h, w, y, x+1)
            val p20 = getPixel (pix, h, w, y+1, x-1) val p21 = getPixel (pix, h, w, y+1, x)
            val p22 = getPixel (pix, h, w, y+1, x+1)
            val sum = 1*p00 + 2*p01 + 1*p02 + 2*p10 + 4*p11 + 2*p12 + 1*p20 + 2*p21 + 1*p22
        in Int.max (0, Int.min (255, sum div 16)) end
        fun process i = if i >= n then () else (Array.update (out, i, blur (i div w, i mod w)); process (i+1))
    in process 0; out end

  fun sobelSeq (pix: int array, h: int, w: int) : real array * real array * real array =
    let val n = h * w val gx = Array.array (n, 0.0) val gy = Array.array (n, 0.0) val mag = Array.array (n, 0.0)
        fun compute (y, x) = let val p00 = toReal (getPixel (pix, h, w, y-1, x-1)) val p01 = toReal (getPixel (pix, h, w, y-1, x))
            val p02 = toReal (getPixel (pix, h, w, y-1, x+1)) val p10 = toReal (getPixel (pix, h, w, y, x-1))
            val p12 = toReal (getPixel (pix, h, w, y, x+1)) val p20 = toReal (getPixel (pix, h, w, y+1, x-1))
            val p21 = toReal (getPixel (pix, h, w, y+1, x)) val p22 = toReal (getPixel (pix, h, w, y+1, x+1))
            val gxV = ~1.0*p00 + 1.0*p02 + ~2.0*p10 + 2.0*p12 + ~1.0*p20 + 1.0*p22
            val gyV = 1.0*p00 + 2.0*p01 + 1.0*p02 + ~1.0*p20 + ~2.0*p21 + ~1.0*p22
        in (gxV, gyV, Math.sqrt (gxV*gxV + gyV*gyV)) end
        fun process i = if i >= n then () else let val (gxV, gyV, magV) = compute (i div w, i mod w)
             in Array.update (gx, i, gxV); Array.update (gy, i, gyV); Array.update (mag, i, magV); process (i+1) end
    in process 0; (gx, gy, mag) end

  fun isZero x = Real.abs x < 1.0E~6
  fun atan2 (y, x) = if x > 0.0 then Math.atan (y / x)
    else if x < 0.0 andalso y >= 0.0 then Math.atan (y / x) + Math.pi
    else if x < 0.0 andalso y < 0.0 then Math.atan (y / x) - Math.pi
    else if isZero x andalso y > 0.0 then Math.pi / 2.0
    else if isZero x andalso y < 0.0 then ~(Math.pi / 2.0) else 0.0
  fun quantizeDir (gxV, gyV) = let val angle = atan2 (gyV, gxV) * 180.0 / Math.pi
      val a = if angle < 0.0 then angle + 180.0 else angle
  in if a < 22.5 orelse a >= 157.5 then 0 else if a < 67.5 then 1 else if a < 112.5 then 2 else 3 end

  fun nmsSeq (gx: real array, gy: real array, mag: real array, h: int, w: int) : real array =
    let val n = h * w val out = Array.array (n, 0.0)
        fun nms (y, x) = let val i = idx (y, x, w) val m = Array.sub (mag, i)
            val d = quantizeDir (Array.sub (gx, i), Array.sub (gy, i))
            val (m1, m2) = case d of 0 => (getPixelReal (mag, h, w, y, x-1), getPixelReal (mag, h, w, y, x+1))
              | 1 => (getPixelReal (mag, h, w, y-1, x+1), getPixelReal (mag, h, w, y+1, x-1))
              | 2 => (getPixelReal (mag, h, w, y-1, x), getPixelReal (mag, h, w, y+1, x))
              | _ => (getPixelReal (mag, h, w, y-1, x-1), getPixelReal (mag, h, w, y+1, x+1))
        in if m >= m1 andalso m >= m2 then m else 0.0 end
        fun process i = if i >= n then () else (Array.update (out, i, nms (i div w, i mod w)); process (i+1))
    in process 0; out end

  val HIGH_RATIO = 0.2 val LOW_RATIO = 0.1
  fun findMax arr = let val n = Array.length arr
      fun lp i m = if i >= n then m else lp (i+1) (Real.max (m, Array.sub (arr, i))) in lp 0 0.0 end

  fun thresholdSeq (magNMS: real array, h: int, w: int) : int array =
    let val n = h * w val maxVal = findMax magNMS val highThr = HIGH_RATIO * maxVal val lowThr = LOW_RATIO * maxVal
        val labels = Array.array (n, 0)
        fun classify i = if i >= n then () else let val m = Array.sub (magNMS, i)
            val lab = if m >= highThr then 2 else if m >= lowThr then 1 else 0
        in Array.update (labels, i, lab); classify (i+1) end
    in classify 0; labels end

  fun getLab (labels, h, w, y, x) = if y < 0 orelse y >= h orelse x < 0 orelse x >= w then 0 else Array.sub (labels, idx (y, x, w))
  fun hasStrong (labels, h, w, y, x) =
    getLab (labels, h, w, y-1, x-1) = 2 orelse getLab (labels, h, w, y-1, x) = 2 orelse
    getLab (labels, h, w, y-1, x+1) = 2 orelse getLab (labels, h, w, y, x-1) = 2 orelse
    getLab (labels, h, w, y, x+1) = 2 orelse getLab (labels, h, w, y+1, x-1) = 2 orelse
    getLab (labels, h, w, y+1, x) = 2 orelse getLab (labels, h, w, y+1, x+1) = 2

  fun hysteresisSeq (labels: int array, h: int, w: int) : int array =
    let val n = h * w val out = Array.array (n, 0)
        fun hyst i = if i >= n then () else let val y = i div w val x = i mod w val lab = Array.sub (labels, i)
            val v = if lab = 2 then 255 else if lab = 1 andalso hasStrong (labels, h, w, y, x) then 255 else 0
        in Array.update (out, i, v); hyst (i+1) end
    in hyst 0; out end

  fun cannySeq (pix: int array, h: int, w: int) : int array =
    let val blurred = gaussianBlurSeq (pix, h, w) val (gx, gy, mag) = sobelSeq (blurred, h, w)
        val magNMS = nmsSeq (gx, gy, mag, h, w) val labels = thresholdSeq (magNMS, h, w)
    in hysteresisSeq (labels, h, w) end

  (* Parallel *)
  fun gaussianBlurPar (pix: int array, h: int, w: int, numWorkers: int) : int array =
    let val n = h * w val out = Array.array (n, 0) val chunkSize = (n + numWorkers - 1) div numWorkers
        fun blur (y, x) = let val p00 = getPixel (pix, h, w, y-1, x-1) val p01 = getPixel (pix, h, w, y-1, x)
            val p02 = getPixel (pix, h, w, y-1, x+1) val p10 = getPixel (pix, h, w, y, x-1)
            val p11 = getPixel (pix, h, w, y, x) val p12 = getPixel (pix, h, w, y, x+1)
            val p20 = getPixel (pix, h, w, y+1, x-1) val p21 = getPixel (pix, h, w, y+1, x)
            val p22 = getPixel (pix, h, w, y+1, x+1)
            val sum = 1*p00 + 2*p01 + 1*p02 + 2*p10 + 4*p11 + 2*p12 + 1*p20 + 2*p21 + 1*p22
        in Int.max (0, Int.min (255, sum div 16)) end
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun process i = if i >= hi then () else (Array.update (out, i, blur (i div w, i mod w)); process (i+1))
          in process lo end)
    in out end

  fun sobelPar (pix: int array, h: int, w: int, numWorkers: int) : real array * real array * real array =
    let val n = h * w val gx = Array.array (n, 0.0) val gy = Array.array (n, 0.0) val mag = Array.array (n, 0.0)
        val chunkSize = (n + numWorkers - 1) div numWorkers
        fun compute (y, x) = let val p00 = toReal (getPixel (pix, h, w, y-1, x-1)) val p01 = toReal (getPixel (pix, h, w, y-1, x))
            val p02 = toReal (getPixel (pix, h, w, y-1, x+1)) val p10 = toReal (getPixel (pix, h, w, y, x-1))
            val p12 = toReal (getPixel (pix, h, w, y, x+1)) val p20 = toReal (getPixel (pix, h, w, y+1, x-1))
            val p21 = toReal (getPixel (pix, h, w, y+1, x)) val p22 = toReal (getPixel (pix, h, w, y+1, x+1))
            val gxV = ~1.0*p00 + 1.0*p02 + ~2.0*p10 + 2.0*p12 + ~1.0*p20 + 1.0*p22
            val gyV = 1.0*p00 + 2.0*p01 + 1.0*p02 + ~1.0*p20 + ~2.0*p21 + ~1.0*p22
        in (gxV, gyV, Math.sqrt (gxV*gxV + gyV*gyV)) end
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun process i = if i >= hi then () else let val (gxV, gyV, magV) = compute (i div w, i mod w)
                   in Array.update (gx, i, gxV); Array.update (gy, i, gyV); Array.update (mag, i, magV); process (i+1) end
          in process lo end)
    in (gx, gy, mag) end

  fun nmsPar (gx: real array, gy: real array, mag: real array, h: int, w: int, numWorkers: int) : real array =
    let val n = h * w val out = Array.array (n, 0.0) val chunkSize = (n + numWorkers - 1) div numWorkers
        fun nms (y, x) = let val i = idx (y, x, w) val m = Array.sub (mag, i)
            val d = quantizeDir (Array.sub (gx, i), Array.sub (gy, i))
            val (m1, m2) = case d of 0 => (getPixelReal (mag, h, w, y, x-1), getPixelReal (mag, h, w, y, x+1))
              | 1 => (getPixelReal (mag, h, w, y-1, x+1), getPixelReal (mag, h, w, y+1, x-1))
              | 2 => (getPixelReal (mag, h, w, y-1, x), getPixelReal (mag, h, w, y+1, x))
              | _ => (getPixelReal (mag, h, w, y-1, x-1), getPixelReal (mag, h, w, y+1, x+1))
        in if m >= m1 andalso m >= m2 then m else 0.0 end
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun process i = if i >= hi then () else (Array.update (out, i, nms (i div w, i mod w)); process (i+1))
          in process lo end)
    in out end

  fun findMaxPar (arr: real array, numWorkers: int) : real =
    let val n = Array.length arr val chunkSize = (n + numWorkers - 1) div numWorkers
        val localMax = Array.array (numWorkers, 0.0)
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun lp i m = if i >= hi then m else lp (i+1) (Real.max (m, Array.sub (arr, i)))
          in Array.update (localMax, wk, lp lo 0.0) end)
        fun merge i m = if i >= numWorkers then m else merge (i+1) (Real.max (m, Array.sub (localMax, i)))
    in merge 0 0.0 end

  fun thresholdPar (magNMS: real array, h: int, w: int, numWorkers: int) : int array =
    let val n = h * w val maxVal = findMaxPar (magNMS, numWorkers)
        val highThr = HIGH_RATIO * maxVal val lowThr = LOW_RATIO * maxVal
        val labels = Array.array (n, 0) val chunkSize = (n + numWorkers - 1) div numWorkers
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun classify i = if i >= hi then () else let val m = Array.sub (magNMS, i)
                  val lab = if m >= highThr then 2 else if m >= lowThr then 1 else 0
              in Array.update (labels, i, lab); classify (i+1) end
          in classify lo end)
    in labels end

  fun hysteresisPar (labels: int array, h: int, w: int, numWorkers: int) : int array =
    let val n = h * w val out = Array.array (n, 0) val chunkSize = (n + numWorkers - 1) div numWorkers
        val _ = Parallel.parfor (0, numWorkers) (fn wk =>
          let val lo = wk * chunkSize val hi = Int.min (n, lo + chunkSize)
              fun hyst i = if i >= hi then () else let val y = i div w val x = i mod w val lab = Array.sub (labels, i)
                  val v = if lab = 2 then 255 else if lab = 1 andalso hasStrong (labels, h, w, y, x) then 255 else 0
              in Array.update (out, i, v); hyst (i+1) end
          in hyst lo end)
    in out end

  fun cannyPar (pix: int array, h: int, w: int, numWorkers: int) : int array =
    let val blurred = gaussianBlurPar (pix, h, w, numWorkers) val (gx, gy, mag) = sobelPar (blurred, h, w, numWorkers)
        val magNMS = nmsPar (gx, gy, mag, h, w, numWorkers) val labels = thresholdPar (magNMS, h, w, numWorkers)
    in hysteresisPar (labels, h, w, numWorkers) end

  (* Comparison *)
  fun compareArrays (a: int array, b: int array) : int * int =
    let val n = Array.length a
        fun count i matches mismatches = if i >= n then (matches, mismatches)
          else if Array.sub (a, i) = Array.sub (b, i) then count (i+1) (matches+1) mismatches
          else count (i+1) matches (mismatches+1)
    in count 0 0 0 end

  fun printResult (name: string, n: int, matches: int, mismatches: int) =
    let val matchPct = 100.0 * toReal matches / toReal n
    in print ("    Pixels:     " ^ Int.toString n ^ "\n");
       print ("    Matches:    " ^ Int.toString matches ^ " (" ^ Real.fmt (StringCvt.FIX (SOME 2)) matchPct ^ "%)\n");
       print ("    Mismatches: " ^ Int.toString mismatches ^ "\n");
       if mismatches = 0 then print "    Status:     PASS (100% match)\n"
       else if matchPct > 99.9 then print "    Status:     PASS (>99.9% match)\n"
       else if matchPct > 99.0 then print "    Status:     WARN (>99% match)\n"
       else print "    Status:     FAIL\n" end

  fun testCanny (filename: string, numWorkers: int) =
    let val _ = print ("\n  [Canny] " ^ filename ^ "\n")
        val (pix, h, w) = readImage filename val n = h * w
        val _ = print ("    Size: " ^ Int.toString w ^ "x" ^ Int.toString h ^ "\n")
        val seqResult = cannySeq (pix, h, w) val parResult = cannyPar (pix, h, w, numWorkers)
        val (matches, mismatches) = compareArrays (seqResult, parResult)
    in printResult ("Canny", n, matches, mismatches); mismatches = 0 end

  (* Main *)
  fun main () =
    let val numWorkers = 16
        val _ = print "============================================================\n"
        val _ = print "  CORRECTNESS TESTS: Canny Edge Detection\n"
        val _ = print "============================================================\n"
        val _ = print ("  Workers: " ^ Int.toString numWorkers ^ "\n")
        val testFiles = ["test_images/test_gradient_256.txt", "test_images/test_shapes_256.txt",
                         "test_images/test_checker_256.txt", "test_images/test_1024x1024.txt", "test_images/test_2048x2048.txt"]
        val results = List.map (fn f => testCanny (f, numWorkers) handle _ => (print ("  [Skipped: " ^ f ^ "]\n"); false)) testFiles
        val passed = List.length (List.filter (fn x => x) results) val total = List.length results
        val _ = print "\n============================================================\n"
        val _ = print "  SUMMARY\n"
        val _ = print "============================================================\n"
        val _ = print ("  Tests passed: " ^ Int.toString passed ^ "/" ^ Int.toString total ^ "\n")
        val _ = if passed = total then print "  Overall: ALL TESTS PASSED\n" else print "  Overall: SOME TESTS FAILED\n"
        val _ = print "============================================================\n"
    in () end

  val _ = main ()

end
