open System
open System.Globalization

let readLines = Seq.initInfinite (fun _ -> Console.ReadLine())
let read parser = 
    readLines
    |> Seq.choose (parser >> function true, v -> Some v | _ -> None)
let readFloats = read (fun s -> 
    Double.TryParse(s.Replace(',', '.'),
                    NumberStyles.Any, 
                    CultureInfo.InvariantCulture))

let rec polynomial (ks:float list) (x:float) =
    match ks with
    | k::ks -> k + x * polynomial ks x
    | [] -> 0.0

let derivative (ks:float list) =
    if ks.IsEmpty then [0.0] else
        List.mapi (fun i k -> k * float (i + 1)) ks.Tail

let rec methodChordHelper (f:float -> float)
        (n:int) (a:float) (b:float) (e:float) =
    let fa = f a
    let fb = f b
    let x = (a * fb - b * fa) / (fb - fa)
    let fx = f x
    let d = Math.Abs (a - b)
    printfn "%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f" n a b x fa fb fx d
    if d < e || Math.Abs fx < e then x else
        methodChordHelper f (n + 1) x (if fa * fx < 0.0 then a else b) e

let rec methodNewtonHelper (f:float -> float)
        (f':float -> float) (n:int) (x:float) (e:float) =
    let fx = f x
    let f'x = f' x
    let x' = x - fx / f'x
    let d = Math.Abs (x - x')
    printfn "%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f" n x fx f'x x' d
    if d < e then x' else methodNewtonHelper f f' (n + 1) x' e

let rec methodIterationHelper (f:float -> float)
        (phi:float -> float) (n:int) (x:float) (e:float) =
    let x' = phi x
    let d = Math.Abs (x - x')
    printfn "%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f" n x (f x) x' x' d
    if d < e then x' else methodIterationHelper f phi (n + 1) x' e

let methodChord (f:float -> float)
        (a:float) (b:float) (e:float) =
    printfn "Метод хорд"
    printfn "№\ta\tb\tx\tf(a)\tf(b)\tf(x)\t|a-b|"
    methodChordHelper f 1 a b e

let methodNewton (f:float -> float)
        (f':float -> float) (x:float) (e:float) =
    printfn "Метод Ньютона"
    printfn "№\tx\tf(x)\tf'(x)\tx^\t|x-x^|"
    methodNewtonHelper f f' 1 x e

let methodIteration (f:float -> float)
        (phi:float -> float) (x:float) (e:float) = 
    printfn "Метод простой итерации"
    printfn "№\tx\tf(x)\tx^\tphi(x)\t|x-x^|"
    methodIterationHelper f phi 1 x e

let drawGraphSvg (fs:(float -> float) list) (xMin:float) (xMax:float) 
        (yMin:float) (yMax:float) (scaleX:float) (scaleY:float) (samples:int) =
    let stream = IO.File.CreateText("pic.svg")
    sprintf """<svg xmlns="http://www.w3.org/2000/svg" 
            version="1.1" width="%f" height="%f"
            stroke="black" text-anchor="end">"""
        ((xMax - xMin) * scaleX)
        ((yMax - yMin) * scaleY)
        |> stream.WriteLine
    sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f"/>"""
        0.0 (yMax * scaleY) ((xMax - xMin) * scaleX - 10.0) (yMax * scaleY)
        |> stream.WriteLine
    sprintf """<line x1="%f" y1="%f" x2="%f" y2="%f"/>"""
        (-xMin * scaleX) 10.0 (-xMin * scaleX) ((yMax - yMin) * scaleY)
        |> stream.WriteLine
    sprintf """<text x="%f" y="%f">0</text>"""
        (-xMin * scaleX - 2.0) (yMax * scaleY + 14.0)
        |> stream.WriteLine
    sprintf """<path d="M %f %f L %f %f L %f %f Z" />"""
        ((xMax - xMin) * scaleX) (yMax * scaleY)
        ((xMax - xMin) * scaleX - 10.0) (yMax * scaleY + 5.0)
        ((xMax - xMin) * scaleX - 10.0) (yMax * scaleY - 5.0)
        |> stream.WriteLine
    sprintf """<text x="%f" y="%f">x</text>"""
        ((xMax - xMin) * scaleX) (yMax * scaleY + 20.0)
        |> stream.WriteLine
    sprintf """<path d="M %f %f L %f %f L %f %f Z" />"""
        (-xMin * scaleX) 0.0
        (-xMin * scaleX - 5.0) 10.0
        (-xMin * scaleX + 5.0) 10.0
        |> stream.WriteLine
    sprintf """<text x="%f" y="%f">y</text>"""
        (-xMin * scaleX - 10.0) 15.0
        |> stream.WriteLine
    sprintf """<circle cx="%f" cy="%f" r="2"/>""" 
        (-xMin * scaleX) (yMax * scaleY)
        |> stream.WriteLine
    for i in 1..int xMax do
        let cx = (float i - xMin) * scaleX
        let cy = yMax * scaleY
        sprintf """<circle cx="%f" cy="%f" r="2"/>""" cx cy
            |> stream.WriteLine
        sprintf """<text x="%f" y="%f" text-anchor="end">%d</text>"""
            (cx - 2.0) (cy + 14.0) i
            |> stream.WriteLine
    for i in 1..int -xMin do
        let cx = (-float i - xMin) * scaleX
        let cy = yMax * scaleY
        sprintf """<circle cx="%f" cy="%f" r="2"/>""" cx cy
            |> stream.WriteLine
        sprintf """<text x="%f" y="%f" text-anchor="end">-%d</text>"""
            (cx - 2.0) (cy + 14.0) i
            |> stream.WriteLine
    for i in 1..int yMax do
        let cx = -xMin * scaleX
        let cy = (yMax - float i) * scaleY
        sprintf """<circle cx="%f" cy="%f" r="2"/>""" cx cy
            |> stream.WriteLine
        sprintf """<text x="%f" y="%f" text-anchor="end">%d</text>"""
            (cx - 2.0) (cy + 14.0) i
            |> stream.WriteLine
    for i in 1..int -yMin do
        let cx = -xMin * scaleX
        let cy = (yMax + float i) * scaleY
        sprintf """<circle cx="%f" cy="%f" r="2"/>""" cx cy
            |> stream.WriteLine
        sprintf """<text x="%f" y="%f" text-anchor="end">-%d</text>"""
            (cx - 2.0) (cy + 14.0) i
            |> stream.WriteLine

    for f in fs do
        sprintf """<path fill="transparent" d="M %f %f"""
            0.0 ((yMax - f xMin) * scaleY)
            |> stream.WriteLine
        for i in 1..samples do
            let x = xMin + (xMax - xMin) * float i / float samples
            sprintf "L %f %f" 
                ((x - xMin) * scaleX) ((yMax - f x) * scaleY)
                |> stream.WriteLine
        stream.WriteLine "\"/>"
    stream.WriteLine "</svg>"
    stream.Close()

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8
    // f = -2.4 x^3 + 1.27 x^2 + 8.63 x + 2.31
    // f' = -7.2 x^2 + 2.54 x + 8.63
    // f'' = -14.4 x + 2.54
    // f''' = -14.4
    // phi = ((1.27 x^2 + 8.63 x + 2.31) / 2.4)^(1/3)
    // phi' = ((1.27 x^2 + 8.63 x + 2.31) / 2.4)^(-2/3)
    //        * (2.54 x + 8.63) / 7.2
    // Условие достаточной сходимости метода простой итерации к 
    // центральному корню не выполняется, т.к. в окрестности корня 
    // x ~= -0.286252 производная phi' по модулю превосходит 1:
    // phi'(-0.286252) = 13.3959

    let ks = [2.31; 8.63; 1.27; -2.4]
    let f x = polynomial ks x
    let f' x = polynomial (derivative ks) x
    let phi x =
        let p = ks.Length - 1
        let bas = polynomial (List.take p ks) x / List.last ks
        -float (Math.Sign bas) * Math.Pow (Math.Abs bas, 1.0 / float p)
    // f' = 0 при x = -0.9325403926 и x = 1.28531817
    // f'' = 0 при x = 0.1763888889
    let chordValidMin = 1.28531817
    let newtonValidMax = -0.9325403926

    drawGraphSvg [f] -2.5 2.5 -3.5 11.5 100.0 50.0 1000

    printf "Введите первую границу для метода хорд"
    printf " (больше %f): " chordValidMin
    let a = Seq.filter (fun x -> x >= chordValidMin) readFloats |> Seq.head
    printfn "%f" a
    printf "Введите вторую границу для метода хорд "
    printf "(больше %f, f(a) * f(b) < 0): " chordValidMin
    let b = Seq.filter (fun x -> x >= chordValidMin && f(x) * f(a) < 0.0) 
             readFloats |> Seq.head
    printfn "%f" b
    printf "Введите начальное приближение для метода Ньютона "
    printf "(меньше %f): " newtonValidMax
    let xNewton = Seq.filter (fun x -> x <= newtonValidMax) 
                   readFloats |> Seq.head
    printfn "%f" xNewton
    printf "Введите начальное приближение для метода простой итерации: "
    let xIter = readFloats |> Seq.head
    printfn "%f" xIter
    printf "Введите погрешность (больше 0.01): "
    let epsilon = Seq.filter (fun x -> x >= 0.01) readFloats |> Seq.head
    printfn "%f" epsilon

    let rightRoot = methodChord f a b epsilon
    printf "Правый корень: "
    printfn "%.3f; f(%.3f) = %.3f" rightRoot rightRoot (f rightRoot)
    let leftRoot = methodNewton f f' xNewton epsilon
    printf "Левый корень: "
    printfn "%.3f; f(%.3f) = %.3f" leftRoot leftRoot (f leftRoot)
    let middleRoot = methodIteration f phi xIter epsilon
    printf "Корень метода простой итерации: "
    printfn "%.3f; f(%.3f) = %.3f" middleRoot middleRoot (f middleRoot)
    0
