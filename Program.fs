open System

let readLines = Seq.initInfinite (fun _ -> Console.ReadLine())
let read parser = 
    readLines
    |> Seq.choose (parser >> function true, v -> Some v | _ -> None)
let readFloats = read Double.TryParse

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
    // let x = (a * fb - b * fa) / (fb - fa)
    let x = 0.5 * (a + b)
    let fx = f x
    let d = Math.Abs (a - b)
    printfn "%d\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f" n a b x fa fb d
    if d < e then x else
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
    printfn "№\ta\tb\tx\tf a\tf b\t|a-b|"
    methodChordHelper f 1 a b e

let methodNewton (f:float -> float)
        (f':float -> float) (x:float) (e:float) =
    printfn "Метод Ньютона"
    printfn "№\tx\tf x\tf' x\tx^\t|x-x^|"
    methodNewtonHelper f f' 1 x e

let methodIteration (f:float -> float)
        (phi:float -> float) (x:float) (e:float) = 
    printfn "Метод простой итерации"
    printfn "№\tx\tf x\tx^\tphi x\t|x-x^|"
    methodIterationHelper f phi 1 x e

let drawGraph (f:float -> float)
        (xMin:float) (xMax:float)
        (yMin:float) (yMax:float)
        (xScale:float) (yScale:float)
        (xStep:float) =
    let out = new IO.FileStream("graph.svg", IO.FileMode.Create)
    let encoding = System.Text.UTF8Encoding()
    let addText (str:string) =
        let bytes = encoding.GetBytes(str)
        out.Write(bytes, 0, bytes.Length)
    addText <| sprintf """<svg xmlns="http://www.w3.org/2000/svg"
    stroke="black" viewBox="%f %f %f %f" text-anchor="end" r="3">"""
        (xMin * xScale) (yMin * yScale) (xMax * xScale) (yMax * yScale)
    addText <| sprintf """<line x1="%f" y1="0" x2="%f" y2="0"/>""" (xMin * xScale) (xMax * xScale)
    addText <| sprintf """<line x1="0" y1="%f" x2="0" y2="%f"/>""" (yMin * yScale) (yMax * yScale)
    addText """<text x="-2" y="14">0</text>"""
    for x in 1 .. int xMax do
        addText <| sprintf """<text x="%f" y="14">%d</text>""" (float x * xScale - 2.0) x
        addText <| sprintf """<circle cx="%f" cy="0"/>""" (float x * xScale)
    addText "</svg>"
    out.Close()

[<EntryPoint>]
let main argv =
    Console.OutputEncoding <- Text.Encoding.UTF8
    // f = -2.4 x^3 + 1.27 x^2 + 8.63 x + 2.31
    // f' = -7.2 x^2 + 2.54 x + 8.63
    // f'' = -14.4 x + 2.54
    // f''' = -14.4

    let ks = [2.31; 8.63; 1.27; -2.4]
    let f x = polynomial ks x
    let f' x = polynomial (derivative ks) x
    let phi x = -Math.Pow ((polynomial (List.take (ks.Length - 1) ks) x), (1.0 / float (ks.Length - 1))) / List.last ks
    drawGraph f -2.0 3.0 -5.0 15.0 100.0 25.0 0.1
    // f' = 0 при x = -0.9325403926 и x = 1.28531817
    // f'' = 0 при x = 0.1763888889
    let chordValidMin = 1.28531817
    let newtonValidMax = -0.9325403926

    printfn "Введите первую границу для метода хорд (больше %f)" chordValidMin
    let a = Seq.filter (fun x -> x >= chordValidMin) readFloats |> Seq.head
    printfn "Введите вторую границу для метода хорд (больше %f, f(a) * f(b) < 0)" chordValidMin
    let b = Seq.filter (fun x -> x >= chordValidMin && f(x) * f(a) < 0.0) readFloats |> Seq.head
    printfn "Введите начальное приближение для метода Ньютона (меньше %f)" newtonValidMax
    let xNewton = Seq.filter (fun x -> x <= newtonValidMax) readFloats |> Seq.head
    printfn "Введите начальное приближение для метода простой итерации (от  до )"
    let xIter = readFloats |> Seq.head
    printfn "Введите погрешность (больше 0.01)"
    let epsilon = Seq.filter (fun x -> x >= 0.01) readFloats |> Seq.head

    printfn "%f %f %f %f %f" a b xNewton xIter epsilon
    printfn "Правый корень: %.3f" (methodChord f a b epsilon)
    printfn "Левый корень: %.3f" (methodNewton f f' xNewton epsilon)
    printfn "Центральный корень: %.3f" (methodIteration f phi xIter epsilon)
    0
