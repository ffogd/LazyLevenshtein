// Weitere Informationen zu F# unter "http://fsharp.net".
namespace LazyLevenshtein
#nowarn "40"

module Types =
    type Direction = N | NW  | W 
    type Action = Thunk of Direction | Evaluate | Evaluated of int | EvaluatedFrom of Direction | Diag 
    type Coord =
        X of int | Y of int
        static member (+) (a:Coord, y:int) =
            match a with
            | X x-> X (x+y)
            | Y x-> Y (x+y)

    type Log = {point : int * int; actions: Action list; txt: string}
    let inline toPoint x y =
        match x,y with
        | X x, Y y-> x, y
        | Y y, X x-> x, y
    
module Algorithm =
    open System
    open System.ComponentModel
    open Types
    
    //from http://www.haskell.org/haskellwiki/Edit_distance.
    // "An entry depends on three neighbours which lie on the diagonal below, the current diagonal and the diagonal above.
    //  Each diagonal therefore depends on the diagonal below and the diagonal above where a row depends only on the row above"    
    // levenshtein.fs 
    let inline editDist sa sb =
        //let min3 x y z = if x < y then x else min y (LazyList.head z)
        let lab = List.length sa - List.length sb
        let rec mainDiag:_ Lazy = 
            lazy(oneDiag sa sb (LazyList.delayed (fun () -> LazyList.head uppers.Value)) 
                               (LazyList.consDelayed -1 (fun ()-> LazyList.head lowers.Value)))
        //upper diagonals
        and uppers : _ Lazy = lazy(eachDiag sa sb (LazyList.consDelayed (mainDiag.Value) (fun ()-> uppers.Value)))
        //lower diagonals. note swap sb sa !
        and lowers : _ Lazy = lazy(eachDiag sb sa (LazyList.consDelayed (mainDiag.Value) (fun ()-> lowers.Value)))
        // 'a list -> 'a list -> LazyList<LazyList<int>>
        and eachDiag a b diag = 
            match a, b, diag with
            | _, [], _ -> LazyList.empty
            | a, (bch :: bs), ( LazyList.Cons (lastDiag, diags)) -> 
                let nextDiag = LazyList.delayed (fun () -> LazyList.head (LazyList.tail diags))
                LazyList.consDelayed (oneDiag a bs nextDiag lastDiag) (fun ()-> (eachDiag a bs diags))
        // 'a list -> 'a list -> LazyList<int> -> LazyList<int> -> LazyList<int>
        and oneDiag a b diagAbove diagBelow  = 
            // nw - north-west, n - north, w - west.
            // 'a list -> 'a list -> int -> LazyList<int> -> LazyList<int> -> LazyList<int>
            let rec doDiag a b nw n w = 
                match a, b with
                | [], _ -> LazyList.empty
                | _, [] -> LazyList.empty 
                | (ach :: achs), (bch :: bchs) -> 
                    if (ach  = bch) then
                //case if ach = bch then nw
                        LazyList.consDelayed nw (fun ()-> 
                                                    doDiag achs bchs nw 
                                                        (LazyList.delayed (fun ()-> LazyList.tail n)) 
                                                        (LazyList.delayed (fun ()-> LazyList.tail w)))
                // case else 1 + min3 (LazyList.head w) nw n
                    else if (LazyList.head w) < nw then
                    // case let min3 x y z = if x < y then x ...
                        let me = 1 + (LazyList.head w)
                        LazyList.consDelayed me (fun ()-> 
                                                    doDiag achs bchs me 
                                                        (LazyList.delayed (fun ()-> LazyList.tail n)) 
                                                        (LazyList.tail w))
                    // case let min3 x y z = ... else min y (LazyList.head z)
                    else
                        let me = 1 + min nw (LazyList.head n)
                        LazyList.consDelayed me (fun ()-> 
                                                    doDiag achs bchs me 
                                                        (LazyList.tail n)
                                                        (LazyList.tail w))               
            let firstelt = 1 + (LazyList.head diagBelow)
            LazyList.consDelayed firstelt (fun ()-> doDiag a b firstelt diagAbove (LazyList.tail diagBelow))
 
        if lab = 0      then mainDiag.Value
        else if lab > 0 then (LazyList.toArray lowers.Value).[lab - 1]
        else                 (LazyList.toArray (uppers.Value)).[-1 - lab]
 
    let inline lazyDist (s1:string) (s2:string) =
        match s1.Length,s2.Length with
        |0, l2 -> l2
        |l1, 0 -> l1
        | _ -> 
            let res = editDist (List.ofSeq s1) (List.ofSeq s2) |> LazyList.toArray
            res.[res.Length - 1]
    //Algorithm for Visualisation
    let  editDistance sa sb = 
        let xmax, ymax= Array.length sa, Array.length sb
        let loga,logb= Array.init xmax id, Array.init ymax id
        let lab = xmax - ymax
        let fa i = sa.[i]
        let fb i = sb.[i]
        let fcoorda i = X loga.[i], W
        let fcoordb i = Y logb.[i], N
        let log = ref []
        
        let rec mainDiag:_ Lazy = 
            lazy(oneDiag 0 0 (fa , xmax) (fb , ymax) (LazyList.delayed (fun ()-> LazyList.head uppers.Value)) (LazyList.consDelayed (-1, NW) (fun ()-> LazyList.head lowers.Value)) (fcoorda, fcoordb))

        and uppers : _ Lazy = 
            lazy(eachDiag 0 0 (fa , xmax) (fb , ymax) (LazyList.consDelayed (mainDiag.Value) (fun ()-> uppers.Value)) (fcoorda, fcoordb))

        and lowers : _ Lazy = 
            lazy(eachDiag 0 0 (fb  ,ymax) (fa , xmax)  (LazyList.consDelayed (mainDiag.Value) (fun ()-> lowers.Value)) (fcoordb, fcoorda))
        and eachDiag i j psa psb diag fn = 
                    match psa, psb, diag with
                    | _, (_, bmax), _ when j = bmax-> LazyList.empty
                    | _, _, ( LazyList.Cons (lastDiag, diags)) -> 
                        let nextDiag = LazyList.delayed (fun ()-> LazyList.head (LazyList.tail diags))
                        LazyList.consDelayed (oneDiag i (j+1) psa psb nextDiag lastDiag fn) (fun ()-> (eachDiag i (j+1) psa psb diags fn))
        and oneDiag i j psa psb (diagAbove : _ LazyList) (diagBelow : _ LazyList) (fcoordx, fcoordy) = 
                   let ((fsa,amax), (fsb, bmax)) = psa, psb
                   let rec doDiag i j (nw, d) n w = 
                        match i , j with
                        | i, _  when i = amax  -> LazyList.empty
                        | _, j  when j = bmax  -> LazyList.empty 
                        | _ -> 
                            let (x, dx), (y, dy) = (fcoordx i), (fcoordy j)
                            log := {point =toPoint x y; actions = [Evaluate]; txt = sprintf "%A = %A ?" (fsa i) (fsb j)}:: !log
                            match (fsa i  = fsb j) with
                            | true ->
                        //case if ach = bch then nw
                                log := {point  = toPoint x y; actions = [EvaluatedFrom NW; Evaluated nw; Thunk dx; Thunk dy]; txt = sprintf "=NW (%A)" nw} :: !log
                                LazyList.consDelayed (nw, d) (fun ()-> 
                                                            doDiag (i + 1) (j + 1) (nw, d) 
                                                                (LazyList.delayed (fun ()-> log := {point =toPoint (x + 1) y; actions = [Diag]; txt = ""}:: !log;LazyList.tail n)) 
                                                                (LazyList.delayed (fun ()-> log := {point =toPoint x (y+1); actions = [Diag]; txt = ""}:: !log;LazyList.tail w)))
                        // case else 1 + min3 (LazyList.head w) nw n
                            | false ->
                            // case let min3 x y z = if x < y then x ...
                                let me, dir = (LazyList.head w)
                                match me < nw with
                                | true ->
                                    log := {point =toPoint x y; actions = [EvaluatedFrom dy; Evaluated (1+me); Thunk dy]; txt =sprintf "%A(%A)<%A(%A)=1+%A(%A)" dy me NW nw dy me } :: !log
                                    LazyList.consDelayed (1+me,dy) (fun ()-> 
                                                                doDiag (i + 1) (j + 1)  (1+me, dir) 
                                                                    (LazyList.delayed (fun ()-> log := {point =toPoint (x + 1) y; actions = [Diag]; txt = ""}:: !log;LazyList.tail n)) 
                                                                    (LazyList.tail w))
                            // case let min3 x y z = ... else min y (LazyList.head z)
                            //else
                                | false ->
                                    let hd, dir =LazyList.head n
                                    let me = 1 + min nw hd
                                     
                                    if nw < hd then
                                        log := {point = toPoint x y; actions = [EvaluatedFrom NW; Evaluated me]; txt =sprintf "=1+min NW(%A) %A(%A) " nw dx hd } :: !log
                                    else
                                        log := {point = toPoint x y; actions = [EvaluatedFrom dx; Evaluated me]; txt =sprintf "=1+min NW(%A) %A(%A) " nw dx hd  } :: !log
                                    LazyList.consDelayed (me,dx) (fun ()-> 
                                                                doDiag (i + 1) (j + 1)  (me, dir) 
                                                                    (LazyList.tail n)
                                                                    (LazyList.tail w))       
                   
                   let firstelt,d =  (LazyList.head diagBelow)
                   LazyList.consDelayed (1 + firstelt, d) (fun ()-> doDiag i j (1 + firstelt,d) diagAbove (LazyList.tail diagBelow))

        (if lab = 0 then mainDiag.Value
         else if lab > 0 then (LazyList.toArray lowers.Value).[lab - 1]
         else (LazyList.toArray uppers.Value).[-1 - lab]),log

    let lazyDistance (s1:string) (s2:string) = 
            let sa,sb = Array.ofSeq s1, Array.ofSeq s2
            match sa.Length,sb.Length with
            |0, l2 -> l2, []
            |l1, 0 -> l1, []
            | _ -> 
                let res = editDistance sa sb 
                let t = fst res|> LazyList.toList
                let e = (snd res)
                t.[t.Length - 1]|>fst, List.rev !e


