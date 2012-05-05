namespace LazyLevenshteinMVVM.Model
open LazyLevenshtein.Types
open LazyLevenshtein.Algorithm
open System

type Point = {value : string option; xy : int * int; isEvaluate : bool; isEvaluated : bool; isThunk : bool; 
                isEvaluatedFrom : bool;isDiagonalStep : bool; evaluatedFrom : string}
type LevenshteinModel =
    {mutable points :  Log list; mutable distance : int option;}
    static member Empty = {points =[]; distance = None;}
    member this.IsEmpty = List.isEmpty this.points
    member private this.Initpoint xy = {value = None; xy = xy; isEvaluate = false; isEvaluated = false; isThunk = false; 
                                        isDiagonalStep = false; isEvaluatedFrom = false;
                                        evaluatedFrom = String.Empty}
    member this.Run (s1, s2) = 
        let (d, ps) = lazyDistance s1 s2
        this.points <- ps
        this.distance <- Some d
    member this.OneStep () = 
        match this.points with
        | [] -> []
        | point :: ps ->
            this.points <- ps
            let direction2point (x,y) direction =
                match direction with
                | N -> x, y - 1
                | W -> x - 1, y
                | NW -> x - 1, y - 1
            let action2point xy txt action  =
                match action with
                | Thunk direction -> {this.Initpoint (direction2point xy direction) with isThunk = true; value = Some "<?>" }
                | Evaluate -> 
                    {this.Initpoint xy with isEvaluate = true; value = Some (txt);}
                | Evaluated value -> {this.Initpoint xy with value = Some (value.ToString()); isEvaluated = true; evaluatedFrom = txt }
                | EvaluatedFrom direction -> {this.Initpoint (direction2point xy direction) with isEvaluatedFrom = true;value = Some(sprintf "%A" direction) }
                | Diag ->  {this.Initpoint xy with isDiagonalStep=true}
            match point.actions with
            | [] -> []
            | [action] -> [action2point point.point point.txt action]
            | action :: xs  -> action2point point.point point.txt action  :: List.map (action2point point.point point.txt) xs

