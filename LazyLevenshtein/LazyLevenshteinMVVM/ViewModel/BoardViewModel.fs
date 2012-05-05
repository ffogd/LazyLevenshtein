namespace LazyLevenshteinMVVM.ViewModel

open System
open System.Windows
open System.Windows.Threading
open System.Windows.Input
open System.ComponentModel
open System.Collections.ObjectModel
open LazyLevenshteinMVVM.Model

type BoardCell() =
    inherit ViewModelBase()
    let mutable cellValue = ""
    let mutable isEvaluated = false
    let mutable isEvaluatedFrom =false
    let mutable isEvaluate = false
    let mutable isThunk = false
    let mutable isDiagStep = false
    let mutable evaluateText = String.Empty
    let mutable evaluatedFrom = String.Empty

    member x.FromPoint (point : LazyLevenshteinMVVM.Model.Point) =
        match x.IsEvaluated with
        | true -> 
            match point.isDiagonalStep, point.isEvaluatedFrom with 
            | true, true ->
                x.IsDiagStep <- true
                x.IsEvaluatedFrom <- true
            | true, false ->  x.IsDiagStep <- true
            | false, true ->  x.IsEvaluatedFrom <- true
            | _ -> ()
        | false ->
            x.IsEvaluate <- point.isEvaluate
            x.IsDiagStep <- point.isDiagonalStep
            x.IsThunk <- point.isThunk
            match point.value with
            | Some v -> 
                match point.isEvaluate, point.isEvaluated, point.isEvaluatedFrom, point.isThunk with
                | true, _, _, _ ->   x.EvaluateText <- v
                | _, true, _, _ -> 
                    x.IsEvaluated <- point.isEvaluated
                    x.EvaluatedFrom <- point.evaluatedFrom
                    x.CellValue <- v
                |_, _, true, _ ->    
                    x.IsEvaluatedFrom <- point.isEvaluatedFrom
                | _, _, _, true -> 
                    x.IsThunk <- point.isThunk
                    x.CellValue <- v
                |_->()
            | None -> ()
    member x.IsEvaluated 
        with get() = isEvaluated
        and set value = 
            isEvaluated <- value
            x.OnPropertyChanged(<@x.IsEvaluated@>)
    member x.IsEvaluate 
        with get() = isEvaluate
        and set value = 
            isEvaluate <- value
            x.OnPropertyChanged(<@x.IsEvaluate@>)
    member x.IsThunk 
        with get() = isThunk
        and set value = 
            isThunk <- value
            x.OnPropertyChanged(<@x.IsThunk@>)
    member x.IsDiagStep 
        with get() = isDiagStep
        and set value = 
            isDiagStep <- value
            x.OnPropertyChanged(<@x.IsDiagStep@>)
    member x.CellValue
        with get() = cellValue
        and set value = 
            cellValue <- value
            x.OnPropertyChanged(<@x.CellValue@>)
    member x.EvaluateText
        with get() = evaluateText
        and set value = 
            evaluateText <- value
            x.OnPropertyChanged(<@x.EvaluateText@>)
    member x.EvaluatedFrom
        with get() = evaluatedFrom
        and set value = 
            evaluatedFrom <- value
            x.OnPropertyChanged(<@x.EvaluatedFrom@>)
    member x.IsEvaluatedFrom
        with get() = isEvaluatedFrom
        and set value = 
            isEvaluatedFrom <- value
            x.OnPropertyChanged(<@x.IsEvaluatedFrom@>)

type InnerGrid() as x =
    inherit ViewModelBase()
        
    let mutable rows = new ObservableCollection<ObservableCollection<BoardCell>>()
    let mutable sizex = 0
    let mutable sizey = 0
    let mutable textValX = String.Empty
    let mutable textValY = String.Empty

    let model = LevenshteinModel.Empty
    let mutable timer = new DispatcherTimer(DispatcherPriority.Normal)
    let mutable distance = 0
    let pointToCell (point : LazyLevenshteinMVVM.Model.Point)  =
                let col = rows.[snd point.xy + 2] 
                let cell = col.[fst point.xy + 2]
                cell.FromPoint point
                cell.IsEvaluatedFrom <- false
                cell.IsDiagStep <- false
                col.[fst point.xy + 2] <- cell
                rows.[snd point.xy + 2] <- col       
    do
        timer.Interval <- new TimeSpan(0, 0, 0, 0, 1500)
        timer.Tick.Add(fun _  -> x.AnimateOneStep () )    
    
    member x.AnimateOneStep () = 
        match rows.Count, model.OneStep () with 
        | _, [] -> timer.Stop()
        | 0, _ -> timer.Stop()
        | _, points ->
            points |> List.iter pointToCell 
            x.OnPropertyChanged(<@x.GridRows@>)
    member private x.TextValMatrix  =

        [for i in 0..sizex-2 do
                rows.[0].[i+2].CellValue <- textValX.[i].ToString()
                rows.[1].[i+2].CellValue <- (i+1).ToString() 
                rows.[1].[i+2].IsEvaluated<-true
        ] |> ignore

        rows.[1].[1].CellValue <- "0" 
        [for j in 2..sizey do  
              let c = rows.[j].[0]
              let n = rows.[j].[1]
              c.CellValue <- textValY.[j-2].ToString()
              n.CellValue <- (j - 1).ToString()
              n.IsEvaluated<-true
        ] |> ignore
        timer.Stop()
        model.Run(textValX, textValY)
        match model.distance with
        | None -> x.Distance<-0
        | Some v -> x.Distance<-v
        x.OnPropertyChanged(<@x.GridRows@>)
        timer.Start()

    member x.TextValX
        with get() = textValX
        and set value = 
            textValX<-value
            x.SizeX <- (if textValX.Length > 0 then textValX.Length + 1 else 0)
            x.TextValMatrix
            x.OnPropertyChanged(<@x.TextValX@>)
            
    member x.TextValY
        with get() = textValY
        and set value = 
            textValY<-value
            x.SizeY <- (if textValY.Length > 0 then textValY.Length + 1 else 0)
            x.TextValMatrix
            x.OnPropertyChanged(<@x.TextValY@>)
    member x.Distance 
        with get() = distance
        and set value = 
            distance <- value
            x.OnPropertyChanged(<@x.Distance@>)
    member x.GridRows = rows
    member private x.Size =
        sizex <- (if sizex > 0 then sizex else (if sizey > 0 then 1 else 0))
        sizey <- (if sizey > 0 then sizey else (if sizex > 0 then 1 else 0))
        [   for i in 0..sizey do
                let col = new ObservableCollection<BoardCell>()
                for j in 0..sizex do
                    let c = new BoardCell()
                    c.CellValue <-""
                    col.Add(c)

                rows.Add(col) ] |> ignore  
    member x.SizeY
        with get() = sizey
        and set value = 
            sizey <- value
            rows <- new ObservableCollection<ObservableCollection<BoardCell>>()
            x.Size
            x.OnPropertyChanged(<@x.SizeY@>)
    member x.SizeX
        with get() = sizex
        and set value = 
            sizex <- value
            rows <- new ObservableCollection<ObservableCollection<BoardCell>>()
            x.Size
            x.OnPropertyChanged(<@x.SizeX@>)
