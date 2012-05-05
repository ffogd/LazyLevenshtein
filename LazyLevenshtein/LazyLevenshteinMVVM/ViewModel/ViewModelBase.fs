namespace LazyLevenshteinMVVM.ViewModel

open System

open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
 
type ViewModelBase() =
    let propertyChangedEvent = new Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    member x.OnPropertyChanged (expr: Expr) = 
        match expr with
        | PropertyGet(_, methodInfo, _) ->
            let propertyName = methodInfo.Name
            propertyChangedEvent.Trigger(x, new PropertyChangedEventArgs(propertyName))
        | other -> failwith "not implemented" 
        
