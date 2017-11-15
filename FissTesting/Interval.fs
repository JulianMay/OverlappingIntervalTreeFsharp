module Interval

    type Node<'a , 'v when 'a : comparison> = {From : 'a; To : 'a; Values : 'v list; ChildNodes : Node<'a,'v> list}        
    let IsWithinPeriodOf (x:Node<'a, _>) (n:Node<'a, _>) = x.From >= n.From && x.To <= n.To
    let IsSamePeriodAs (x:Node<'a, _>) (n:Node<'a, _>) = x.From = n.From && x.To = n.To
    let IsIntersecting (x:Node<'a, _>) (n:Node<'a, _>) = (x.From < n.From && n.From < x.To) || (x.From < n.To && n.To < x.To)        

    let rec CountChildNodes (n: Node<'a,'v>) : int =
        n.ChildNodes 
            |> List.map (fun c -> if c.ChildNodes.Length = 0 then 1 else 1 + CountChildNodes c)
            |> List.sum

    let least a b = if a < b then a else b
    let greatest a b = if a > b then a else b   

    let ValidateNodeInterval (n:Node<'a,'v>) = compare n.From n.To = -1

    let Compound (a:Node<'a,'v>) (b:Node<'a,'v>) =     
        if (IsSamePeriodAs a b) then {a with Values = List.append a.Values b.Values}
        else if (IsWithinPeriodOf a b) then { b with ChildNodes = a::b.ChildNodes}
        else if (IsWithinPeriodOf b a) then { a with ChildNodes = b::a.ChildNodes}
        else {From=least a.From b.From; To = greatest a.To b.To; Values = []; ChildNodes = [a;b]}

    let FindChildNodeWithInterval (p:Node<'a,'v>) (s:'a, e:'a) =
        Some p

    let rec NodeWithValueRemoved (n:Node<'a,'v>) (s:'a, e:'a) (v:'v) =
        match (n.From, n.To) with
            |(f,t) when ((f,t)=(s,e))  -> 
                let nv = List.filter (fun x -> not(x=v)) n.Values
                {From =s; To=e; Values=nv; ChildNodes=n.ChildNodes}
            |_-> 
                let tn = FindChildNodeWithInterval n (s,e)
                match tn with
                    | Some candidate -> NodeWithValueRemoved n (s,e) v
                    | None -> n
            

    type Tree<'a , 'v when 'a : comparison>(r : Option<Node<'a,'v>>) =
        do if (r.IsSome && not(ValidateNodeInterval r.Value)) 
            then invalidArg "r" "'From' must be earlier than 'To'"

        member x.RootNode = r

        member x.CountNodes = 
            match x.RootNode with
                | Some n -> 1 + (CountChildNodes n)
                | None -> 0

    let TreeWithNodeAdded (t:Tree<'a,'v>) (n:Node<'a,'v>) = 
        if not(ValidateNodeInterval n) then
            invalidArg "r" "'From' must be earlier than 'To'"
        if t.RootNode.IsNone then 
            Tree(Some n) 
        else 
            let newRoot = Compound t.RootNode.Value n
            Tree(Some newRoot)
            
    let TreeWithValueRemoved (t:Tree<'a,'v>) (s:'a, e:'a) (v:'v) =
        match t.RootNode with
            | None -> t
            | Some r -> Tree(Some (NodeWithValueRemoved r (s,e) v))