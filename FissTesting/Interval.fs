module Interval

    type Node<'a , 'v when 'a : comparison>(f : 'a, t : 'a, v : 'v list, c : Node<'a,'v> list)  =
        do if (f>=t) then failwithf "'from' must be less than 'to'"
        member x.From = f
        member x.To = t
        member x.Values = v
        member x.ChildNodes = c
        
        member x.IsWithinPeriodOf (n:Node<'a, _>) = x.From >= n.From && x.To <= n.To
        member x.IsSamePeriodAs (n:Node<'a, _>) = x.From = n.From && x.To = n.To
        member x.IsIntersecting (n:Node<'a, _>) = (x.From < n.From && n.From < x.To) || (x.From < n.To && n.To < x.To)
        
        member x.WithChildNode (n:Node<'a,'v>) = Node(x.From,x.To,x.Values,n::x.ChildNodes)

    let rec CountChildNodes (n: Node<'a,'v>) : int =
        n.ChildNodes 
            |> List.map (fun c -> if c.ChildNodes.Length = 0 then 1 else 1 + CountChildNodes c)
            |> List.sum 


    let least a b = if a < b then a else b
    let greatest a b = if a > b then a else b   

    let Compound (a:Node<'a,'v>) (b:Node<'a,'v>) =     
        if (a.IsSamePeriodAs b) then Node(a.From, a.To, List.append a.Values b.Values, [])
        else if (a.IsWithinPeriodOf b) then b.WithChildNode a
        else if (b.IsWithinPeriodOf a) then a.WithChildNode b
        else Node(least a.From b.From, greatest a.To b.To, [], [a;b])


    type Tree<'a , 'v when 'a : comparison>(r : Option<Node<'a,'v>>) =
        member x.RootNode = r

        member x.CountNodes = 
            match x.RootNode with
                | Some n -> 1 + (CountChildNodes n)
                | None -> 0

    let TreeWithNodeAdded (t:Tree<'a,'v>) (n:Node<'a,'v>) = 
        if t.RootNode.IsNone then Tree(Some n)
        else 
            let newRoot = Compound t.RootNode.Value n
            Tree(Some newRoot)
