namespace FissTesting

open NUnit.Framework
open Time
open Interval


module IntervalTests =

    let dt (y,m,d) = Date(y,m,d)
    let noChildren = List.empty<Node<Date,string>>
    let nn (d1, d2, v, childs) = {From =d1; To=d2; Values=v; ChildNodes=childs}
    let emptyTree = Tree<Date,string>(None)

    [<Test>]
    let ``Nodes can count it's child-nodes recursively``() =
        let a1 = nn(dt(1, 1, 1), dt(2, 2, 2), ["a1"], noChildren)
        let b1 = nn(dt(1, 1, 1), dt(2, 2, 2), ["b1"], [a1])        
        let b2 = nn(dt(1, 1, 1), dt(2, 2, 2), ["b2"], noChildren)
        let sut = nn(dt(1, 1, 1), dt(2, 2, 2), ["sut"], [b1;b2])

        Assert.AreEqual(3, CountChildNodes sut)

    [<Test>]
    let ``Node cannot be part of tree when From is greater than To``() =
        //gave up constructor for record type, so now we can have invalid nodes
        let invalid = nn(dt(2017,1,10),dt(2017,1,8), [], [])
        printfn "TEST: %i" (compare invalid.From invalid.To)

        try
            let t = Interval.Tree(Some invalid)
            Assert.Fail "Apperently it could?"
        with
            | :? AssertionException as ae -> raise ae;
            | _ -> ();

    [<Test>]
    let ``Interval.Tree can count its nodes``() =
        let root1 = nn(dt(1, 1, 1), dt(2, 2, 2), ["a1"], noChildren)
        let sutWith1 = Interval.Tree(Some root1)
        Assert.AreEqual(1, sutWith1.CountNodes)
            
        let root4 = nn(dt(1, 1, 1), dt(2, 2, 2), ["root"], 
                            [  nn(dt(1, 1, 1), dt(2, 2, 2), ["b2"], noChildren);
                                nn(dt(1, 1, 1), dt(2, 2, 2), ["b1"], 
                                    [  nn(dt(1, 1, 1), dt(2, 2, 2), ["leaf"], noChildren)  ])   
                            ])
        let sutWith4 = Interval.Tree(Some root4)
        Assert.AreEqual(4, sutWith4.CountNodes)

    //Periods for maintenance lookup tests
    let a = dt(2017,1,3),dt(2017,1,5)
    let b = dt(2017,1,7),dt(2017,1,12)
    let c = dt(2017,1,10),dt(2017,1,11)
    let d = dt(2017,1,11),dt(2017,2,1)
    let e = dt(2017,1,10),dt(2017,1,11) //e is equal to c

    [<Test>]
    let ``Siblings will be grouped in new parent root``() =   
        let sut = [ nn(fst a, snd a, ["a"], noChildren);
                    nn(fst b, snd b, ["b"], noChildren) ]
                |> Seq.fold (fun t n-> TreeWithNodeAdded t n) emptyTree
        
        Assert.AreEqual(3, sut.CountNodes)
        Assert.That(sut.RootNode.IsSome)
        let root = sut.RootNode.Value
        CollectionAssert.IsEmpty(root.Values)
        Assert.AreEqual((fst a, snd b), (root.From, root.To))
        let nao = sut.RootNode.Value.ChildNodes |> Seq.tryFind (fun (n:Node<_,_>) -> Seq.contains "a" n.Values) 
        let nbo = sut.RootNode.Value.ChildNodes |> Seq.tryFind (fun (n:Node<_,_>) -> Seq.contains "b" n.Values)
        Assert.That(nao.IsSome)
        let na = nao.Value
        Assert.That(na.From = fst a)
        Assert.That(na.To = snd a)
        Assert.That(na.ChildNodes = noChildren)

    [<Test>]
    let ``Equal intervals aggregates values on single node``() =
        let x = nn(fst c, snd c, ["a"], noChildren)
        let y = nn(fst c, snd c, ["b"], noChildren)
        let before = TreeWithNodeAdded emptyTree x
        let sut = TreeWithNodeAdded before y

        Assert.AreEqual(1, sut.CountNodes)
        CollectionAssert.AreEquivalent(["a";"b"], sut.RootNode.Value.Values)

    [<Test>]
    let ``new node becomes child when within interval of existing``() =
        let existing = nn(dt(2017,3,3), dt(2017,3,20), ["existing"], noChildren)
        let newNode = nn(dt(2017,3,10), dt(2017,3,15), ["new"], noChildren)
        let before = Tree(Some existing)
        
        let sut = TreeWithNodeAdded before newNode

        let r = sut.RootNode.Value
        Assert.AreEqual(existing.From, r.From)
        Assert.AreEqual(existing.To, r.To)
        CollectionAssert.AreEqual(["existing"], r.Values)
        Assert.AreEqual(1, r.ChildNodes.Length)
        Assert.AreEqual(newNode, r.ChildNodes.Head)

           
    [<Test>]
    let ``new node becomes parent when existing is within it's interval``() =
        let newNode = nn(dt(2017,3,3), dt(2017,3,20), ["new"], noChildren)
        let existing = nn(dt(2017,3,10), dt(2017,3,15), ["existing"], noChildren)
        let before = Tree(Some existing)
        
        let sut = TreeWithNodeAdded before newNode

        let r = sut.RootNode.Value
        Assert.AreEqual(newNode.From, r.From)
        Assert.AreEqual(newNode.To, r.To)
        CollectionAssert.AreEqual(["new"], r.Values)
        Assert.AreEqual(1, r.ChildNodes.Length)
        Assert.AreEqual(existing, r.ChildNodes.Head)

    [<Test>]
    let ``tree is structurally untouched when a removed value was not the last``() =
        let existing = nn(dt(2017,3,9), dt(2017,3,15), ["t"], 
                            [nn(dt(2017,3,10),dt(2017,3,11), ["one";"two";"three"], noChildren)])
        let before = Tree(Some existing)

        let sut = TreeWithValueRemoved before (dt(2017,3,10),dt(2017,3,11)) "two"

        Assert.That(sut.RootNode.IsSome);
        Assert.That(sut.RootNode.Value.ChildNodes.Length, Is.EqualTo(1));
        Assert.AreEqual(["one"; "three"],sut.RootNode.Value.ChildNodes.Head.Values)

    [<Test>]
    let ``tree reorganizes when nodes with one childnode become value-empty``() =
        let existing = nn(dt(2017,3,9), dt(2017,3,15), ["t"], 
                            [nn(dt(2017,3,10),dt(2017,3,14), ["two"],  //<- This level is removed when the value "two" is removed
                                [nn(dt(2017,3,11),dt(2017,3,11), ["x"], noChildren)])])
        let before = Tree(Some existing)

        let sut = TreeWithValueRemoved before (dt(2017,3,10),dt(2017,3,14)) "two"
        Assert.That(sut.RootNode.IsSome);
        Assert.That(sut.RootNode.Value.ChildNodes.Length, Is.EqualTo(1));
        Assert.That(sut.RootNode.Value.ChildNodes.Head.ChildNodes.Length, Is.EqualTo(0));
        Assert.AreEqual(["x"],sut.RootNode.Value.ChildNodes.Head.Values)


