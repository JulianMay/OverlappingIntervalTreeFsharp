namespace FissTesting

open NUnit.Framework
open Time


module TimeTests =


    [<Test>]
    let ``Date's can be compared directly``() =
        let d = Date(2017, 10, 5)
        let da = Date(2017, 10, 5)
        let db = Date(2017, 10, 4)
        let dc = Date(2017, 11, 5)
        let dd = Date(2018, 4, 2)

        Assert.AreEqual(d, da)
        Assert.Greater(d,db)
        Assert.Less(d,dc)
        Assert.Less(d,dd)    


    [<Test>]
    let ``Periods cannot start later than they end``() =
        let ok = new Period(Date(2017,10,5),Date(2017, 10, 6))
        try
            let t = new Period(Date(2017,10,5), Date(2017, 10, 4))
            Assert.Fail "Apperentley they could?"
        with
            | :? AssertionException as ae -> raise ae;
            | _ -> ();
            

        