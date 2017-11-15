
module Time
    
    type Date(y:int, m:int, d:int) = 
        member x.Year = y
        member x.Month = m
        member x.Day = d

        override x.ToString() = sprintf "%i/%i/%i" x.Year x.Month x.Day
        override x.GetHashCode() = hash (x.Year, x.Month, x.Day)
        override x.Equals y = 
            match y with
                | :? Date as y -> (x.Year, x.Month, x.Day) = (y.Year, y.Month, y.Day)
                | _ -> false
        interface System.IComparable with
            member x.CompareTo y =
                match y with
                | :? Date as y -> compare (x.Year, x.Month, x.Day) (y.Year, y.Month, y.Day)
                | _ -> invalidArg "y" "cannot compare values of different types"
    

    type Period(f:Date, t:Date) =        
        do if (f>=t) then failwithf "'from' must be earlier than 'to'"
        let From = f
        let To = t