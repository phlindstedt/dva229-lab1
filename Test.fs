[<RequireQualifiedAccess>]
module Test =

    open FsCheck
    open FsCheck.Xunit

    module GenList =
        // Generates a list with only unique elements
        let genListWithUniqueElements: Gen<int list> =
            Gen.listOf (Gen.choose (-100, 100)) |> Gen.map List.distinct

        // Generates a list where each unique element is duplicated a random number of times
        let genListWithRandomDuplicates: Gen<int list> =
            genListWithUniqueElements
            |> Gen.map (fun xs ->
                xs
                |> List.map (fun x -> Gen.choose (1, 3) |> Gen.map (fun n -> List.replicate n x)) // Each element gets duplicated 1-3 times
                |> Gen.sequence
            )
            |> Gen.map (fun innerGen -> Gen.sample 1 1 innerGen |> List.head) // Flatten nested Gen
            |> Gen.map List.concat // Flatten Gen list

    module TwoListsGenerator =
        let genDifferentLists: Gen<int list * int list> =
            Gen.zip GenList.genListWithRandomDuplicates GenList.genListWithRandomDuplicates

        let genEdgeCaseLists: Gen<int list * int list>=
            Gen.oneof [
                Gen.constant ([], [])                             // Both empty
                Gen.map (fun xs -> (xs, [])) GenList.genListWithRandomDuplicates  // One empty (xs)
                Gen.map (fun ys -> ([], ys)) GenList.genListWithRandomDuplicates  // One empty (ys)
                Gen.map (fun xs -> (xs, xs)) GenList.genListWithRandomDuplicates  // Already unioned
                Gen.map (fun xs -> (xs @ xs, xs)) GenList.genListWithRandomDuplicates // Duplicates in xs
                Gen.map (fun ys -> (ys, ys @ ys)) GenList.genListWithRandomDuplicates // Duplicates in ys
                genDifferentLists                             // Random case
            ]

        type EdgeCaseArbitrary =
            static member Lists() = Arb.fromGen genEdgeCaseLists

    module OneListGenerator =
        let genEdgeCaseList: Gen<int list> =
            Gen.oneof [
                Gen.constant [] // Empty list
                GenList.genListWithUniqueElements // Unique elements only
                GenList.genListWithRandomDuplicates // List with random duplicates
            ]

        type EdgeCaseArbitrary =
            static member List() = Arb.fromGen genEdgeCaseList

    module OddsGenerator =
        // Converts an int to a list of its odd digits
        let extractOdds (x: int) : int list =
            let isNegative = 
                match x < 0 with
                | true -> -1
                | _ -> 1
            abs x  // Convert to absolute value to remove negative signs
            |> string  // Convert number to string
            |> Seq.map (fun c -> (int c - int '0') * isNegative)  // Convert characters to digits
            |> Seq.filter (fun n -> n % 2 <> 0)  // Keep only odd numbers
            |> Seq.toList  // Convert to list

        // Generates random integers for testing
        let genRandomInt: Gen<int> =
            Gen.choose (0, 999999) // Random integers from 0 to 999999

        // Generates numbers that only have even digits
        let genEvenOnlyInt: Gen<int> =
            Gen.listOf (Gen.elements ['0'; '2'; '4'; '6'; '8']) // Choose from even digits
            |> Gen.map (fun lst -> if lst = [] then 0 else System.Int32.Parse(System.String(lst |> List.toArray)))

        // Generates numbers that only have odd digits
        let genOddOnlyInt: Gen<int> =
            Gen.listOf (Gen.elements ['1'; '3'; '5'; '7'; '9']) // Choose from odd digits
            |> Gen.map (fun lst -> if lst = [] then 0 else System.Int32.Parse(System.String(lst |> List.toArray)))

        // Generates numbers that contain both even and odd digits
        let genMixedInt: Gen<int> =
            Gen.listOf (Gen.elements ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']) // Choose from all digits
            |> Gen.map (fun lst -> if lst = [] then 0 else System.Int32.Parse(System.String(lst |> List.toArray)))

        // Generator that covers all cases
        let genEdgeCaseInt: Gen<int> =
            Gen.oneof [
                Gen.constant 0 // Edge case: 0 should return []
                genRandomInt // Random numbers
                genEvenOnlyInt // Numbers with only even digits
                genOddOnlyInt // Numbers with only odd digits
                genMixedInt // Numbers with mixed even and odd digits
            ]

        // FsCheck Arbitrary instance
        type EdgeCaseArbitrary =
            static member Ints() = 
                Arb.fromGen (genEdgeCaseInt |> Gen.map (fun x -> (x, extractOdds x))) // Generates (int, expected result)


    type ``Lab1 List Functions`` () =

        [<Property>]
        member _.``Mem Randomized Test`` (x: int, xs: int list) =
            Xunit.Assert.Equal(List.contains x xs, Lab2.List.mem x xs)

        [<Property(Arbitrary = [| typeof<TwoListsGenerator.EdgeCaseArbitrary> |])>]
        member _.``Union Randomized Test`` (xs: int list, ys: int list) =
            let result = Lab2.List.union xs ys |> List.sort
            let expected = Set.union (Set.ofList xs) (Set.ofList ys) |> Set.toList |> List.sort
            Xunit.Assert.Equivalent(expected, result, true)

        [<Property(Arbitrary = [| typeof<TwoListsGenerator.EdgeCaseArbitrary> |])>]
        member _.``Intersection Randomized Test`` (xs: int list, ys: int list) =
            let expected = xs |> List.distinct |> List.filter (fun x -> List.contains x ys)
            let result = Lab2.List.intersection xs ys
            Xunit.Assert.Equivalent(expected, result, true)

        [<Property(Arbitrary = [| typeof<TwoListsGenerator.EdgeCaseArbitrary> |])>]
        member _.``SetMinus Randomized Test`` (xs: int list, ys: int list) =
            let expected = xs |> List.filter(fun x -> not (List.contains x ys)) |> List.distinct
            let result = Lab2.List.setminus xs ys
            Xunit.Assert.Equivalent(expected, result, true)

        [<Property>]
        member _.``Foo1 Randomized Test`` (x: int, xs: int list) =
            let expected = Seq.replicate (List.sumBy (fun y -> if x = y then 1 else 0) xs) x |> Seq.toList
            let result = Lab2.List.foo1 x xs
            Xunit.Assert.Equivalent(expected, result, true)

        [<Property>]
        member _.``Foo2 Randomized Test`` (x: int, xs: int list) =
            let eq  a b = a = b
            let lt  a b = a < b
            let gt  a b = a > b
            let neq a b = a = b

            Xunit.Assert.Equivalent(Lab2.List.foo2 eq  x xs, List.filter (eq  x) xs, true)
            Xunit.Assert.Equivalent(Lab2.List.foo2 lt  x xs, List.filter (lt  x) xs, true)
            Xunit.Assert.Equivalent(Lab2.List.foo2 gt  x xs, List.filter (gt  x) xs, true)
            Xunit.Assert.Equivalent(Lab2.List.foo2 neq  x xs, List.filter (neq  x) xs, true)

        [<Property>]
        member _.``Digits Randomized Test`` (x: int) =
            let expected =
                if x <= 0 then []
                else x.ToString().ToCharArray() |> Array.map (fun c -> int c - int '0') |> Array.toList
            let result: int list = Lab2.List.digits x
            Xunit.Assert.Equivalent(expected, result, true)

        [<Property(Arbitrary = [| typeof<OddsGenerator.EdgeCaseArbitrary> |])>]
        member _.``Odds Randomized Test`` (x: int) =
            let expected = 
                if x <= 0 then []
                else OddsGenerator.extractOdds x
            let result = Lab2.List.odds x
            try
                Xunit.Assert.Equivalent(expected, result, true)
            with
            | :? Xunit.Sdk.XunitException ->
                let errorMessage = sprintf "Test failed for x = %d\nExpected: %A\nGot: %A" 
                                    x expected result
                raise (Xunit.Sdk.XunitException(errorMessage))

    type ``Lab1 Empty Tree Functions`` () =

        [<Xunit.Fact>]
        member _.``Given an empty tree, then tree is not a leaf`` () =
            let result = 
                match (isEmpty Tree.empty, isLeaf Tree.empty) with
                | (true, false) -> true
                | _ -> false
            let errorMessage = sprintf "Test failed: Expected 'isEmpty Tree.empty = true' and 'isLeaf Tree.empty = false', but got different results."
            Xunit.Assert.True(result, errorMessage)
        
        [<Xunit.Fact>]
        member _.``Given an empty tree, when head is called, then error shall be raised`` () =
            Xunit.Assert.Throws<System.Exception>(fun () -> Tree.head Tree.empty |> ignore)

        [<Xunit.Fact>]
        member _.``Given an empty tree, when left is called, then error shall be raised`` () =
            Xunit.Assert.Throws<System.Exception>(fun () -> Tree.left Tree.empty |> ignore)

        [<Xunit.Fact>]
        member _.``Given an empty tree, when right is called, then error shall be raised`` () =
            Xunit.Assert.Throws<System.Exception>(fun () -> Tree.right Tree.empty |> ignore)

    type ``Lab1 Leaf Error Handling`` () =

        [<Xunit.Fact>]
        member _.``Given a leaf, when left is called, then error should be raised`` () =
            Xunit.Assert.Throws<System.Exception>(fun () -> Tree.left (Tree.leaf 1) |> ignore)

        [<Xunit.Fact>]
        member _.``Given a leaf, when right is called, then error should be raised`` () =
            Xunit.Assert.Throws<System.Exception>(fun () -> Tree.left (Tree.leaf 1) |> ignore)

    type ``Lab1 Tree Functionality`` () =

        [<Property>]
        member _.``Given an leaf, then tree is not empty`` (e: int) =
            let l = leaf e
            let result =
                match (isEmpty l, isLeaf l) with
                | (false, true) -> true
                | _ -> false
            Xunit.Assert.True(result, 
                sprintf "Test failed: Expected 'isEmpty %d = false' and 'isLeaf %d = true', but got different results." e e)

        [<Property>]
        member _.``Given a root, then tree is neither empty nor a leaf`` (e1: int) (e2 : int) (e3 : int) =
            let emptyRoot = Tree.root Tree.empty e1 Tree.empty
            let testRoot = Tree.root (Tree.leaf e1) e2 (Tree.leaf e3)

            Xunit.Assert.True(testRoot |> Tree.isEmpty = false, 
                sprintf "Test failed: Expected 'isEmpty (root (leaf %d) %d (leaf %d)) = false', but got different results." e1 e2 e3)
            Xunit.Assert.True(testRoot |> Tree.isLeaf = false, 
                sprintf "Test failed: Expected 'isLeaf (root (leaf %d) %d (leaf %d)) = false', but got different results." e1 e2 e3)
            Xunit.Assert.True(emptyRoot |> Tree.isEmpty = false, 
                sprintf "Test failed: Expected 'isEmpty (root empty %d empty) = false', but got different results." e1)
            Xunit.Assert.True(emptyRoot |> Tree.isLeaf = false, 
                sprintf "Test failed: Expected 'isLeaf (root empty %d empty) = false', but got different results." e1)

        [<Property>]
        member _.``Given a non-empty tree when head is called, the value is retrieved`` (e : int) =
            let testLeaf = Tree.leaf e
            let res1 = testLeaf |> Tree.head
            let res2 = (Tree.root (Tree.leaf (e / 2)) e (Tree.leaf (e / 4)))
                       |> Tree.head
            Xunit.Assert.True((res1 = e), 
                sprintf "Test failed: Expected 'head (leaf %d) = %d', but got different results 'head (leaf %d) = %d. " e e e res1)
            Xunit.Assert.True((res2 = e), 
                sprintf "Test failed: Expected 'head (root (leaf (%d / 2)) %d (Tree.leaf (%d / 4))) = %d', but got different results 'head (root (leaf (%d / 2)) %d (Tree.leaf (%d / 4))) = %d. " e e e e e e e res2)

        [<Property>]
        member _.``Given a root when left is called, the left subtree is retrieved`` (e : int) =
            let expected = Tree.leaf e
            let testRoot = Tree.root (Tree.leaf e) e (Tree.empty) 
                           |> Tree.left
            Xunit.Assert.True((expected = testRoot), 
                sprintf "Test failed: Expected 'leaf (root (leaf %d) %d empty) = %A', but got different results 'leaf (root (leaf %d) %d empty) = %A'." e e expected e e testRoot)

        [<Property>]
        member _.``Given a root when right is called, the right subtree is retrieved`` (e : int) =
            let expected = Tree.leaf e
            let testRoot = Tree.root (Tree.empty) e (Tree.leaf e)
                           |> Tree.right
            Xunit.Assert.True((expected = testRoot), 
                sprintf "Test failed: Expected 'leaf (root empty %d (leaf %d)) = %A', but got different results 'leaf (empty %d root (leaf %d)) = %A'." e e expected e e testRoot)

    type ``Lab1 BST Functionality`` () =
        
        [<Property>]
        member _.``Given a sorted tree t with head n, when value e is inserted and e < n, then e is found in the left subtree of t`` (b: int) (a : NonEmptyArray<int>) =
            let l = a.Get |> List.ofArray  // Convert array to list
            let tree = BST.ofList l        // Create BST from list

            let treeWithB = BST.insert b tree
            let leftSubtree = Tree.left treeWithB |> BST.toList

            let isValid = 
                match b < l.[0] with
                | true -> (List.contains b leftSubtree)
                | false -> not (List.contains b leftSubtree)

            let errorMessage = sprintf "Test failed!\n  List: %A\n  Inserted: %d\n  Left subtree: %A\n  Expected: %d to be in Left Subtree." l b leftSubtree b
            Xunit.Assert.True(isValid, errorMessage)

        [<Property>]
        member _.``Given a sorted tree t with head n, when value e is inserted and e >= n, then e is found in the right subtree of t`` (b: int) (a : NonEmptyArray<int>) =
            let l = a.Get |> List.ofArray
            let tree = BST.ofList l

            let treeWithB = BST.insert b tree
            let rightSubtree = Tree.right treeWithB |> BST.toList

            let isValid = 
                match b >= l.[0] with
                | true -> (List.contains b rightSubtree)
                | false -> not (List.contains b rightSubtree)

            let errorMessage = sprintf "Test failed!\n  List: %A\n  Inserted: %d\n  Right subtree: %A\n  Expected: %d to be in Right Subtree." l b rightSubtree b
            Xunit.Assert.True(isValid, errorMessage)

        [<Property>]
        member _.SortList (xs : int list) =
            let expected = xs |> List.sort
            let result = BST.sortList xs
            try
                Xunit.Assert.Equivalent(expected, result, true)
            with
            | :? Xunit.Sdk.XunitException ->
                let errorMessage = sprintf "Test failed\nExpected: %A\nGot: %A" expected result
                raise (Xunit.Sdk.XunitException(errorMessage))


    type ValidOperators() =
        static member Char() =
            Gen.elements ['+'; '-'; '*'; '/'; '.'; 's'; '%']
            |> Arb.fromGen
    type ValidFloats() =
        static member Float() =
            Arb.Default.Float()
            |> Arb.filter (fun x -> not (System.Double.IsNaN x) && not (System.Double.IsInfinity x))

    [<Properties(Arbitrary = [| typeof<ValidOperators>; typeof<ValidFloats> |])>]
    module ``Lab 1 CalcTree`` =
        type Result = 
            | Float of float 
            | Bool of bool

        let isFloat value =
            match value with
            | Float _ -> true
            | _ -> false

        let isValidFloat r =
            match r with
            | Bool _ -> false
            | Float f -> not (System.Double.IsNaN f || System.Double.IsInfinity f)

        let isCloseEnough a b =
            match a, b with
            | Float f1, Float f2 when isValidFloat (Float f1) && isValidFloat (Float f2) ->
                if System.Double.Equals(f1, 0.0) && System.Double.Equals(f2, 0.0) then true
                else
                    let diff = abs (f1 - f2)
                    let tolerance = 1e-6 * max(1.0) (max (abs f1) (abs f2))  // ✅ Relative tolerance
                    diff < tolerance
            | Float _, Float _ when not (isValidFloat a) && not (isValidFloat b) -> true
            | Bool _, Bool _ -> true
            | _ -> false
    
        let printResult x =
            match x with
            | Bool b -> sprintf "%b" b 
            | Float f -> sprintf "%f" f 
    
        /// Property: Evaluating a number should return itself
        [<Property>]
        let ``Evaluate a number should return the number itself`` (x: float) =
            let exprTree = CalcTree.number x
            let actual = CalcTree.eval exprTree
            Xunit.Assert.True(
                (actual = x),
                sprintf "Test failed for: x=%f, expected=%f, actual=%f" 
                        x x actual
            )

        /// Property: Evaluating an addition expression should return x + y
        [<Property>]
        let ``Evaluate addition expression should return correct result`` (opChar: char) (x: float) (y: float) =
            let mutable expected = Bool false
            let mutable actual = Bool false
        
            try            
                let res = 
                    match opChar with
                    | '+' -> x + y
                    | '-' -> x - y
                    | '*' -> x * y
                    | '/' -> if y <> 0.0 then x / y else failwith "Division by zero"
                    | _   -> failwith "Invalid operator"
                expected <- Float res
            with
            | _ -> expected <- Bool false
            try
                let exprTree = CalcTree.expr (CalcTree.op opChar) (CalcTree.number x) (CalcTree.number y)
                actual <- Float (CalcTree.eval exprTree)
            with
            | _ -> expected <- Bool false
        
            Xunit.Assert.True(
                isCloseEnough expected actual,
                sprintf "Test failed for: op=%c, x=%f, y=%f, expected=%s, actual=%s" 
                        opChar x y (printResult expected) (printResult actual) 
            )

        /// Property: Scaling an expression should multiply the result
        [<Property>]
        let ``Scaling an expression should multiply the result`` (factor: float) (x: float) (y: float) =
            let mutable expected = Bool false
            let mutable actual = Bool false
            try
                let exprTree = CalcTree.expr (CalcTree.op '+') (CalcTree.number x) (CalcTree.number y)
                let scaledTree = CalcTree.scale factor exprTree
                actual <- Float (CalcTree.eval scaledTree) 
            with
            | _ -> actual <- Bool false
            try
                let exprTree = CalcTree.expr (CalcTree.op '+') (CalcTree.number (factor * x)) (CalcTree.number (factor * y))
                expected <- Float (CalcTree.eval exprTree)
            with
            | _ -> expected <- Bool false
        
            Xunit.Assert.True(
                isCloseEnough actual expected,
                sprintf "Test failed for: factor=%f, x=%f, y=%f, expected=%s, actual=%s" 
                        factor x y (printResult expected) (printResult actual) 
            )

        /// Property: Scaling a single number should multiply it
        [<Property>]
        let ``Scaling a single number should return f * x`` (factor: float) (x: float) =
            let mutable expected = Bool false
            let mutable actual = Bool false
            try
                let exprTree = CalcTree.scale factor (CalcTree.number x)
                actual <- Float (CalcTree.eval exprTree)
            with
            | _ -> actual <- Bool false
            try
                 expected <- Float (factor * x)
            with
            | _ -> expected <- Bool false
            Xunit.Assert.True(
                isCloseEnough actual expected,
                sprintf "Test failed for: factor=%f, x=%f, expected=%s, actual=%s" 
                        factor x (printResult expected) (printResult actual)
            )

        /// Property: Division by zero should throw an exception
        [<Xunit.Fact>]  // Using [<Fact>] since FsCheck doesn't handle exceptions well
        let ``Division by zero should fail`` () =
            let exprTree = CalcTree.expr (CalcTree.op '/') (CalcTree.number 5.0) (CalcTree.number 0.0)
            Xunit.Assert.Throws<System.Exception>(fun () -> CalcTree.eval exprTree |> ignore)
