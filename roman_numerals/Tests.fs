open Xunit

[<Fact>] let ``quotrem`` () = Assert.Equal((2, 1), roman.quotRem 5 2)
[<Fact>] let ``quotrem2`` () = Assert.Equal((1, 0), roman.quotRem 5 5)
[<Fact>] let ``quotrem3`` () = Assert.Equal((0, 4), roman.quotRem 4 5)
[<Fact>] let ``quotrem4`` () = Assert.Equal((1, 2), roman.quotRem 7 5)

[<Fact>] let ``I`` () = Assert.Equal("I", roman.toRoman 1)
[<Fact>] let ``II`` () = Assert.Equal("II", roman.toRoman 2)
[<Fact>] let ``III`` () = Assert.Equal("III", roman.toRoman 3)
[<Fact>] let ``IV`` () = Assert.Equal("IV", roman.toRoman 4)
[<Fact>] let ``V`` () = Assert.Equal("V", roman.toRoman 5)
[<Fact>] let ``VI`` () = Assert.Equal("VI", roman.toRoman 6)
[<Fact>] let ``VII`` () = Assert.Equal("VII", roman.toRoman 7)
[<Fact>] let ``VIII`` () = Assert.Equal("VIII", roman.toRoman 8)
[<Fact>] let ``IX`` () = Assert.Equal("IX", roman.toRoman 9)