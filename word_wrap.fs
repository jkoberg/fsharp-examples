module Main

open System
open System.Text.RegularExpressions


let requirement = """
When placing orders through the online system, customers are allowed to add special instructions 
to their orders, for instance specifying that they don't need utensils or that they want their 
steak cooked a certain way.

Restaurant receipt printers have restrictions for how many characters can fit on each line.
This means that in many cases special instructions must be wrapped and put on multiple lines
in order for the text to fit.

Write a function 'WordWrap' that takes as input a string (the special instructions) and an
integer (the character limit) and returns the special instructions wrapped onto lines with
no line exceeding the character limit.
"""


module Wrapping =

    /// A convenient method for appending a single item to a sequence
    let append<'a> (sa:seq<'a>) (a: 'a) = 
        Seq.append sa (Seq.singleton a)


    let rec reassemble (limit:int) (remainingWords:string list) (currentLine:string) (output:string seq): string seq = 

        // If the current line is empty, we don't need to append a space - so we have `limit` characters remaining to fill
        // Otherwise, we will be appending a space, so we have one less character.
        let remainingChars = 
            if currentLine = "" 
                then limit 
                else (limit - currentLine.Length - 1)
                
        // If no space remains - append the current line and continue
        if remainingChars <= 0
        then reassemble limit remainingWords "" (append output currentLine)
        else match remainingWords with 

                // No more words to process - append the current line to the output in progress and return that as the final output
                | [] -> append output currentLine 
        
                // The next word will fit on this line in whole.  Append it (with a space if needed), and continue
                | word :: tail when word.Length <= remainingChars -> 
                    let newLine = if currentLine = "" then word else $"{currentLine} {word}"
                    reassemble limit tail newLine output
            
                // The next word will not fit in the remaining space, but it will fit on the next line.
                // Output the current line and continue.
                | word :: tail when word.Length <= limit ->
                    reassemble limit remainingWords "" (append output currentLine)

                // The next word will not fit in the remaining space, and it won't fit on the next line.
                //    * Split that word into two - one that will fit, and the rest, and continue.
                | word :: tail when word.Length > limit ->
                    let splitWord = [word.[0..(remainingChars - 1)]; word.[remainingChars..]]
                    reassemble limit (List.append splitWord tail) currentLine output 



    /// Split a message into a lines of at most `charlimit` characters 
    let WordWrap (charlimit:int) (message:string) = 

        // Split the input on any whitespace
        let tokens: string list = Regex.Split(message, @"\s") |> Array.toList 

        // reassemble the words into a sequence of strings
        let output: string seq = reassemble charlimit tokens "" Seq.empty

        // Output a single string
        String.Join('\n', output)


open Wrapping

module WrappingTests = 
    // No words that are longer than a line
    let test1 () = 
        let input = "one two three four five six seven"
        let limit1 = 10
        // ||||||||||
        // one two
        // three four
        // five six
        // seven
        let expected = ["one two"; "three four"; "five six"; "seven"]
        let expectedString = String.Join('\n', expected)
        let actual = WordWrap limit1 input
        assert (actual = expectedString)
        Console.WriteLine(actual)

    // Word longer than a line that must be split
    let test2 () = 
        let input = "one two three supercalifragilisticexpiealidocious four"
        let limit1 = 10
        // ||||||||||
        // one two
        // three supe
        // rcalifragi
        // listicexpi
        // ealidociou
        // s four
        let expected = ["one two"; "three supe"; "rcalifragi"; "listicexpi"; "ealidociou"; "s four"]
        let expectedString = String.Join('\n', expected)
        let actual = WordWrap limit1 input
        assert (actual = expectedString)
        Console.WriteLine(actual)
        
    // Words of exactly the length of a line
    let test3 () = 
        let input = "one two three supe rcalifragi listicexpi ealidociou s four"
        let limit1 = 10
        // ||||||||||
        // one two
        // three supe
        // rcalifragi
        // listicexpi
        // ealidociou
        // s four
        let expected = ["one two"; "three supe"; "rcalifragi"; "listicexpi"; "ealidociou"; "s four"]
        let expectedString = String.Join('\n', expected)
        let actual = WordWrap limit1 input
        assert (actual = expectedString)
        Console.WriteLine(actual)


      
open WrappingTests


[<EntryPoint>]
let main argv =

    let allTests = [test1; test2; test3]

    for test in allTests do
      test()

    printfn "All tests ran"

    0

    