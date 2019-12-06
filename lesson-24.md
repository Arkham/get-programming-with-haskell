# Working with files

In this chapter we will:

- Work with file handles in Haskell
- Read from and write to files
- Understand limitations of lazy evaluation for I/O

## Opening and closing files

Let's work with this simple file:

```
Hello world!
Goodbye world!
```

Next we will import the `System.IO` module and use the `openFile` function

```
openFile :: FilePath -> IOMode -> IO Handle
```

Let's explore this types in GHCi

```
Prelude System.IO> :info FilePath
type FilePath = String

Prelude System.IO> :info IOMode
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

Prelude System.IO> :info Handle
data Handle
  = GHC.IO.Handle.Types.FileHandle FilePath
  | GHC.IO.Handle.Types.DuplexHandle FilePath
```

The first two types are quite straight-forward, so let's talk about the
third: the `Handle` type is a file handle that lets you pass around a
reference to a file. As we've discussed throughout the unit, the `IO` type
means that you have a handle in the context of `IO`.

```haskell
main :: IO ()
main = do
  myFile <- openFile "hello.txt" ReadMode
  hClose myFile
  putStrLn "done!"
```

As you can see, after we open the file with `openFile` we can close it with
`hClose`.

Opening and closing a file is not that exciting: you clearly want to read
and write to files. You can use two functions to do that:

- `hPutStrLn` works like `putStrLn` but requires a file handle. In fact the
  latter is a specific version where the file handle is `stdout`.
- `hGetLine` works like `getLine` but requires a file handle. The latter is
  a specific version that uses `stdin` as the file handle.

Here's a program that reads the first line of `hello.txt` and writes it to
the console, then reads the second line and writes it to a new file
`goodbye.txt`.

```haskell
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
```

In this program we assumed that `hello.txt` had at least two lines. How
would we check that the file is long enough? Using `hIsEOF`!

```haskell
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  firstMissing <- hIsEOF helloFile
  firstLine <- if not firstMissing
               then hGetLine helloFile
               else return ""
  secondMissing <- hIsEOF helloFile
  secondLine <- if not secondMissing
                then hGetLine helloFile
                else return ""
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile secondLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "done!"
```

## Simple I/O tools

There are easier ways to deal with files:

```haskell
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()
```

To better understand these functions we will create a program that takes a
file as an argument, then counts the characters, words, and lines in the
file. It would output to another file called `stats.dat` something like
this:

```
hello.txt chars: 29 words: 5 lines: 2
what.txt chars: 30000 words: 2404 lines: 1
```

You can check out the source in `examples/app/FileCounts.hs`.

## The trouble with lazy I/O

You will notice that if you run the program on `stats.dat` this will
happen:

```
$ stack exec file-counts-exe stats.dat
file-counts-exe: stats.dat: openFile: resource busy (file is locked)
```

The problem is that `readFile` doesn't close the file handle. Here's the
implementation in Haskell:

```haskell
readFile :: FilePath -> IO String
readFile name = do
  inputFile <- openFile name ReadMode
  hGetContents inputFile
```

Let's try to fix our main by replacing `readFile` with a more manual
implementation that closes the file

```haskell
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  hClose file
  let summary = (countsText . getCounts) input
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
```

If we try to run this version, we will bump into a pretty arcane error
message!

```
file-counts-exe: stats.dat: hGetContents: illegal operation (delayed read on closed handle)
```

The problem here is caused by lazy evaluation:

- Your input isn't used until you define `summary`
- `summary` isn't used until you call `appendFile`
- `appendFile` performs an `IO` action which evaluates `summary` and
  `input`
- by that time, the file has already been closed, because the `hClose file`
  line is an `IO` action that must happen as soon as you evaluate it.

So what can we do to fix this? We could close the file after `appendFile`,
but that wouldn't solve the problem, since we want the new file handle to
be able to append to the same file!

So instead we have to force the evaluation of `summary` first, then close
the file, then append!

```haskell
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = (countsText . getCounts) input
  putStrLn summary
  hClose file
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
```

This should be a lesson that lazy I/O can lead to some really nasty bugs.

## Strict I/O

The best solution to this problem is to use a strict type. We used
`Data.Text` before and one of its features is being strict! You can rewrite
the program using `Text` and all issues will be gone.

## Summary

In this lesson we have seen how to open, read and write to files. Although
lazy I/O can greatly simplify code, it becomes very difficult to reason
about as the complexity of the program increases.
