# Operate Do

This packages provides an useful syntax sugar for infixing.

## Usage

```haskell
-- Same as `pure const <*> Just 1 <*> Just 2 == Just 1`
[opdo| <*> ->
  pure const
  Just 1
  Just 2
  |]
```

## The reason why

When we make complexible program, we use many brackets:

```haskell
dataParser = ComplexData
  <$> (takeWhile1 isToken <* char8 ' ')
  <*> (takeWhile1 (/= 32) <* char8 ' ')
  <*> (some <* endOfLine)
```

`Operate do` provides non-use brackets interface:

```haskell
-- Or `[opdo| <*> -> { pure ComplexData; ... } |]`
[opdo| <*> ->
  pure ComplexData
  takeWhile1 isToken <* char8 ' '
  takeWhile1 (/= 32) <* char8 ' '
  some <* endOfLine
  |]
```

Of course, you can free to use `$`.  Moreover, this is used as weak `do`:

```haskell
-- Same `do { putStrLn "Hello"; putStrLn "World" }`
[opdo| >> ->
  putStrLn "Hello"
  putStrLn "World"
  |]
```

## Syntax

```text
<operate do>     ::= [opdo| <opdooperator> -> { <opdostatements> <expression> [;] }]
<opdooperator>   ::= <identifer>
                   | ( <identifer> )
<opdostatements> ::= <opdostatements> <opdostatement>
                   |
<opdostatement>  ::= <expression> ;
                   | ;
```

An example:

```haskell
[opdo| >>> -> { (+ 1); show; head |] :: Num a => a -> Char

# Same (use section version)
[opdo| (>>>) -> { (+ 1); show; head } |]
```

Allow multiline statement same as `do` notation:

```haskell
[opdo| >>> ->
  (+ 1)
  show
  head
|]

# Same (brackets multiline)
[opdo| >>> -> {
  (+ 1);
  show;
  head;
}|]
```

This is not supported from some reason:

```haskell
# This is compile error!
[opdo| >>> -> (+ 1)
              show
              head
|]
```

## Translation

### For operation

Taking associativity into consideration:

```haskell
[opdo| op -> a; b; c |] == a op b op c
```

### For function

Take left associativity:

```haskell
[opdo| func -> a; b; c |] == a `func` b `func` c
```
