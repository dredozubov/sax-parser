# sax
Fast monadic streaming XML parser.

It feels and looks like any other parser combinators library.
To make it more performant it uses continuations under the hood.

`sax` parses a stream of events and employs a skipping strategy to deal with tag
closing and similar issues by having special combinators with a skipping semantics.

Just-get-me-up-to-speed example
-------------------------------

```haskell
newtype World = World ByteString deriving (Show, Eq)
data Hello = Hello
  { hHello :: ByteString
  , hWorld :: World
  , hIsDom :: Bool
  , hNonExistent :: [ByteString]
  } deriving (Show, Eq)

helloXml :: ByteString
helloXml = "<?xml version=\"1.1\"?><f><foo bla=\"alb\"><bar><hello><inner>Hello</inner><skipMe><meToo>and Me</meToo></skipMe><world> wor</world><world>ld!</world></hello></bar></foo><quuz>ok</quuz></f>"

helloParser :: SaxParser Hello
helloParser = do
  -- skips to the opening of tag "hello", it'll handle tag-closing events
  -- for tags it skipped implicitly, meaning that the parser won't break
  -- when it'll encounter a SAX events `CloseTag "f"`, `CloseTag "foo"`
  -- and `CloseTag "bar"`
```

Benchmarks
----------

No "open-source" benchmarking in the repo, sorry for that.
