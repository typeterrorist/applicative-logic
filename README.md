# applicative-logic

`applicative-logic` is a Haskell library that generalizes logical operations for
`Applicative` and `Alternative` functors, providing a set of utility functions that
extend common Boolean operations to more abstract structures.

## What is this?

There is a [blog post](https://hakon.gylterud.net/programming/applicative-logic.html) which explains the purpose of this library.

## Installation

### From Hackage


```
 $ cabal install applicative-logic --lib
```

### From GitHub

```
 $ git clone https://github.com/typeterrorist/applicative-logic.git
 $ cd applicative-logic
 $ cabal install
```


## Usage

```haskell
import Prelude hiding (all,any,or,and,(&&)) -- Hide offending Prelude functions
import qualified Prelude -- To retain access to standard functions
import Control.Applicative.Logic

. . .
```
