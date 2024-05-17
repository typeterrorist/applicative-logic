# applicative-logic

`applicative-logic` is a Haskell library that generalizes logical operations for
`Applicative` and `Alternative` functors, providing a set of utility functions that
extend common Boolean operations to more abstract structures.

## Installation

### From GitHub

```
 $ git clone 
 $ cd applicative-lgoic
 $ cabal install
```


## Usage

```haskell
import Prelude hiding (all,any,or,and,(&&)) -- Hide offending Prelude functions
import qualified Prelude -- To retain access to standard functions
import Control.Applicative.Logic

. . .
```
