
module StateTransformer where

-- Simplest way to define a state is a type of value we want to manipulate, e.g.
--
type State = Int
--
-- The most basic form of function on this type is a `state transformer`, which
-- takes an imput state and produces output state
--
--   type ST = State -> State
--
-- In general however, we want to return a result value in addition to updating
-- the state. For example, if the state represents a counter, a function for
-- incrementing the counter may also wish to return its current value. So we
-- generalise type of state transformers to also return a result value
--
--   type ST a = State -> (a, State)
--
-- Pictorially, we show state transformer as follows
--
--          +----+
--          |    |----> v
--          |    |
--   s ---->|    |----> s'
--          +----+
--
-- Conversely, a state transformer may also wish to take argument values. But
-- we exploit currying for this effect. E.g. state transformer that takes a
-- character and returns an integer would have a type
--
--   Char -> ST Int
--
-- or in other words
--
--   Char -> State -> (Int, State)
--
-- and pictorially
--
--          +----+
--   c ---->|    |----> v
--          |    |
--   s ---->|    |----> s'
--          +----+
--
-- Given that ST is a parameterised type it's natural to try and make it into a
-- monad to be able to use the `do` notation when writing stateful programs.
-- So we redefine ST using `newtype` mechanism, which requries a dummy data
-- constructor that we will name `S`

newtype ST a = S (State -> (a, State))

-- For convenience we also define special-purpose application function `app`

app :: ST a -> State -> (a, State)
app (S st) a = st a
-- or in point-free style
--   app (S st) = st
--
--        +-----+
--        |     |----> a
--        | app |
-- s ---->|     |----> s'
--        +-----+

-- Making ST into a Monad starts with Functor and Applicative

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f sa = S (\s -> let (a, s') = app sa s in (f a, s'))
--
--         +----+   a   +---+
--         |    |------>| f |----> f a
--         | st |       +---+
--  s ---->|    |----------------> s'
--         +----+

instance Applicative ST where
  -- pure :: a -> ST a
  pure a = S(\s -> (a, s))
--
--        +------+
-- x ---->|      |----> x
--        | pure |
-- s ---->|      |----> s
--        +------+

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S (\s -> let (f, s')  = app stf s
                             (x, s'') = app stx s'
                          in (f x, s''))
--
--        +-----+  f               +---+
--        |     |----------------->|   |----> f x
--        |     |      +-----+     | $ |
--        | stf |      |     |---->|   |
--        |     |      | stx |     +---+
-- s ---->|     |----->|     |--------------> s''
--        +-----+  s'  +-----+

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s
                       in app (f x) s')

--        +----+  x   +---+
--        |    |----->|   |----> f x
--        | st |      | f |
-- s ---->|    |----->|   |----> s''
--        +----+  s'  +---+

-- Relabelling trees
-- =================

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a')
                  (Leaf 'b'))
            (Leaf 'c')

-- problem, write a function that relabels each leaf with a unique integer

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where (l', n')  = rlabel l n
        (r', n'') = rlabel r n'

-- but the above is a little complicated by having to thread fresh integer
-- values through the calls

-- simplify by using State

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- using the fact that ST is an applicative functor we can define labelling
-- differently
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- using the fact that ST is a monad we can define it using the `do` notation
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)
mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')
  
