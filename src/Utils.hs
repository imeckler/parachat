module Utils where

import Data.Maybe

ignoreM :: Monad m => m a -> m ()
ignoreM = (>> return ())

fromRight :: Either a b -> b
fromRight (Right x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

may :: Maybe a -> b -> (a -> b) -> b
may m def f = maybe def f m

maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

decorate :: (a -> b) -> a -> (a, b)
decorate f x = (x, f x)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixr 9 .>
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

infixl 1 >>|
(>>|) :: Functor f => f a -> (a -> b) -> f b
(>>|) = flip fmap

bind :: Monad m => (a -> m b) -> m a -> m b
bind = flip (>>=)

boolElim :: a -> a -> Bool -> a
boolElim t f b = if b then t else f

