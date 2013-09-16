module Utils where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

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
