import Prelude hiding (Applicative , (<*>), pure )

class (Functor f) => Applicative f where
	pure  :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b


instance Applicative Maybe where
	pure a  = Just a
	(<*>) (Just f) applicative = fmap f applicative 
	(<*>) Nothing _ = Nothing

instance Applicative [] deriving (Show) where
  pure a = [a]
  (<*>) [f] [] = []
  (<*>) [f] (x:xs) = [f x] ++ [f] <*> xs