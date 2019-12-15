module WStore where

import Relude.Extra.Map

data WStore s a = WStore { wMap :: Map s a
                         , wPos :: s
                         }

instance Functor (WStore s) where
  fmap f (WStore m p) = WStore (fmap f m) p

wStore' :: Ord s => s -> WStore s a
wStore' = WStore $ fromList []

wStore :: (Enum s, Num s, Ord s) => [a] -> WStore s a
wStore vs = WStore (fromList $ zip [0..] vs) 0

wExtract :: Ord s => WStore s a -> Maybe a
wExtract (WStore m p) = lookup p m

wPeek :: Ord s => s -> WStore s a -> Maybe a
wPeek p (WStore m _) = lookup p m

wPeeks :: Ord s => (s -> s) -> WStore s a -> Maybe a
wPeeks f (WStore m p) = lookup (f p) m

wSeek :: s -> WStore s a -> WStore s a
wSeek p (WStore m _) = WStore m p

wSeeks :: (s -> s) -> WStore s a -> WStore s a
wSeeks f (WStore m p) = WStore m (f p)

wExperiment :: (Ord s, Functor f) => (s -> f s) -> WStore s a -> f (Maybe a)
wExperiment f w = fmap (flip wPeek w) (f (wPos w))

wWrite :: Ord s => s -> Maybe a -> WStore s a -> WStore s a
wWrite k (Just v) (WStore m p) = WStore (insert k v m) p
wWrite _ Nothing ws = ws
