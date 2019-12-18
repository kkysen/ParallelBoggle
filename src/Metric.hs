module Metric (
    Metric,
    MetricSpace,
    metric,
    metric2,
    by
) where

type Metric a = a -> a -> Double

class MetricSpace a where
    
    metric :: Metric a
    metric x y = sqrt $ metric2 x y
    
    metric2 :: Metric a
    metric2 x y = m * m
      where
        m = metric x y

by :: (a -> b) -> Metric b -> Metric a
by map metric x y = metric (map x) (map y)

