module Dijkstra where

    import qualified SBH_Bootstrapped as SBHB

    type Vertex = Int
    type Weight = Int
    type Edge = (Vertex, Weight)
    type Graph = Vertex -> [Edge]

--     -- Dijkstra's algorithm
--     dijkstra :: Graph -> Vertex -> [Weight]
--     dijkstra g v = dijkstra' (SBHB.fromList [(v, 0)]) g []

--     dijkstra' :: SBHB.Heap (Vertex, Weight) -> Graph -> [Weight] -> [Weight]
--     dijkstra' h g ws
--       | SBHB.isEmpty h = ws
--       | otherwise      = let ((v, d), h') = SBHB.deleteMin h
--                              ws' = d : ws
--                              es = g v
--                              h'' = SBHB.insertAll (map (\(v', d') -> (v', d' + d)) es) h'
--                          in dijkstra' h'' g ws'
  