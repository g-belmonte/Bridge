{-# LANGUAGE LexicalNegation #-}

module Main where

import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event (..))
import Linear (V2 (..), normalize, (*^))

main :: IO ()
main =
  play
    ( InWindow
        "Bridge"
        (800, 600)
        (0, 0)
    )
    white
    60
    initWorld
    draw
    event
    step

-- Main loop functions

draw :: World -> Picture
draw world =
  foldMap (\node -> drawNode node) world.nodes
    <> foldMap (\edge -> drawEdge edge world.nodes) world.edges
  where

drawNode :: Node -> Picture
drawNode node = case node of
  Node {position = (V2 x y), state = Fixed} -> Color red $ Translate x y (ThickCircle 15 4)
  Node {position = (V2 x y), state = Free} -> Translate x y (ThickCircle 15 4)

drawEdge :: Edge -> Map Int Node -> Picture
drawEdge edge nodes =
  Polygon [p1, p2, p3, p4]
  where
    (nodeIdxA, nodeIdxB) = edge.nodeIndices
    (V2 x y) = getNodePosition nodeIdxA nodes
    (V2 x' y') = getNodePosition nodeIdxB nodes
    thickness = 3
    (V2 nx ny) = thickness *^ normalize (V2 (y - y') (x' - x))
    p1 = ((x + nx), (y + ny))
    p2 = ((x - nx), (y - ny))
    p3 = ((x' - nx), (y' - ny))
    p4 = ((x' + nx), (y' + ny))

event :: Event -> World -> World
event _event world = world

step :: Float -> World -> World
step dt w = applyN i (applyPhysics (dt / i)) w
  where
    i = 150
    applyN 0 _ v = v
    applyN n f v = applyN (n-1) f (f v)

-- Bridge

data World = Bridge
  { edges :: [Edge],
    nodes :: Map Int Node
  }

data Edge = Edge
  { nodeIndices :: (Int, Int),
    initialLength :: Float,
    currentLength :: Float,
    elasticityCoef :: Float,
    dampingCoef :: Float
  }
  deriving (Show)

mkEdge :: Int -> Int -> Edge
mkEdge idxA idxB =
  Edge
    { nodeIndices = (idxA, idxB),
      initialLength = len,
      currentLength = len,
      elasticityCoef = 1000,
      dampingCoef = 0.1
    }
  where
    len = vectorLength $ (getNodePosition idxB nodeMap) - (getNodePosition idxA nodeMap)

data Node = Node
  { position :: V2 Float,
    velocity :: V2 Float,
    acceleration :: V2 Float,
    mass :: Float,
    state :: NodeState
  }
  deriving (Show)

data NodeState = Fixed | Free
  deriving (Show)

mkNode :: V2 Float -> Node
mkNode pos =
  Node
    { position = pos,
      velocity = (V2 0 0),
      acceleration = (V2 0 0),
      mass = 10,
      state = Free
    }

getNodePosition :: Int -> Map Int Node -> V2 Float
getNodePosition nodeIndex nodes = case Map.lookup nodeIndex nodes of
  Nothing -> error "Node index does not exist"
  Just node -> node.position

vectorLength :: V2 Float -> Float
vectorLength (V2 x y) = sqrt (x * x + y * y)

-- Physics

gravity :: V2 Float
gravity = (V2 0 -9.81)

applyPhysics :: Float -> World -> World
applyPhysics dt world =
  world
    & updatePosition dt world
    & updateVelocity dt world
    & updateAcceleration dt world

class UpdatePosition a where
  updatePosition :: Float -> World -> a -> a

instance UpdatePosition Node where
  updatePosition dt _ node = case node.state of
    Fixed -> node
    Free -> node {position = node.position + (dt *^ node.velocity)}

instance UpdatePosition (Map Int Node) where
  updatePosition dt world nodes = fmap (updatePosition dt world) nodes

instance UpdatePosition Edge where
  updatePosition _dt world edge = edge {currentLength = len}
    where
      (idxA, idxB) = edge.nodeIndices
      len = vectorLength $ getNodePosition idxB world.nodes - getNodePosition idxA world.nodes

instance UpdatePosition [Edge] where
  updatePosition dt world edges = fmap (updatePosition dt world) edges

instance UpdatePosition World where
  updatePosition dt world newWorld =
    newWorld
      { edges = updatePosition dt world newWorld.edges,
        nodes = updatePosition dt world newWorld.nodes
      }

class UpdateVelocity a where
  updateVelocity :: Float -> World -> a -> a

instance UpdateVelocity Node where
  updateVelocity dt _world node = case node.state of
    Fixed -> node
    Free -> node {velocity = node.velocity + (dt *^ node.acceleration)}

instance UpdateVelocity (Map Int Node) where
  updateVelocity dt world nodes = fmap (updateVelocity dt world) nodes

instance UpdateVelocity World where
  updateVelocity dt world newWorld =
    newWorld
      { nodes = updateVelocity dt world newWorld.nodes
      }

updateAcceleration :: Float -> World -> World -> World
updateAcceleration _dt world newWorld =
  newWorld
    { nodes = newNodes
    }
  where
    nodesWithResetAccel =
      fmap
        ( \node -> case node.state of
            Fixed -> node
            Free -> node {acceleration = gravity}
        )
        newWorld.nodes
    newNodes = foldl' f nodesWithResetAccel world.edges

    f :: Map Int Node -> Edge -> Map Int Node
    f ns edge =
      updatedNodes
      where
        (idxA, idxB) = edge.nodeIndices
        Just nodeA = Map.lookup idxA ns
        Just nodeB = Map.lookup idxB ns
        edgeVector@(V2 ex ey) = normalize $ nodeB.position - nodeA.position
        elasticForce = edge.elasticityCoef * (edge.currentLength - edge.initialLength) *^ edgeVector
        dampingForce = if vbaLen < precision then (V2 0 0) else (-edge.dampingCoef) *^ proj
          where
            precision = 0.0000001
            vba@(V2 vx vy) = nodeB.velocity - nodeA.velocity -- velocity of B relative to A
            vbaLen = vectorLength vba
            proj = ((vx * ex + vy * ey) / (vbaLen * vbaLen)) *^ edgeVector -- projection of vba into the edge normalized vector

        updatedNodes =
          ns
            & Map.adjust
              ( \n -> case n.state of
                  Fixed -> n
                  Free -> n {acceleration = n.acceleration + ((1 / n.mass) *^ (elasticForce + dampingForce))}
              )
              idxA
            & Map.adjust
              ( \n -> case n.state of
                  Fixed -> n
                  Free -> n {acceleration = n.acceleration - ((1 / n.mass) *^ elasticForce + dampingForce)}
              )
              idxB

-- Initial state

initWorld :: World
initWorld =
  Bridge
    { edges =
        [mkEdge i (i + 1) | i <- [0 .. 5] <> [7 .. 11]]
          <> [mkEdge i (i + 7) | i <- [0 .. 5]]
          <> [mkEdge (i + 7) (i + 1) | i <- [0 .. 5]],
      nodes = nodeMap
    }

nodeMap :: Map Int Node
nodeMap =
  Map.fromList $
    zip
      [0 ..]
      [ (mkNode (V2 -300 0)) {state = Fixed},
        mkNode (V2 -200 0),
        mkNode (V2 -100 0),
        mkNode (V2 0 0),
        mkNode (V2 100 0),
        mkNode (V2 200 0),
        (mkNode (V2 300 0)) {state = Fixed},
        mkNode (V2 -250 100),
        mkNode (V2 -150 100),
        mkNode (V2 -50 100),
        mkNode (V2 50 100),
        mkNode (V2 150 100),
        mkNode (V2 250 100)
      ]
-- initWorld :: World
-- initWorld =
--   Bridge
--     { edges = [mkEdge 0 1],
--       nodes = nodeMap
--     }

-- nodeMap :: Map Int Node
-- nodeMap =
--   Map.fromList $
--     zip
--       [0 ..]
--       [ (mkNode (V2 0 0)) {state = Fixed},
--         mkNode (V2 50 -100)
--       ]
