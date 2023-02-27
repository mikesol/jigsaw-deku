module Main where

import Prelude

import Control.Monad.Writer (lift, runWriterT, tell)
import Data.Array ((..))
import Data.Array (intercalate) as Array
import Data.Either (Either(..))
import Data.Filterable (compact, filter)
import Data.Foldable (for_, oneOfMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (<:=>))
import Deku.Core (dyn)
import Deku.DOM.Attr.Fill (Fill(Fill)) as D
import Deku.DOM.Attr.Height (Height(Height)) as D
import Deku.DOM.Attr.Href (Href(Href)) as D
import Deku.DOM.Attr.Id (Id(Id)) as D
import Deku.DOM.Attr.OnMousedown (OnMousedown(OnMousedown)) as D
import Deku.DOM.Attr.OnMouseup (OnMouseup(OnMouseup)) as D
import Deku.DOM.Attr.Stroke (Stroke(Stroke)) as D
import Deku.DOM.Attr.StrokeWidth (StrokeWidth(StrokeWidth)) as D
import Deku.DOM.Attr.ViewBox (ViewBox(ViewBox)) as D
import Deku.DOM.Attr.Width (Width(Width)) as D
import Deku.DOM.Attr.X (X(X)) as D
import Deku.DOM.Attr.Y (Y(Y)) as D
import Deku.DOM.Elt.Defs (defs_)
import Deku.DOM.Elt.Image (image)
import Deku.DOM.Elt.Pattern (pattern)
import Deku.DOM.Elt.Rect (rect)
import Deku.DOM.Elt.Svg (svg)
import Deku.Do as Deku
import Deku.Hooks (useDyn_, useMailboxed, useMemoized, useState)
import Deku.Listeners (click_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Event (Event, fold, keepLatest)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Keyboard as Keyboard
import FRP.Event.Mouse (getMouse, withPosition)
import QualifiedDo.Alt as Alt

type Piece = Tuple Int Int
type Pieces = Array Piece

width :: Int
width = 3840

height :: Int
height = 2160

pieces_wide :: Int
pieces_wide = 16

pieces_high :: Int
pieces_high = 9

piece_width :: Int
piece_width = width / pieces_wide

piece_height :: Int
piece_height = height / pieces_high

pieces :: Pieces
pieces = Tuple <$> 0 .. (pieces_wide - 1) <*> 0 .. (pieces_high - 1)

asDiff :: Event { x :: Int, y :: Int } -> Event { x :: Int, y :: Int }
asDiff = map snd <<< compact <<< fold f Nothing
  where
  f (Just (p0 /\ _)) p1 = Just (p0 /\ { x: p1.x - p0.x, y: p1.y - p0.y })
  f Nothing p1 = Just (p1 /\ { x: 0, y: 0 })

main :: Effect Unit
main = do
  m <- getMouse
  piece_positions' <- Map.fromFoldable <$> for pieces \(x /\ y) -> do
    x' <- randomInt 0 width <#> (_ - (x * piece_width))
    y' <- randomInt 0 height <#> (_ - (y * piece_height))
    pure $ (x /\ y) /\ (x' /\ y')
  runInBody Deku.do
    shift_pressed <- useMemoized Alt.do
      pure false
      filter (_ == "ShiftLeft") Keyboard.up $> false
      filter (_ == "ShiftLeft") Keyboard.down $> true
    animated <- useMemoized $ compact (_.pos <$> withPosition m animationFrame)
    set_driver /\ driver <- useMailboxed
    set_moved_piece /\ moved_piece <- useMailboxed
    set_currently_selected /\ currently_selected <- useState Set.empty
    set_piece_positions /\ piece_positions <- useState piece_positions'
    svg
      Alt.do
        D.Width !:= show (width * 2)
        D.Height !:= show (height * 2)
        click_ (set_currently_selected Set.empty)
      [ defs_ $
          pieces <#> \(x /\ y) -> pattern
            Alt.do
              D.Id !:= "background-" <> show x <> "-" <> show y
              D.Width !:= "100%"
              D.Height !:= "100%"
              D.ViewBox !:=
                ( [ 0, 0, piece_width, piece_height ]
                    <#> show
                    # Array.intercalate " "
                )
            [ image
                Alt.do
                  D.Href !:= "ship-1366926_crop_4k.png"
                  D.X !:= show (-x * piece_width)
                  D.Y !:= show (-y * piece_height)
                  D.Width !:= show width
                  D.Height !:= show height
                []
            ]
      , dyn
          $
            ( oneOfMap pure
                (Map.toUnfoldable piece_positions' :: Array _)
            ) <#> \((x /\ y) /\ (x' /\ y')) -> Deku.do
              { sendTo } <- useDyn_
              rect
                Alt.do
                  D.X <:=> Alt.do
                    pure $ show x'
                    keepLatest $ moved_piece (x /\ y) <#> case _ of
                      Left (x'' /\ _) -> show <$> ((_.x <$> asDiff animated) + pure x'')
                      Right (x'' /\ _) -> pure $ show x''
                  D.Y <:=> Alt.do
                    pure $ show y'
                    keepLatest $ moved_piece (x /\ y) <#> case _ of
                      Left (_ /\ y'') -> show <$> ((_.y <$> asDiff animated) + pure y'')
                      Right (_ /\ y'') -> pure $ show y''
                  D.Width !:= (show $ piece_width)
                  D.Height !:= (show $ piece_height)
                  D.Fill !:= "url(#background-" <> show x <> "-" <> show y <> ")"
                  D.Stroke <:=>
                    ( currently_selected <#>
                        Set.member (x /\ y) >>> if _ then "orange" else "none"
                    )
                  D.StrokeWidth !:= "5"
                  D.OnMousedown <:=>
                    ( { is_shift_pressed: _
                      , active_pieces: _
                      , all_positions: _
                      }
                        <$> shift_pressed
                        <*> currently_selected
                        <*> piece_positions
                        <#> \{ is_shift_pressed, active_pieces, all_positions } -> do
                          set_driver { address: x /\ y, payload: true }
                          sendTo (pieces_wide * pieces_high)
                          let
                            selection =
                              if Set.member (x /\ y) active_pieces then active_pieces
                              else Set.singleton (x /\ y)
                                # if is_shift_pressed then Set.union active_pieces else identity
                          set_currently_selected selection
                          for_ selection \(a /\ b) -> do
                            let old_pos = Map.lookup (a /\ b) all_positions
                            for_ old_pos \(x'' /\ y'') -> do
                              set_moved_piece { address: a /\ b, payload: Left (x'' /\ y'') }
                    )
                  D.OnMouseup <:=>
                    ( keepLatest $
                        { is_driver: _
                        , active_pieces: _
                        , all_positions: _
                        } <$> (driver $ x /\ y) <*> currently_selected <*> piece_positions <#> \{ is_driver, active_pieces, all_positions } ->
                          guard is_driver $ asDiff animated <#> \p -> do
                            _ /\ new_vals <- runWriterT do
                              for_ active_pieces \(a /\ b) -> do
                                let old_pos = Map.lookup (a /\ b) all_positions
                                for_ old_pos \(x'' /\ y'') -> do
                                  let new_pos = (x'' + p.x) /\ (y'' + p.y)
                                  lift $ set_moved_piece { address: a /\ b, payload: Right new_pos }
                                  tell $ [ (a /\ b) /\ new_pos ]
                            set_piece_positions $ Map.union (Map.fromFoldable new_vals) all_positions
                            set_driver { address: x /\ y, payload: false }
                    )
                []
      ]
