module Main where

import Prelude

import ChocoPie (runChocoPie)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Event (Event)
import FRP.Event as Event
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name

unName :: Name -> String
unName (Name name) = name

newtype DateString = DateString String

unDate :: DateString -> String
unDate (DateString string) = string

someDate :: DateString
someDate = DateString "2019-03-09"

type File =
  { name :: Name
  , watched :: Maybe DateString
  }

type State =
  -- files
  { files :: Array File
  -- counted by positions from the top
  , cursor :: Maybe Int
  -- TODO: filtering, sorting has turned out to be useless?
  , filterString :: String
  , toggled :: Boolean
  }

-- TODO: add queries i need
data Query a
  = Toggle a
  | ToggleWatched Name a
  | EEQuery ExternalEvent a

initialState :: State
initialState =
  { files:
      [ {name:Name"[MyGroup] Some Typical Garbage Show - 09.mkv",watched:Just someDate}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 08.mkv",watched:Just someDate}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 07.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 06.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 05.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 04.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 03.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 02.mkv",watched:Nothing}
      , {name:Name"[MyGroup] Some Typical Garbage Show - 01.mkv",watched:Nothing}
      ]
  , cursor: Nothing
  , filterString: ""
  , toggled: false
  }

render :: State -> H.ComponentHTML Query
render state =
  HH.div []
    [ HH.h1_ [HH.text "VT"]
    , header
    , HH.div_ $ Array.mapWithIndex mkFile files
    ]
  where
    label = if state.toggled then "On" else "Off"

    header = HH.div
      [ HP.class_ $ HH.ClassName "header" ]
      [ HH.div [ HP.class_ $ HH.ClassName "filters" ] []
      , HH.div [ HP.class_ $ HH.ClassName "recents" ] $
          [ HH.h3_ [ HH.text "Recently watched:" ]
          ] <> recents
      ]

    recents = mkRecent <$> Array.take 5 files

    mkRecent file = HH.div
      [ HP.class_ $ HH.ClassName "recent" ]
      [ HH.text $ unName file.name ]

    -- TODO: filtering, maybe sorting
    files = state.files

    mkFile idx file = HH.div
      [ HP.classes
          [ HH.ClassName "file"
          , HH.ClassName case state.cursor of
              Just pos | pos == idx -> "cursor"
              _ -> ""
          ]
      ]
      [ HH.div
          [ HP.class_ $ HH.ClassName "icon"
          ] []
      , mkFileCell "name" $ HH.text $ unName file.name
      , mkFileCell "watched" $ HH.text $ maybe ""
          (\(DateString date) -> "watched " <> date)
          file.watched
      ]

    mkFileCell className e = HH.div
      [HP.class_ $ HH.ClassName className]
      [e]

eval :: Query ~> H.ComponentDSL State Query Void Aff
eval (Toggle next) = do
  state <- H.get
  let nextState = state { toggled = not state.toggled }
  H.put nextState
  pure next

eval (ToggleWatched name next) = do
  H.modify_ updateFiles
  pure next
  where
    updateFiles s = s { files = update <$> s.files }
    update file
      | file.name == name
      , watched <- maybe (Just someDate) (const Nothing) file.watched
      = file { watched = watched }
      | otherwise = file

eval (EEQuery (DirectionEvent dir) next) = do
  case dir of
    Down -> H.modify_ \s -> do
      let cursor = min (Array.length s.files) (maybe (-1) (_ + 1) s.cursor)
      s { cursor = Just cursor }
    Up -> H.modify_ \s -> do
      let cursor = max 0 (maybe 0 (_ - 1) s.cursor)
      s { cursor = Just cursor }
  pure next

eval (EEQuery OpenEvent next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> do
      Console.log $ "we should be opening this file: " <> unName file.name
      pure next
    _ -> pure next

eval (EEQuery MarkEvent next) = do
  s <- H.get
  case s.cursor of
    Just pos
      | Just file <- Array.index s.files pos
      -> eval (ToggleWatched file.name next)
    _ -> pure next

eval (EEQuery FocusFilterEvent next) = do
  Console.log "FocusFilterEvent"
  pure next

eval (EEQuery RefreshEvent next) = do
  Console.log "RefreshEvent"
  pure next

eval (EEQuery FetchIconsEvent next) = do
  Console.log "FetchIconsEvent"
  pure next

myButton :: H.Component HH.HTML Query Unit Void Aff
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
data ExternalEvent
  -- move cursor on j/k
  = DirectionEvent Direction
  -- open file on o
  | OpenEvent
  -- mark file watched on m
  | MarkEvent
  -- call refresh files on r
  | RefreshEvent
  -- focus on filter on s
  | FocusFilterEvent
  -- call fetch icons on I (shift + i)
  | FetchIconsEvent

data Direction = Up | Down

keyboard :: Event Unit -> Effect (Event ExternalEvent)
keyboard _ = do
  {event, push} <- Event.create
  addWindowKeyListener \key ->
    case key of
      "o" -> push OpenEvent
      "k" -> push $ DirectionEvent Up
      "j" -> push $ DirectionEvent Down
      "w" -> push MarkEvent
      "m" -> push MarkEvent
      "r" -> push RefreshEvent
      "s" -> push FocusFilterEvent
      "I" -> push FetchIconsEvent
      _ -> pure unit
  pure event

halogen :: Event ExternalEvent -> Effect (Event Unit)
halogen externalEvents = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI myButton unit body
    _ <- liftEffect $ Event.subscribe externalEvents \ee -> do
      launchAff_ $ io.query $ H.action $ EEQuery ee
      pure $ mempty :: Event Unit
    pure unit
  mempty

main :: Effect Unit
main = do
  liftEffect $ runChocoPie mkSink drivers
  Console.log "Started application"
  where
    mkSink sources =
      { keyboard: mempty :: Event Unit
      , halogen: sources.keyboard
      }
    drivers = {keyboard, halogen}

foreign import addWindowKeyListener :: (String -> Effect Unit) -> Effect Unit
