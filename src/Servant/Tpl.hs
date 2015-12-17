{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Servant.Tpl where

import Control.Lens
import qualified Data.ByteString.Char8 as BSC
import Data.Proxy
import Data.Typeable
import GHC.TypeLits
import Servant.API
import Servant.Server
import Servant.Server.Internal
import Servant.Server.Internal.SnapShims
import qualified Snap.Snaplet.Heist as Snaplet
import Heist.Interpreted
import Snap


data HTML deriving Typeable

data KnownSymbol a => Tpl (a :: Symbol)
  deriving Typeable

instance Accept HTML where
  contentType _ = "text/html; charset=utf-8"


instance  (KnownSymbol a) => HasServer (Tpl a) where
  type ServerT (Tpl sym) (Handler b v) = (Handler b v) ()
  route Proxy rawApplication = LeafRouter $ \request respond -> do
    r <- rawApplication
    case r of
      RR (Right rawApp) -> snapToApplication'' rawApp request
                           (respond . succeedWith)
      RR (Left err)     -> respond $ failWith err


class KnownSymbol a => Describe (f :: Symbol -> *) a where
  describe :: Proxy (f sym) -> String

instance KnownSymbol sym => Describe Tpl sym where
  describe _ = symbolVal (Proxy :: Proxy sym)


servantRender :: (Snaplet.HasHeist app, KnownSymbol sym, Tpl sym ~ Tpl userSym)
              => Proxy userSym
              -> Server (Tpl userSym) (Handler app app)
servantRender p = do
  let filename = symbolVal p
  hs   <- Snaplet.getHeistState
  a    <- renderTemplate hs (BSC.pack filename)
  case a of
    Just (x,_)  -> writeBuilder x
    Nothing     -> pass


type MyTest = "template" :> Tpl "testp"

server :: Server MyTest (Handler App App)
server = servantRender (Proxy :: Proxy "testp")


myAPI :: Proxy MyTest
myAPI = Proxy

data App = App
  { _heist :: Snaplet (Snaplet.Heist App)}
makeLenses ''App

instance Snaplet.HasHeist App where heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "" Nothing $ do
  h <- nestSnaplet "heist" heist $ Snaplet.heistInit "templates"
  addRoutes [("api", server)]
  return $ App h

type AppHandler = Handler App App

test :: IO ()
test = serveSnaplet mempty appInit
