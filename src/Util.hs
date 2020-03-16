
module Util where

import           Brick                      as Br
import           Control.Monad.Trans.Reader (withReaderT)
import           Lens.Micro                 ((%~), (^.))

vLimitMax :: Br.Widget n -> Br.Widget n
vLimitMax p = Br.Widget (Br.hSize p) Br.Fixed $ do
  ctx <- getContext
  let usableHeight = ctx ^. availHeightL
      widgetHeight = usableHeight
  withReaderT (Br.availHeightL %~ min widgetHeight) $ render $ Br.cropToContext p

hLimitMax :: Br.Widget n -> Br.Widget n
hLimitMax p = Br.Widget Br.Fixed (Br.vSize p) $ do
  ctx <- getContext
  let usableWidth = ctx ^. availWidthL
      widgetWidth = usableWidth
  withReaderT (Br.availWidthL %~ min widgetWidth) $ render $ Br.cropToContext p
