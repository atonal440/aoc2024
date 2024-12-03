module Days (dispatch) where

import Lib ( Dispatch )
import qualified Days.D01
import qualified Days.D02
import qualified Days.D03
import qualified Days.D04
import qualified Days.D05
import qualified Days.D06
import qualified Days.D07
import qualified Days.D08
import qualified Days.D09
import qualified Days.D10
import qualified Days.D11
import qualified Days.D12
import qualified Days.D13
import qualified Days.D14
import qualified Days.D15
import qualified Days.D16
import qualified Days.D17
import qualified Days.D18
import qualified Days.D19
import qualified Days.D20
import qualified Days.D21
import qualified Days.D22
import qualified Days.D23
import qualified Days.D24
import qualified Days.D25

dispatch :: Int -> Dispatch
dispatch = \case
  1   -> Days.D01.dispatch
  2   -> Days.D02.dispatch
  3   -> Days.D03.dispatch
  4   -> Days.D04.dispatch
  5   -> Days.D05.dispatch
  6   -> Days.D06.dispatch
  7   -> Days.D07.dispatch
  8   -> Days.D08.dispatch
  9   -> Days.D09.dispatch
  10  -> Days.D10.dispatch
  11  -> Days.D11.dispatch
  12  -> Days.D12.dispatch
  13  -> Days.D13.dispatch
  14  -> Days.D14.dispatch
  15  -> Days.D15.dispatch
  16  -> Days.D16.dispatch
  17  -> Days.D17.dispatch
  18  -> Days.D18.dispatch
  19  -> Days.D19.dispatch
  20  -> Days.D20.dispatch
  21  -> Days.D21.dispatch
  22  -> Days.D22.dispatch
  23  -> Days.D23.dispatch
  24  -> Days.D24.dispatch
  25  -> Days.D25.dispatch
  unk -> error $ "unknown day " <> show unk
