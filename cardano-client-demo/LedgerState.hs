{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

import           Cardano.Api
import           Cardano.Slotting.Slot (WithOrigin (At, Origin))
import           Control.Monad (when)
import           Control.Monad.Trans.Except
import           Data.Foldable
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Word
import           Network.TypedProtocol.Pipelined (Nat (..))
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                   (ChainSyncClientPipelined (ChainSyncClientPipelined),
                   ClientPipelinedStIdle (CollectResponse, SendMsgDone, SendMsgRequestNextPipelined),
                   ClientStNext (..))
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
import           System.Environment (getArgs)

import qualified Shelley.Spec.Ledger.TxBody as L
import qualified Shelley.Spec.Ledger.RewardUpdate as L
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Shelley.Spec.Ledger.API as L
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Map.Strict as Map

data State = State { lastCheckpoint    :: Word64
                   , lastRewStartEpoch :: Word64
                   , lastRewEndEpoch   :: Word64
                   }

main :: IO ()
main = do
  -- Get socket path from CLI argument.
  configFilePath : socketPath : target : checkpointSize : _ <- getArgs
  -- $ cabal exec ledger-state configuration/cardano/shelley_qa-config.json state-node-shelley_qa/node.socket 8190331207ecedfcaf448340d8bb84354a0e8c285982a2d7f98bb234 500000
  lastCheckpoint <- fmap (either (error . T.unpack . renderFoldBlocksError) id) $ runExceptT $ foldBlocks
    configFilePath
    socketPath
    (Testnet $ NetworkMagic 3)
    True -- enable validation?
    State {lastCheckpoint = 0, lastRewStartEpoch = 0, lastRewEndEpoch = 0}
    (\_env
      !ledgerState
      (BlockInMode (Block (BlockHeader (SlotNo slotNo) _blockHeaderHash (BlockNo _blockNoI)) transactions) _era)
      state -> do
        let go = L.unStake . L._stake . L._pstakeGo . L.esSnapshots . L.nesEs
        let cps = read checkpointSize
        let (name, info) =
              case ledgerState of
                LedgerStateByron _                                     ->
                  ("byron", Nothing)
                LedgerStateShelley (Shelley.ShelleyLedgerState _ ls _) ->
                  ( "shelley", Just (L.nesEL ls, L.nesRu ls, go ls))
                LedgerStateAllegra (Shelley.ShelleyLedgerState _ ls _) ->
                  ( "allegra", Just (L.nesEL ls, L.nesRu ls, go ls))
                LedgerStateMary    (Shelley.ShelleyLedgerState _ ls _) ->
                  ( "mary",    Just (L.nesEL ls, L.nesRu ls, go ls))

        displayCheckpoint name slotNo (lastCheckpoint state) cps
        mapM_ (displayDeleg target slotNo . getTxBody) transactions

        let lc = if newCheckpoint slotNo (lastCheckpoint state) cps then slotNo else (lastCheckpoint state)
        es <- case info of
                Just (ep, L.SJust _, _) ->
                  dispRewardStart (lastRewStartEpoch state) ep slotNo
                _ -> return (lastRewStartEpoch state)
        ee <- case info of
                Just (ep, L.SJust (L.Complete ru), goSnap) ->
                  dispReward (lastRewEndEpoch state) ep slotNo (L.rs ru) goSnap target
                _ -> return (lastRewEndEpoch state)

        return State {lastCheckpoint = lc, lastRewStartEpoch = es, lastRewEndEpoch = ee}
    )

  return ()
  where
    log :: Show a => String -> a -> IO ()
    log title a = putStrLn $ title <> " " <> show a

    newCheckpoint s lc cps = s - lc >= cps
    displayCheckpoint era s lc cps = when (newCheckpoint s lc cps) (log ("CHECKPOINT-" <> era) s)

    displayDeleg :: String -> Word64 -> TxBody era -> IO ()
    displayDeleg _ _ (ByronTxBody _                               ) = return ()
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraShelley txb _ _) = displayFiltered t s $ L._certs txb
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraAllegra txb _ _) = displayFiltered t s $ MA.certs' txb
    displayDeleg t s (ShelleyTxBody ShelleyBasedEraMary    txb _ _) = displayFiltered t s $ MA.certs' txb

    dispCert s c = log "DELEG" (s, c)
    displayFiltered t s cs = mapM_ (dispCert s) $ filter (onlyTarget t) (toList cs)

    isTarget t (L.KeyHashObj    (L.KeyHash    kh)) = (tail . init . show) kh == t
    isTarget t (L.ScriptHashObj (L.ScriptHash sh)) = (tail . init . show) sh == t

    onlyTarget t (L.DCertDeleg (L.RegKey cred))                    = isTarget t cred
    onlyTarget t (L.DCertDeleg (L.DeRegKey cred))                  = isTarget t cred
    onlyTarget t (L.DCertDeleg (L.Delegate (L.Delegation cred _))) = isTarget t cred
    onlyTarget t _ = False

    dispRewardStart el (EpochNo e) slot =
      if el < e
        then log "REWARD-START" (e-1, slot) >> return e
        else return el

    dispReward el (EpochNo e) slot rs ss t =
      if el < e
        then
          log "REWARD-END" (e-1, slot, Map.filterWithKey (\k _ -> isTarget t k) rs)
            >> log "STAKE" (e-1, slot, Map.filterWithKey (\k _ -> isTarget t k) ss)
            >> return e
        else return el
