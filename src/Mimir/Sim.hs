{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mimir.Sim(
    module Mimir.Std,
    Sim,
    createSim,
    destroySim
) where

import Mimir.Types
import Mimir.Std
import Mimir.Sim.Types

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (Lens, over, set, view, (&), (.~), (%~))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either (left)
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)
import Data.List (partition, sortOn)
import Data.Time.Clock.POSIX (getPOSIXTime)

---
--- Create a Sim
---

createSim :: (Exchange e, Monad (ExchangeM e), OrderBookP e, TradeHistoryP e, Iso OrderBook (OrderBookT e), Iso Trade (TradeT e)) => Int -> Double -> Double -> e -> IO (Sim e)
createSim cycleDelayMS currencyBalance commodityBalance e = do
    tmeMS <- fmap (round . (* 1000)) $ getPOSIXTime
    let stat = SimState {
        _ssIDGen = div tmeMS 1000,
        _ssUpdatedUTCMS = tmeMS,
        _ssCurrencyBalance = currencyBalance,
        _ssCommodityBalance = commodityBalance,
        _ssPendingLimitOrders = [],
        _ssPendingMarketOrders = []
    }
    tstat <- atomically $ newTVar stat
    tid <- forkIO $ runSim cycleDelayMS tstat e
    return $ Sim {
        _siExchange = e,
        _siManagerThreadID = tid,
        _siState = tstat
    }

---
--- Destroy a Sim
---

destroySim :: Sim e -> IO ()
destroySim = killThread . view siManagerThreadID

---
--- Run a Sim
---

runSim :: (Exchange e, Monad (ExchangeM e), OrderBookP e, TradeHistoryP e, Iso OrderBook (OrderBookT e), Iso Trade (TradeT e)) => Int -> TVar SimState -> e -> IO ()
runSim cycleDelayMS tstat e = forever $ do
    stat <- atomically $ readTVar tstat
    let plos = _ssPendingLimitOrders stat
    let pmos = _ssPendingMarketOrders stat
    when (length pmos + length plos > 0) $ simCycle tstat e
    tmeMS <- fmap (round . (* 1000)) $ getPOSIXTime
    atomically $ modifyTVar tstat (set ssUpdatedUTCMS tmeMS)
    threadDelay $ cycleDelayMS * 1000

simCycle :: (Exchange e, Monad (ExchangeM e), OrderBookP e, TradeHistoryP e, Iso OrderBook (OrderBookT e), Iso Trade (TradeT e)) => TVar SimState -> e -> IO ()
simCycle tstat e = do
    res <- reifyIO (orderBook' e >>= \ob -> tradeHistory' e >>= \tx -> return (isoG ob, fmap isoG tx)) e
    case res of
        Left _ -> return () --TODO: throw?
        Right (ob, tx) -> atomically $ modifyTVar tstat (matchOrders ob tx)

matchOrders ::  OrderBook -> [Trade] -> SimState -> SimState
matchOrders ob rtx stat = flip (foldl (satisfyPLO ob')) plos $ foldl (satisfyPMO ob') stat pmos
    where
    plos = reverse . sortOn (view ploID) $ _ssPendingLimitOrders stat
    pmos = reverse . sortOn (view pmoID) $ _ssPendingMarketOrders stat
    updatedMS = _ssUpdatedUTCMS stat
    updated = max updatedMS $ min (earliestPLO updatedMS plos) (earliestPMO updatedMS pmos)
    ob' = appendRecentTrades ob rtx updated

earliestPLO :: Int -> [PendingLimitOrder] -> Int
earliestPLO def plos = case (fmap _ploTimeUTCMS plos) of
    [] -> def
    tx -> minimum tx

earliestPMO :: Int -> [PendingMarketOrder] -> Int
earliestPMO def pmos = case (fmap _pmoTimeUTCMS pmos) of
    [] -> def
    tx -> minimum tx

appendRecentTrades :: OrderBook -> [Trade] -> Int -> OrderBook
appendRecentTrades ob rtx updated = ob & obAsks %~ (++asks) & obBids %~ (++bids)
    where
    (tbids, tasks) = partition ((==BID) . _trType) $ filter ((>updated) . _trTimeUTCMS)  rtx
    bids = fmap tradeToEntry tbids
    asks = fmap tradeToEntry tasks

tradeToEntry :: Trade -> OrderBookEntry
tradeToEntry t = OrderBookEntry vol price
    where
    vol = _trVolume t
    price = _trUnitPrice t

satisfyPLO :: OrderBook -> SimState -> PendingLimitOrder -> SimState
satisfyPLO ob stat plo
    | typ == BID = satisfyLimitBuy ob plo stat
    | typ == ASK = satisfyLimitSell ob plo stat
    where typ = _ploType plo

satisfyLimitBuy :: OrderBook -> PendingLimitOrder -> SimState -> SimState
satisfyLimitBuy ob plo stat = case buyPrice vol ob of
    Nothing -> stat
    Just price -> case (price <= maxCost) of
        True -> stat & ssPendingLimitOrders %~ stripOrder & ssCurrencyBalance %~ (+ (maxCost - price)) & ssCommodityBalance %~ (+ vol)
        False -> stat
    where
    oid = _ploID plo
    vol = _ploVolume plo
    unitPrice = _ploUnitPrice plo
    maxCost = vol * unitPrice
    stripOrder = (filter $ (/=oid) . view ploID)

satisfyLimitSell :: OrderBook -> PendingLimitOrder -> SimState -> SimState
satisfyLimitSell ob plo stat = case sellPrice vol ob of
    Nothing -> stat
    Just price -> case (price >= minPrice) of
        True -> stat & ssPendingLimitOrders %~ stripOrder & ssCurrencyBalance %~ (+ price)
        False -> stat
    where
    oid = _ploID plo
    vol = _ploVolume plo
    unitPrice = _ploUnitPrice plo
    minPrice = vol * unitPrice
    stripOrder = (filter $ (/=oid) . _ploID)

satisfyPMO :: OrderBook -> SimState -> PendingMarketOrder -> SimState
satisfyPMO ob stat pmo
    | typ == BID = satisfyMarketBuy ob pmo stat
    | typ == ASK = satisfyMarketSell ob pmo stat
    where typ = _pmoType pmo

satisfyMarketBuy :: OrderBook -> PendingMarketOrder -> SimState -> SimState
satisfyMarketBuy ob pmo stat = case buyVolume amount ob of
    Nothing -> stat' & ssCurrencyBalance %~ (+ amount)
    Just vol -> stat' & ssCommodityBalance %~ (+ vol)
    where
    oid = _pmoID pmo
    amount = _pmoAmount pmo
    stripOrder = (filter $ (/=oid) . view pmoID)
    stat' = stat & ssPendingMarketOrders %~ stripOrder

satisfyMarketSell :: OrderBook -> PendingMarketOrder -> SimState -> SimState
satisfyMarketSell ob pmo stat = case sellPrice amount ob of
    Nothing -> stat' & ssCommodityBalance %~ (+ amount)
    Just price -> stat' & ssCurrencyBalance %~ (+ price)
    where
    oid = _pmoID pmo
    amount = _pmoAmount pmo
    stripOrder = (filter $ (/=oid) . view pmoID)
    stat' = stat & ssPendingMarketOrders %~ stripOrder

buyPrice :: Double -> OrderBook -> Maybe Double
buyPrice vol = priceFor vol 0 . sortOn _oePrice . view obAsks

buyVolume :: Double -> OrderBook -> Maybe Double
buyVolume amount = buyVolume' amount 0 . sortOn _oePrice . view obAsks

buyVolume' :: Double -> Double -> [OrderBookEntry] -> Maybe Double
buyVolume' amount vol ex
    | amount <= 0 = Just vol
    | otherwise = case ex of
        [] -> Nothing
        (e:t) -> buyVolume' (amount - paid) (vol + received) t
            where
            entryVal = _oePrice e * _oeVolume e
            paid = min amount entryVal
            received = paid / (_oePrice e)

sellPrice :: Double -> OrderBook -> Maybe Double
sellPrice vol = priceFor vol 0 . reverse . sortOn _oePrice . view obBids

priceFor :: Double -> Double -> [OrderBookEntry] -> Maybe Double
priceFor vol price ex
    | vol <= 0 = Just price
    | otherwise = case ex of
        [] -> Nothing
        (e:t) -> priceFor (vol - traded) (price + cost) t
            where
            traded = min vol (view oeVolume e)
            cost = _oePrice e * traded



---
--- Utility
---

runExchange :: (Exchange e, Iso StdErr (ErrorT e)) => (e -> ExchangeM e a) -> Sim e -> StdM (Sim e) a
runExchange f sim = do
    let ex = view siExchange sim
    res <- liftIO $ reifyIO (f ex) ex
    case res of
        Left e -> lift . left $ isoG e
        Right r -> return r

readState :: Exchange e => Sim e -> StdM (Sim e) SimState
readState = liftIO . atomically . readTVar . view siState

viewState :: Exchange e => Lens SimState SimState r r -> Sim e -> StdM (Sim e) r
viewState lens = liftIO . atomically . fmap (view lens) . readTVar . view siState

operateState :: Exchange e => (SimState -> (a, SimState)) -> Sim e -> StdM (Sim e) a
operateState f sim = liftIO . atomically $ do
    let tv = view siState sim
    (res, stat) <- fmap f $ readTVar tv
    writeTVar tv stat
    return res

modifyState :: Exchange e => (SimState -> SimState) -> Sim e -> StdM (Sim e) ()
modifyState f = operateState $ (\s -> ((), f s))

---
--- Exchange instance
---

instance Exchange e => Exchange (Sim e) where
    type ExchangeM (Sim e) = StdM (Sim e)
    type ErrorT (Sim e) = StdErr
    reifyIO = reifyStdM

---
--- Ticker
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TickerP e) => TickerP (Sim e) where
    type TickerT (Sim e) = TickerT e
    ticker' = runExchange ticker'

---
--- Candles
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), CandlesP e) => CandlesP (Sim e) where
    type CandleIntervalT (Sim e) = CandleIntervalT e
    type CandleT (Sim e) = CandleT e
    candles' t iv = runExchange (\e -> candles' e iv) t

---
--- OrderBook
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), OrderBookP e) => OrderBookP (Sim e) where
    type OrderBookT (Sim e) = OrderBookT e
    orderBook' = runExchange orderBook'

---
--- TradeHistory
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TradeHistoryP e) => TradeHistoryP (Sim e) where
    type TradeT (Sim e) = TradeT e
    tradeHistory' = runExchange tradeHistory'


---
--- Order
---

instance (Exchange e, Monad (ExchangeM e), OrderP e, Iso Order (OrderT e), Iso OrderType (OrderTypeT e), Iso Double (OrderAmountT e), Iso String (OrderIDT e)) => OrderP (Sim e) where
    type OrderTypeT (Sim e) = OrderTypeT e
    type OrderAmountT (Sim e) = OrderAmountT e
    type OrderT (Sim e) = OrderT e
    type OrderIDT (Sim e) = OrderIDT e
    currentOrders' sim = do
        stat <- readState sim
        let plos = view ssPendingLimitOrders stat
        return . fmap isoF $ fmap toOrder plos
    placeLimitOrder' sim etyp evol eprice = do
        let typ = isoG etyp
        let vol = isoG evol
        let price = isoG eprice
        nid <- operateState newID sim
        tmeMS <- liftIO $ fmap (round . (* 1000)) getPOSIXTime
        let plo = mkPLO typ nid vol price tmeMS
        ok <- operateState (addPLO plo) sim
        case ok of
            True ->  return $ isoF nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    placeMarketOrder' sim etyp eamount = do
        let typ = isoG etyp
        let amount = isoG eamount
        nid <- operateState newID sim
        tmeMS <- liftIO $ fmap (round . (* 1000)) getPOSIXTime
        let pmo = PendingMarketOrder typ nid tmeMS amount
        ok <- operateState (addPMO pmo) sim
        case ok of
            True ->  return $ isoF nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    cancelOrder' sim oid = modifyState (cancelO $ isoG oid) sim

addPLO :: PendingLimitOrder -> SimState -> (Bool, SimState)
addPLO plo stat
    | bal >= amount = (True, stat & ssPendingLimitOrders %~ (++[plo]) & blens %~ (\b -> b - amount))
    | otherwise = (False, stat)
    where
    typ = view ploType plo
    blens = case typ of
        BID -> ssCurrencyBalance
        ASK -> ssCommodityBalance
    amount = case typ of
        BID -> _ploVolume plo * _ploUnitPrice plo
        ASK -> _ploVolume plo
    bal = case typ of
        BID -> _ssCurrencyBalance stat
        ASK -> _ssCommodityBalance stat

addPMO :: PendingMarketOrder -> SimState -> (Bool, SimState)
addPMO pmo stat
    | bal >= amount = (True, stat & ssPendingMarketOrders %~ (++[pmo]) & blens %~ (\b -> b - amount))
    | otherwise = (False, stat)
    where
    typ = view pmoType pmo
    blens = case typ of
        BID -> ssCurrencyBalance
        ASK -> ssCommodityBalance
    amount = view pmoAmount pmo
    bal = case typ of
        BID -> _ssCurrencyBalance stat
        ASK -> _ssCommodityBalance stat

toOrder :: PendingLimitOrder -> Order
toOrder plo =
    Order {
        _oType = _ploType plo,
        _oID = _ploID plo,
        _oTimeUTCMS = _ploTimeUTCMS plo,
        _oVolume = _ploVolume plo,
        _oUnitPrice = _ploUnitPrice plo
    }

mkPLO :: OrderType -> String -> Double -> Double -> Int -> PendingLimitOrder
mkPLO typ oid vol price tmeMS =
    PendingLimitOrder {
        _ploType = typ,
        _ploID = oid,
        _ploTimeUTCMS = tmeMS,
        _ploVolume = vol,
        _ploUnitPrice = price
    }

cancelO :: String -> SimState -> SimState
cancelO oid stat
    | orderExists = stat & ssPendingLimitOrders %~ (filter ((/=oid) . view ploID)) & blens %~ (+ amount)
    | otherwise = stat
    where
    plo = listToMaybe . filter ((==oid) . view ploID) $ view ssPendingLimitOrders stat
    orderExists = isJust plo
    blens = case (fmap _ploType plo) of
        Nothing -> undefined
        (Just BID) -> ssCurrencyBalance
        (Just ASK) -> ssCommodityBalance
    amount = case plo of
        Nothing -> 0
        (Just o) -> case _ploType o of
            BID -> _ploVolume o * _ploUnitPrice o
            ASK -> _ploVolume o

newID :: SimState -> (String, SimState)
newID stat = (show nid, set ssIDGen nid stat)
    where nid = view ssIDGen stat + 1

---
--- Balances
---

instance (Exchange e, Monad (ExchangeM e), BalancesP e, Iso Balances (BalancesT e)) => BalancesP (Sim e) where
    type BalancesT (Sim e) = BalancesT e
    balances' sim = do
        stat <- readState sim
        let cur = _ssCurrencyBalance stat
        let com = _ssCommodityBalance stat
        return . isoF $ Balances cur com
