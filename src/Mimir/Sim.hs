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
import Data.Maybe (fromJust, isJust, listToMaybe)
import Data.List (partition, sortOn)
import Data.Time.Clock.POSIX (getPOSIXTime)

---
--- Create a Sim
---

createSim :: (Exchange e, Monad (ExchangeM e), OrderBookP e, Iso OrderBook (OrderBookT e)) => Int -> Double -> Double -> e -> IO (Sim e)
createSim cycleDelayMS currencyBalance commodityBalance e = do
    idg <- fmap round getPOSIXTime
    let stat = SimState {
        _ssIDGen = idg,
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

runSim :: (Exchange e, Monad (ExchangeM e), OrderBookP e, Iso OrderBook (OrderBookT e)) => Int -> TVar SimState -> e -> IO ()
runSim cycleDelayMS tstat e = forever $ do
    plos <- atomically . fmap (view ssPendingLimitOrders) $ readTVar tstat
    pmos <- atomically . fmap (view ssPendingMarketOrders) $ readTVar tstat
    case (length pmos + length plos == 0) of
        True -> return ()
        False -> do
            res <- reifyIO (orderBook' e) e
            case res of
                Left _ -> return () --TODO: throw?
                Right ob -> atomically $ modifyTVar tstat (matchOrders (isoG ob))
    threadDelay $ cycleDelayMS * 1000

matchOrders :: OrderBook -> SimState -> SimState
matchOrders ob stat = flip (foldl (satisfyPLO ob)) plos $ foldl (satisfyPMO ob) stat pmos
    where
    plos = reverse . sortOn (view ploID) $ view ssPendingLimitOrders stat
    pmos = reverse . sortOn (view pmoID) $ view ssPendingMarketOrders stat

satisfyPLO :: OrderBook -> SimState -> PendingLimitOrder -> SimState
satisfyPLO ob stat plo
    | typ == BID = satisfyLimitBuy ob plo stat
    | typ == ASK = satisfyLimitSell ob plo stat
    where typ = view ploType plo

satisfyLimitBuy :: OrderBook -> PendingLimitOrder -> SimState -> SimState
satisfyLimitBuy ob plo stat = case buyPrice vol ob of
    Nothing -> stat
    Just price -> case (price < maxPrice) of
        True -> stat & ssPendingLimitOrders %~ stripOrder & ssCurrencyBalance %~ (+ (available - price)) & ssCommodityBalance %~ (+ vol)
        False -> stat
    where
    oid = view ploID plo
    vol = view ploVolume plo
    maxPrice = view ploUnitPrice plo
    available = vol * maxPrice
    stripOrder = (filter $ (/=oid) . view ploID)

satisfyLimitSell :: OrderBook -> PendingLimitOrder -> SimState -> SimState
satisfyLimitSell ob plo stat = case sellPrice vol ob of
    Nothing -> stat
    Just price -> case (price > minPrice) of
        True -> stat & ssPendingLimitOrders %~ stripOrder & ssCurrencyBalance %~ (+ price)
        False -> stat
    where
    oid = view ploID plo
    vol = view ploVolume plo
    minPrice = view ploUnitPrice plo
    available = vol * minPrice
    stripOrder = (filter $ (/=oid) . view ploID)

satisfyPMO :: OrderBook -> SimState -> PendingMarketOrder -> SimState
satisfyPMO ob stat pmo
    | typ == BID = satisfyMarketBuy ob pmo stat
    | typ == ASK = satisfyMarketSell ob pmo stat
    where typ = view pmoType pmo

satisfyMarketBuy :: OrderBook -> PendingMarketOrder -> SimState -> SimState
satisfyMarketBuy ob pmo stat = case buyVolume amount ob of
    Nothing -> stat
    Just vol -> stat & ssPendingMarketOrders %~ stripOrder & ssCommodityBalance %~ (+ vol)
    where
    oid = view pmoID pmo
    amount = view pmoAmount pmo
    stripOrder = (filter $ (/=oid) . view pmoID)

satisfyMarketSell :: OrderBook -> PendingMarketOrder -> SimState -> SimState
satisfyMarketSell ob pmo stat = case sellPrice amount ob of
    Nothing -> stat
    Just price -> stat & ssPendingMarketOrders %~ stripOrder & ssCurrencyBalance %~ (+ price)
    where
    oid = view pmoID pmo
    amount = view pmoAmount pmo
    stripOrder = (filter $ (/=oid) . view pmoID)

buyPrice :: Double -> OrderBook -> Maybe Double
buyPrice vol = priceFor vol 0 . sortOn (view oePrice) . view obAsks

buyVolume :: Double -> OrderBook -> Maybe Double
buyVolume amount = buyVolume' amount 0 . sortOn (view oePrice) . view obAsks

buyVolume' :: Double -> Double -> [OrderBookEntry] -> Maybe Double
buyVolume' amount vol ex
    | amount <= 0 = Just vol
    | otherwise = case ex of
        [] -> Nothing
        (e:t) -> buyVolume' (amount - paid) (vol + received) t
            where
            entryVal = view oePrice e * view oeVolume e
            paid = min amount entryVal
            received = paid / (view oePrice e)

sellPrice :: Double -> OrderBook -> Maybe Double
sellPrice vol = priceFor vol 0 . reverse . sortOn (view oePrice) . view obBids

priceFor :: Double -> Double -> [OrderBookEntry] -> Maybe Double
priceFor vol price ex
    | vol <= 0 = Just price
    | otherwise = case ex of
        [] -> Nothing
        (e:t) -> priceFor (vol - traded) (price + cost) t
            where
            traded = min vol (view oeVolume e)
            cost = view oePrice e * traded



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

instance (Exchange e, Monad (ExchangeM e), OrderP e, Iso Order (OrderT e), Iso OrderType (OrderTypeT e), Iso Double (OrderAmountT e), Iso OrderResponse (OrderResponseT e)) => OrderP (Sim e) where
    type OrderTypeT (Sim e) = OrderTypeT e
    type OrderAmountT (Sim e) = OrderAmountT e
    type OrderT (Sim e) = OrderT e
    type OrderResponseT (Sim e) = OrderResponseT e
    currentOrders' sim = do
        stat <- readState sim
        let plos = view ssPendingLimitOrders stat
        return . fmap isoF $ fmap toOrder plos
    placeLimitOrder' sim eo = do
        let o = isoG eo
        nid <- operateState newID sim
        let plo = toPLO $ set oID nid o
        ok <- operateState (addPLO plo) sim
        case ok of
            True ->  return . isoF $ OrderResponse nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    placeMarketOrder' sim etyp eamount = do
        let typ = isoG etyp
        let amount = isoG eamount
        nid <- operateState newID sim
        let pmo = PendingMarketOrder typ nid amount
        ok <- operateState (addPMO pmo) sim
        case ok of
            True ->  return . isoF $ OrderResponse nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    cancelOrder' sim eo = modifyState (cancelO . view oID $ isoG eo) sim

addPLO :: PendingLimitOrder -> SimState -> (Bool, SimState)
addPLO plo stat
    | bal >= amount = (True, stat & ssPendingLimitOrders %~ (++[plo]) & blens %~ (\b -> b - amount))
    | otherwise = (False, stat)
    where
    typ = view ploType plo
    blens = case typ of
        BID -> ssCurrencyBalance
        ASK -> ssCommodityBalance
    amount = view ploVolume plo * view ploUnitPrice plo
    bal = case typ of
        BID -> view ssCurrencyBalance stat
        ASK -> view ssCommodityBalance stat

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
        BID -> view ssCurrencyBalance stat
        ASK -> view ssCommodityBalance stat

toOrder :: PendingLimitOrder -> Order
toOrder plo =
    Order {
        _oType = view ploType plo,
        _oID = view ploID plo,
        _oTimeUTC = fmap (flip div 1000000) $ view ploTimeUTCUS plo,
        _oExpirationTimeUTC = fmap (flip div 1000000) $ view ploExpirationTimeUTCUS plo,
        _oVolume = view ploVolume plo,
        _oUnitPrice = view ploUnitPrice plo
    }

toPLO :: Order -> PendingLimitOrder
toPLO o =
    PendingLimitOrder {
        _ploType = view oType o,
        _ploID = view oID o,
        _ploTimeUTCUS = fmap (* 1000000) $ view oTimeUTC o,
        _ploExpirationTimeUTCUS = fmap (* 1000000) $ view oExpirationTimeUTC o,
        _ploVolume = view oVolume o,
        _ploUnitPrice = view oUnitPrice o
    }

cancelO :: String -> SimState -> SimState
cancelO oid stat
    | orderExists = stat & ssPendingLimitOrders %~ (filter ((/=oid) . view ploID)) & blens %~ (+ amount)
    | otherwise = stat
    where
    plo = listToMaybe . filter ((==oid) . view ploID) $ view ssPendingLimitOrders stat
    orderExists = isJust plo
    blens = case (fmap (view ploType) plo) of
        Nothing -> undefined
        (Just BID) -> ssCurrencyBalance
        (Just ASK) -> ssCommodityBalance
    amount = case plo of
        Nothing -> 0
        Just o -> view ploVolume o * view ploUnitPrice o

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
        let cur = view ssCurrencyBalance stat
        let com = view ssCommodityBalance stat
        return . isoF $ Balances cur com
