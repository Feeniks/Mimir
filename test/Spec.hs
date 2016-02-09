{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Mimir.Types
import Mimir.Std.Types
import Mimir.Sim

import Control.Concurrent
import Control.Lens.TH
import Control.Lens
import Data.Functor.Identity
import Test.HUnit

main :: IO ()
main = do
    let te = TestExchange testOrderBook
    sim <- createSim 1 105 1 te
    putStrLn "--- balances"
    testBalances sim
    putStrLn "--- done"

testBalances :: Sim TestExchange -> IO ()
testBalances sim = do
    res1 <- reifyIO balances sim
    res1 @?= Right (Balances 105 1)

    res2 <- reifyIO (placeLimitOrder BID 1 106) sim
    assertBool "expected error placing buy order of value higher than currency balance" $ isLeft res2

    res3 <- reifyIO (placeLimitOrder BID 0.5 100) sim
    assertBool "expected success placing buy order of value lower than currency balance" . not $ isLeft res3

    (Right res4) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res4 (Balances 55 1)

    let oid = fromRight res3
    reifyIO (cancelOrder oid) sim

    (Right res5) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res5 (Balances 105 1)

    res6 <- reifyIO (placeMarketOrder ASK 0.7) sim
    assertBool "expected success when placing market sell of value lower than commodity balance" . not $ isLeft res6

    threadDelay 1000

    (Right res7) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res7 (Balances 174.9 0.3)

    res8 <- reifyIO (placeLimitOrder BID 1 110) sim
    assertBool "expected success when placing limit buy of value lower than currency balance" . not $ isLeft res8

    threadDelay 1000

    (Right res9) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res9 (Balances 64.9 1.3)

    res10 <- reifyIO (placeMarketOrder BID 60) sim
    assertBool "expected success when placing market buy of value lower than currency balance" . not $ isLeft res10

    threadDelay 1000

    (Right res11) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res11 (Balances 4.9 1.8454545)

    res12 <- reifyIO (placeLimitOrder ASK 1 99) sim
    assertBool "expected success when placing limit sell of value lower than commodity balance" . not $ isLeft res12

    threadDelay 1000

    (Right res13) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res13 (Balances 104.525 0.8454545)

    res14 <- reifyIO (placeLimitOrder ASK 0.5454545 105) sim
    assertBool "expected success when placing limit sell of value lower than commodity balance" . not $ isLeft res14

    threadDelay 1000

    (Right res15) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res15 (Balances 104.525 0.3)

    let oid2 = fromRight res14
    reifyIO (cancelOrder oid2) sim

    (Right res16) <- reifyIO balances sim
    assertBool "balances should be equal" $ balancesEqual res16 (Balances 104.525 0.8454545)

    return ()

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

fromRight :: Either a b -> b
fromRight (Right r) = r

balancesEqual :: Balances -> Balances -> Bool
balancesEqual b1 b2 = (abs (view bCurrency b1 - view bCurrency b2) < 0.00001) && (abs (view bCommodity b1 - view bCommodity b2) < 0.00001)

---
---
---

testOrderBook = OrderBook {
    _obBids = [
        OrderBookEntry 0.5 100,
        OrderBookEntry 0.25 99.5,
        OrderBookEntry 2.25 99.0
    ],
    _obAsks = [
        OrderBookEntry 1.5 110,
        OrderBookEntry 0.5 110.1,
        OrderBookEntry 0.75 112
    ]
}

data TestExchange = TestExchange OrderBook

instance Exchange TestExchange where
    type ExchangeM TestExchange = Identity
    type ErrorT TestExchange = StdErr
    reifyIO act _ = return . Right $ runIdentity act

instance OrderBookP TestExchange where
    type OrderBookT TestExchange = OrderBook
    orderBook' (TestExchange ob) = return ob

instance TradeHistoryP TestExchange where
    type TradeT TestExchange = Trade
    tradeHistory' _ = return []

instance OrderP TestExchange where
    type OrderTypeT TestExchange = OrderType
    type OrderAmountT TestExchange = Double
    type OrderT TestExchange = Order
    type OrderIDT TestExchange = String
    currentOrders' = undefined
    placeLimitOrder' = undefined
    placeMarketOrder' = undefined
    cancelOrder' = undefined

instance BalancesP TestExchange where
    type BalancesT TestExchange = Balances
    balances' = undefined
