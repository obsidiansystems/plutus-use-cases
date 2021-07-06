{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HMap
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import Data.Vessel
import Data.Vessel.Identity
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism
import Language.Javascript.JSaddle (eval, liftJSM)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom.Core
import Rhyolite.Frontend.App
import Safe (headMay)

import Common.Api
import Common.Route
import Common.Plutus.Contracts.Uniswap.Types
import Common.Plutus.Contracts.Uniswap.Estimates
import Common.Schema


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
        <> "integrity" =: "sha384-+0n0xVW2eSR5OomGNYDnhzAbDsOXxcvSN1TPprVMTNDbiYZCxYbOOl7+AMvyTG2x"
        <> "crossorigin" =: "anonymous"
        ) blank
  , _frontend_body = do
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
      let errorLeft e = case e of
            Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
            Right x -> x
      let validFullEncoder = errorLeft $ checkEncoder fullRouteEncoder
      _ <- runObeliskRhyoliteWidget vesselToWire "common/route" validFullEncoder (BackendRoute_Listen :/ ()) $ workflow $ app
      return ()
  }

app :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m)) => Workflow t m ()
app = Workflow $ do
  _ <- navBar Nothing
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h2" "display-5 fw-bold" $ text "Welcome to POKE-DEX!"
      el "p" $ text "POKE-DEX is a proto-type example of how a token exchange decentralized application would behave using smart contracts on the Cardano Blockchain. Below are some crypto wallets you can choose from to play around with this Dapp's features. You will be able to swap Ada for supported tokens, swap tokens, stake ada or other tokens for liquidity, and observe the wallet's portfoilio. Don't worry, this is not spending anyone's actual ADA. Select a wallet and give it a try!"
      elClass "h3" "display-5 fw-bold" $ text "Wallet Accounts"
      elClass "p" "lead" $ text "Choose one of the avaiable wallets below: "
      dmmWalletIds <- viewContracts
      walletEv <- switchHold never <=< dyn $ ffor dmmWalletIds $ \case
        Nothing -> do
          text "There are no wallets avaiable"
          return never
        Just mWalletIds -> case mWalletIds of
          Nothing -> do
            text "There are no wallets avaiable"
            return never
          Just walletIds -> do
            walletIdEvents <- elClass "ul" "list-group" $ do
              forM walletIds $ \wid -> fmap (switch . current) $ prerender (return never) $ do
                -- TODO: Add some highlight on hover for list items
                (e,_) <- elAttr' "li" ("class" =: "list-group-item list-group-item-dark" <> "style" =: "cursor:pointer") $ text wid
                return $ wid <$ domEvent Click e
            return $ leftmost walletIdEvents
      return ((), swapDashboard <$> walletEv)

data Dashboard = Dashboard_Swap | Dashboard_Portfolio | Dashboard_Pool
  deriving (Eq, Ord, Show)

navBar :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m)) => Maybe Text -> m (Event t Dashboard)
navBar mWid = divClass "navbar navbar-expand-md navbar-dark bg-dark" $ do
  divClass "container-fluid" $ do
    elAttr "a" ("class" =: "navbar-brand" <> "href" =: "#") $ text "POKE-DEX - Plutus Obelisk Koin Economy Decentralized Exchange "
      -- Note: This websocket keeps track of Slot number
      --   el "p" $ text "-------------------------------"
      -- Note: This websocket keeps track of Slot number
      -- ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
      -- _ <- widgetHold blank $ ffor (_webSocket_recv ws) $ \(a :: Maybe Aeson.Value) -> el "p" $ text $ T.pack $ show a
    case mWid of
      Nothing -> return never
      Just wid -> do
        -- select which page of the dashboard the user would like to see
        navSelect <- elClass "ul" "nav navbar-nav" $ do
          swapEv <- do
            (e,_) <- elClass' "li" "text-white nav-item ms-5" $ text "Swap"
            return $ Dashboard_Swap  <$ domEvent Click e
          portfolioEv <- do
            (e,_) <- elClass' "li" "text-white nav-item ms-5" $ text "Portfolio"
            return $ Dashboard_Portfolio  <$ domEvent Click e
          poolEv <- do
            (e,_) <- elClass' "li" "text-white nav-item ms-5" $ text "Pool"
            return $ Dashboard_Pool  <$ domEvent Click e
          return $ leftmost [swapEv, portfolioEv, poolEv]
        -- event that fires once the page has finished loading
        pb <- getPostBuild
        -- recurring event used to poll for wallet balance
        pollingEvent <- tickLossyFromPostBuildTime 10
        requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
        fmap (switch . current) $ prerender (return never) $ do
          -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
          ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
          -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
          let fundsEvent = flip ffilter  (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                Nothing -> False
                Just incomingWebSocketData -> do
                  let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                      fundsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                  observableStateTag == ["NewObservableState"] && fundsTag == ["Funds"]
          widgetHold_ blank $ ffor fundsEvent $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
            Nothing -> return ()
            Just incomingWebSocketData -> do
              -- aeson-lens happened here to unpack the json object received from the websocket
              let currencyDetails = incomingWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
                  mAdaBal = fmap fst <$> Map.lookup "" $ parseTokensToMap currencyDetails
              -- TODO: Use the ADA coin symbol
              elClass "p" "text-white" $ text $ "ADA Balance: " <> (T.pack $ show $ fromMaybe 0 mAdaBal)
          return navSelect

swapDashboard :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m))
  => Text
  -> Workflow t m ()
swapDashboard wid = Workflow $ do
  navEvent <- navBar $ Just wid
  pb <- getPostBuild
  -- recurring event used to poll for pool balance
  pollingEvent <- tickLossyFromPostBuildTime 10
  requesting_ $ (Api_CallPools (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
  let portfolioEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Portfolio
      poolEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Pool
  _ <- divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Swap Tokens"
      el "p" $ text "What would you like to swap?"
      dmmPooledTokens <- viewPooledTokens
      _ <- switchHold never <=< dyn $ ffor dmmPooledTokens $ \case
        Nothing -> return never
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> return never
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                firstOption = headMay $ Map.keys dropdownList
            case firstOption of
              Nothing -> do
                elClass "p" "text-warning" $ text "There are no tokens available to swap."
                return never
              Just fstOpt -> do
                divClass "form container" $ do
                  divClass "form-group" $ do
                    -- Select first token and amount
                    -- TODO: Create a convenient widget function out of the dropdown text input for coins
                    (selectionA, amountA) <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinAAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinAChoice, _inputElement_value coinAAmountInput)
                    -- Select second token and amount
                    (selectionB, amountB) <- divClass "input-group row mt-3" $ do
                      coinBChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                    -- TODO: add loading modal
                    swap <- divClass "input-group row mt-3" $ do
                      (e,_) <- elClass' "button" "btn btn-primary" $ text "Swap"
                      return $ domEvent Click e
                    let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                        requestLoad = ((\w c1 c2 a1 a2 -> Api_Swap w c1 c2 a1 a2)
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amountA)
                          <*> (toAmount <$> amountB))
                    -- This response doesn't return anything useful, so it is thrown away
                    _ <- requesting $ tagPromptlyDyn requestLoad swap
                    _ <- fmap (switch . current) $ prerender (return never) $ do
                      ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
                      -- TODO: Create abstracted function for filtering out websocket events
                      let observableStateSuccessEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    swappedTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                                newObservableStateTag == ["NewObservableState"] && swappedTag == ["Swapped"]
                          observableStateFailureEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    failureMessageTag = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                newObservableStateTag == ["NewObservableState"] && failureMessageTag /= [""]
                          -- Pools is used to get information about liquidity pools and token pairs to provide swap estimations
                          poolsEvent = flip ffilter  (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                            Nothing -> False
                            Just incomingWebSocketData -> do
                              let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                  poolsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                              observableStateTag == ["NewObservableState"] && poolsTag == ["Pools"]
                      dynPoolMap <- holdDyn Map.empty $ ffor poolsEvent $ \mIncomingPoolsWebSocketData -> do
                        let poolDetails = case mIncomingPoolsWebSocketData of
                              Nothing -> V.empty
                              Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
                        parseLiquidityTokensToMap poolDetails
                      -- Determine what amount will be received after
                      let swapInput = ffor2 amountA amountB $ \a b -> (a,b)
                          ffor4 a b c d f = liftA3 f a b c <*> d
                          swapEstimate = ffor4 selectionA selectionB swapInput dynPoolMap $ \selA selB (amtA, amtB) poolMap -> do
                            let ((coinAName, coinAPoolAmount), (coinBName, coinBPoolAmount)) = fromMaybe (("", 0), ("", 0))
                                  $ fmap snd
                                  $ headMay
                                  $ Map.elems
                                  $ Map.filter (\(_,((tknameA,_), (tknameB, _)))
                                  -> tknameA == (_pooledToken_name selA) && tknameB == (_pooledToken_name selB)) poolMap
                            (amtA',_) :: (Integer, Text) <- T.decimal amtA
                            (amtB',_) :: (Integer, Text) <- T.decimal amtB
                            let (swapAmount, estimatedTkName) = if amtA' == 0 then (amtB',coinAName) else (amtA',coinBName)
                            return $ (findSwapA coinAPoolAmount coinBPoolAmount swapAmount, estimatedTkName)
                      -- display estimated funds to be received during swap and the token name to be received
                      divClass "p-3 mt-2" $ widgetHold_ blank $ ffor (updated swapEstimate) $ \case
                        Left err -> elClass "p" "text-danger" $ text $ T.pack $ show err
                        Right (estimate, eTokenName) -> elClass "p" "text-info" $ text
                          $ "In this transaction you will receive "
                          <> (T.pack $ show estimate)
                          <> " " <> (T.pack $ show $ if "" == eTokenName then "ADA" else eTokenName)
                      -- this event will cause the success message to disappear when it occurs
                      vanishEvent <- delay 7 observableStateSuccessEvent
                      -- show success message based on new observable state
                      widgetHold_ blank $ ffor (leftmost [observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just _ -> elClass "p" "text-success" $ text "Success!"
                      widgetHold_ blank $ ffor observableStateFailureEvent $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just incomingWebSocketData -> do
                            let errMsg = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                            elClass "p" "text-danger" $ text $ T.concat errMsg
                      return never
                    return ()
                return never
      return ()
  return ((), leftmost [(portfolioDashboard wid) <$ portfolioEv, poolDashboard wid <$ poolEv])

portfolioDashboard :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m))
  => Text
  -> Workflow t m ()
portfolioDashboard wid = Workflow $ do
  -- TODO: Consider refactoring workflow code a bit to avoid redrawing navbar whenever a new tab is seleted
  navEvent <- navBar $ Just wid
  let swapEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Swap
      poolEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Pool
  _ <- divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Portfolio"
      el "p" $ text "Here are your tokens."
      pb <- getPostBuild
      -- recurring event used to poll for wallet balance
      pollingEvent <- tickLossyFromPostBuildTime 10
      requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
      fmap (switch . current) $ prerender (return never) $ do
        -- incorporate the use of PAB's websockets to display the wallet's current Ada Balance
        ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
        -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
        let fundsEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
              Nothing -> False
              Just incomingWebSocketData -> do
                let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                    fundsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                observableStateTag == ["NewObservableState"] && fundsTag == ["Funds"]
        widgetHold_ blank $ ffor fundsEvent $ \(mIncomingFundsWebSocketData :: Maybe Aeson.Value) -> case mIncomingFundsWebSocketData of
          Nothing -> return ()
          Just fundsWebSocketData -> do
            let currencyDetails = fundsWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
            elClass "ul" "list-group" $ do
              let formattedTokenDetails = Map.filter
                    -- TODO: Don't lookup tokens by hard coded currencySymbol value
                    (\(_,cs) -> cs == "7c7d03e6ac521856b75b00f96d3b91de57a82a82f2ef9e544048b13c3583487e") $
                    parseTokensToMap currencyDetails
              elClass "li" "list-group-item" $ do
                forM_ (Map.toList formattedTokenDetails) $ \(tokenName, (tokenBalance,_)) ->
                  el "p" $ text $ "Token Name: " <> (T.pack $ show tokenName) <> "Balance: " <> (T.pack $ show tokenBalance)
              return ()
            return ()
        return never
  return ((), leftmost [swapDashboard wid <$ swapEv, poolDashboard wid <$ poolEv])

poolDashboard :: forall t m js. (MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m, Prerender js t m, MonadIO (Performable m))
  => Text
  -> Workflow t m ()
poolDashboard wid = Workflow $ do
  navEvent <- navBar $ Just wid
  let portfolioEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Portfolio
      swapEv  = flip ffilter navEvent $ \navEv -> navEv == Dashboard_Swap
  -- Widget to show liquidity pool blanance
  pb <- getPostBuild
  -- recurring event used to poll for pool balance
  pollingEvent <- tickLossyFromPostBuildTime 10
  -- give pab time to process incoming requests
  initPoolEvent <- delay 1 pb
  getPoolEvent <- delay 5 pollingEvent
  requesting_ $ (Api_CallFunds (ContractInstanceId wid)) <$ (leftmost [pb, () <$ pollingEvent])
  requesting_ $ (Api_CallPools (ContractInstanceId wid)) <$ (leftmost [initPoolEvent, () <$ getPoolEvent])
  _ <- fmap (switch . current) $ prerender (return never) $ do
    -- incorporate the use of PAB's websockets to display the wallet's current Pool Balance
    ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
    -- filter for websocket events relevent to funds that contain the "Funds" tag and "NewObservableState" tag
    let fundsEvent = flip ffilter  (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
          Nothing -> False
          Just incomingWebSocketData -> do
            let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                fundsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
            observableStateTag == ["NewObservableState"] && fundsTag == ["Funds"]
        poolsEvent = flip ffilter  (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
          Nothing -> False
          Just incomingWebSocketData -> do
            let observableStateTag = incomingWebSocketData ^.. key "tag" . _String
                poolsTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
            observableStateTag == ["NewObservableState"] && poolsTag == ["Pools"]
    dFunds <- holdDyn Nothing fundsEvent
    dPools <- holdDyn Nothing poolsEvent
    let fundsAndPools = ffor2 dFunds dPools $ \f p -> (f,p)
    widgetHold_ blank $ ffor (updated fundsAndPools) $
      \(mIncomingFundsWebSocketData :: Maybe Aeson.Value, mIncomingPoolsWebSocketData :: Maybe Aeson.Value) ->
        case mIncomingFundsWebSocketData of
          Nothing -> return ()
          Just fundsWebSocketData -> do
            let currencyDetails = fundsWebSocketData ^. key "contents" . key "Right" . key "contents" . key "getValue" . _Array
                poolDetails = case mIncomingPoolsWebSocketData of
                  Nothing -> V.empty
                  Just poolsWebSocketData -> poolsWebSocketData ^. key "contents" . key "Right" . key "contents" . _Array
                poolMap = parseLiquidityTokensToMap poolDetails
            divClass "p-5 mb-4 bg-light rounded-5" $ divClass "container-fluid py-5" $ do
              elClass "ul" "list-group" $ do
                let formattedTokenDetails = Map.filter
                      -- TODO: Don't lookup tokens by hard coded currencySymbol value
                      (\(_,cs) -> cs == "078ea50abc14180a537b6815a6e8562021bde4eaf0d4c5738290b423df3abeb8") $
                      parseTokensToMap currencyDetails
                elClass "li" "list-group-item" $ do
                  forM_ (Map.toList formattedTokenDetails) $ \(tokenName, (tokenBalance,_)) -> do
                    let mLiquidityInfo = Map.lookup tokenName poolMap
                    case mLiquidityInfo of
                      Nothing -> blank
                      Just (lqTotal, ((tokenNameA, _),(tokenNameB,_))) -> do
                        let lqPercentage :: Double = ((fromIntegral tokenBalance) / (fromIntegral lqTotal)) * 100
                        el "p" $ text $ "Token Name: "
                          <> (T.pack $ show tokenName)
                          <> " (Composed of tokens "
                          <> (T.pack $ show $ if tokenNameA == "" then "Ada" else tokenNameA)
                          <> " and "
                          <> (T.pack $ show $ if tokenNameB == "" then "Ada" else tokenNameB)
                          <> " Balance: "
                          <> (T.pack $ show tokenBalance)
                          <> " Liquidity Percentage: "
                          <> (T.pack $ show lqPercentage)
                          <> "%"
    return never
  -- TODO: Widget to redeem liquidity pool blanance
  _ <- divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Redeem Liquidity"
      el "p" $ text "Which liquidity pool would you like to redeem and how much?"
      dmmPooledTokens <- viewPooledTokens
      _ <- switchHold never <=< dyn $ ffor dmmPooledTokens $ \case
        Nothing -> return never
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> return never
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                firstOption = headMay $ Map.keys dropdownList
            case firstOption of
              Nothing -> do
                elClass "p" "text-warning" $ text "There are no tokens available to redeem."
                return never
              Just fstOpt -> do
                divClass "form container" $ do
                  divClass "form-group" $ do
                    -- Select first token
                    -- TODO: Create a convenient widget function out of the dropdown text input for coins
                    selectionA <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      return $ _dropdown_value coinAChoice
                    -- Select second token
                    selectionB<- divClass "input-group row mt-3" $ do
                      coinBChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      return $ _dropdown_value coinBChoice
                    -- Select amount
                    amount <- divClass "input-group row mt-3" $ do
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return $ _inputElement_value coinBAmountInput
                    -- TODO: add loading modal
                    redeem <- divClass "input-group row mt-3" $ do
                      (e,_) <- elClass' "button" "btn btn-primary" $ text "Redeem"
                      return $ domEvent Click e
                    let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                        requestLoad = ((\w c1 c2 a -> Api_RedeemLiquidity w c1 c2 a)
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amount))
                    -- This response doesn't return anything useful, so it is thrown away
                    _ <- requesting $ tagPromptlyDyn requestLoad redeem
                    _ <- fmap (switch . current) $ prerender (return never) $ do
                      ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
                      -- TODO: Create an abstracted function for filtering out websocket events
                      let observableStateSuccessEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    swappedTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                                newObservableStateTag == ["NewObservableState"] && swappedTag == ["Removed"]
                          observableStateFailureEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    failureMessageTag = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                newObservableStateTag == ["NewObservableState"] && failureMessageTag /= [""]
                      -- this event will cause the success message to disappear when it occurs
                      vanishEvent <- delay 7 observableStateSuccessEvent
                      -- show success message based on new observable state
                      widgetHold_ blank $ ffor (leftmost [observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just _ -> elClass "p" "text-success" $ text "Success!"
                      widgetHold_ blank $ ffor observableStateFailureEvent $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just incomingWebSocketData -> do
                            let errMsg = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                            elClass "p" "text-danger" $ text $ T.concat errMsg
                      return never
                    return ()
                return never
      return ()
  -- Widget with form to allow user to stake/add to pool
  _ <- divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h3" "display-5 fw-bold" $ text "Stake Tokens"
      el "p" $ text "What would you like to stake?"
      dmmPooledTokens <- viewPooledTokens
      _ <- switchHold never <=< dyn $ ffor dmmPooledTokens $ \case
        Nothing -> return never
        Just mPoolTokens -> case mPoolTokens of
          Nothing -> return never
          Just poolTokens -> do
            let dropdownList = Map.fromListWith (\_ tkName -> tkName) $ flip fmap poolTokens $ \pt ->
                  if _pooledToken_name pt == "" then (pt, "ADA") else (pt, _pooledToken_name pt)
                firstOption = headMay $ Map.keys dropdownList
            case firstOption of
              Nothing -> do
                elClass "p" "text-warning" $ text "There are no tokens available to stake."
                return never
              Just fstOpt -> do
                divClass "form container" $ do
                  divClass "form-group" $ do
                    -- Select first token and amount
                    -- TODO: Create a convenient widget function out of the dropdown text input for coins
                    (selectionA, amountA) <- divClass "input-group row" $ do
                      coinAChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinAAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinAChoice, _inputElement_value coinAAmountInput)
                    -- Select second token and amount
                    (selectionB, amountB) <- divClass "input-group row mt-3" $ do
                      coinBChoice <- dropdown fstOpt (constDyn $ dropdownList) $
                        def { _dropdownConfig_attributes = constDyn ("class" =: "form-control col-md-1") }
                      coinBAmountInput <- inputElement $ def
                        & inputElementConfig_elementConfig . elementConfig_initialAttributes
                          .~ ("class" =: "form-control col-md-4" <> "type" =: "number")
                        & inputElementConfig_initialValue
                          .~ ("0" :: Text)
                      return (_dropdown_value coinBChoice, _inputElement_value coinBAmountInput)
                    -- TODO: add loading modal
                    stake <- divClass "input-group row mt-3" $ do
                      (e,_) <- elClass' "button" "btn btn-primary" $ text "Stake"
                      return $ domEvent Click e
                    let pooledTokenToCoin pt = Coin $ AssetClass (CurrencySymbol (_pooledToken_symbol pt), TokenName (_pooledToken_name pt))
                        toAmount amt = Amount $ (read (T.unpack amt) :: Integer)
                        requestLoad = ((\w c1 c2 a1 a2 -> Api_Stake w c1 c2 a1 a2)
                          <$> (constDyn $ ContractInstanceId wid)
                          <*> (pooledTokenToCoin <$> selectionA)
                          <*> (pooledTokenToCoin <$> selectionB)
                          <*> (toAmount <$> amountA)
                          <*> (toAmount <$> amountB))
                    -- This response doesn't return anything useful, so it is thrown away
                    _ <- requesting $ tagPromptlyDyn requestLoad stake
                    _ <- fmap (switch . current) $ prerender (return never) $ do
                      ws <- jsonWebSocket ("ws://localhost:8080/ws/" <> wid) (def :: WebSocketConfig t Aeson.Value)
                      -- TODO: Create an abstracted function for filtering out websocket events
                      let observableStateSuccessEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    swappedTag = incomingWebSocketData ^.. key "contents" . key "Right" . key "tag" . _String
                                newObservableStateTag == ["NewObservableState"] && swappedTag == ["Added"]
                          observableStateFailureEvent = flip ffilter (_webSocket_recv ws) $ \(mIncomingWebSocketData :: Maybe Aeson.Value )
                            -> case mIncomingWebSocketData of
                              Nothing -> False
                              Just incomingWebSocketData -> do
                                let newObservableStateTag = incomingWebSocketData ^.. key "tag" . _String
                                    failureMessageTag = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                                newObservableStateTag == ["NewObservableState"] && failureMessageTag /= [""]
                      -- this event will cause the success message to disappear when it occurs
                      vanishEvent <- delay 7 observableStateSuccessEvent
                      -- show success message based on new observable state
                      widgetHold_ blank $ ffor (leftmost [observableStateSuccessEvent, Nothing <$ vanishEvent]) $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just _ -> elClass "p" "text-success" $ text "Success!"
                      widgetHold_ blank $ ffor observableStateFailureEvent $
                        \(mIncomingWebSocketData :: Maybe Aeson.Value) -> case mIncomingWebSocketData of
                          Nothing -> blank
                          Just incomingWebSocketData -> do
                            let errMsg = incomingWebSocketData ^.. key "contents" . key "Left" . _String
                            elClass "p" "text-danger" $ text $ T.concat errMsg
                      return never
                    return ()
                return never
      return ()
  return ((), leftmost [swapDashboard wid <$ swapEv, portfolioDashboard wid <$ portfolioEv])

-- TODO: Create a websocket parsing module for this
parseTokensToMap :: V.Vector Aeson.Value -> Map.Map Text (Integer, Text)
parseTokensToMap cd = mconcat $ catMaybes $ V.toList $ ffor cd $ \case
  Aeson.Array xs -> case V.toList xs of
    symbol:(Aeson.Array tokens):_ ->
      let currencySymbol = symbol ^. key "unCurrencySymbol" . _String
          tokens' = ffor tokens $ \case
            Aeson.Array xs' -> case V.toList xs' of
              tokenName:tokenAmount:_ -> Just
                ( tokenName ^. key "unTokenName" . _String
                , (fromMaybe 0 $ tokenAmount ^? _Integer, currencySymbol)
                )
              _ -> Nothing
            _ -> Nothing
      in Just $ Map.fromList $ catMaybes $ V.toList tokens'
    _ -> Nothing
  _ -> Just Map.empty

-- returns Map of TokenName (LiquidityAmount, ((CoinA, CoinAPoolAmount), (CoinB, CoinBPoolAmount)))
parseLiquidityTokensToMap :: V.Vector Aeson.Value -> Map.Map Text (Integer, ((Text,Integer), (Text, Integer)))
parseLiquidityTokensToMap cd = mconcat $ catMaybes $ V.toList $ ffor cd $ \case
  Aeson.Array xs -> case V.toList xs of
    coinA:coinB:(Aeson.Array liquidityCoin):_-> case V.toList liquidityCoin of
      (Aeson.Object obj):(Aeson.Number lqAmount):_ -> case HMap.toList obj of
        (_, Aeson.String lqTokenName):_ -> do
          let tna = coinA ^. nth 0 . key "unAssetClass" . values . key "unTokenName" . _String
              tnaPool = fromMaybe 0 $ coinA ^? nth 1 . _Integer
              tnb = coinB ^. nth 0 . key "unAssetClass" . values . key "unTokenName" . _String
              tnbPool = fromMaybe 0 $ coinB ^? nth 1 . _Integer
          Just $ Map.singleton lqTokenName ((fromMaybe 0 $ lqAmount ^? _Integer), ((tna, tnaPool),(tnb, tnbPool)))
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
  _ -> Just Map.empty

viewContracts :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [Text])))
viewContracts = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_ContractList . identityV

viewPooledTokens :: (MonadQuery t (Vessel Q (Const SelectedCount)) m, Reflex t) => m (Dynamic t (Maybe (Maybe [PooledToken])))
viewPooledTokens = (fmap.fmap.fmap) (getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_PooledTokens . identityV
