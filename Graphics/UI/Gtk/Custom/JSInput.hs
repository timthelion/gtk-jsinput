{-GPLV3.0 or later copyright Timothy Hobbs <timothyhobbs@seznam.cz>

Copyright 2012.

This program is free software:
you can redistribute it and/or modify it
under the terms of the GNU General Public License
as published by the Free Software Foundation,
either version 3 of the License,
or
(at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY;
without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy
of the GNU General Public License along with this program.
If not,
see <http://www.gnu.org/licenses/>.
-}

module Graphics.UI.Gtk.Custom.JSInput where

{-
Generates a simple form which allows users to input JSON values of type Bool, Rational and String.
Saving of the form data is performed on "focus change".
This means that you provide jsInputNew with a special callback
and that callback gets run every time the user changes a value in the form.
You can then save the contents of the form,
or sync them to your application's own internal state.
-}

import Text.JSON
import Data.Ratio
import Graphics.UI.Gtk as GTK
import Data.IORef
import Control.Monad.IO.Class

{-main :: IO ()
main = do
   GTK.initGUI       -- is start
   window <- GTK.windowNew
   let
    feilds =
     [("String",JSString $ toJSString "")
     ,("Bool",JSBool False)
     ,("Rational",JSRational False (0%1))]
   jsInput <- jsInputNew feilds
    (\newValuesR->
     case newValuesR of
      Ok values -> putStrLn $ show values
      Error err -> putStrLn err)
   GTK.containerAdd window jsInput
   GTK.onDestroy window GTK.mainQuit
   GTK.widgetShowAll window
   GTK.mainGUI
   return ()-}

jsInputNew ::
 [(String,JSValue)] ->
 (Result [(String,JSValue)] -> IO())->
 IO Widget
jsInputNew
 feilds
 onUpdate
  = do
 vb <- GTK.vBoxNew False 0
 let
  (JSObject initialObject) = makeObj feilds
 valuesIORef <- newIORef feilds
 let
  addFeild (key,value) = do
   element <- case value of
    JSBool checked -> do
     b <- GTK.checkButtonNewWithLabel key
     set b [toggleButtonActive := checked
           ,toggleButtonMode   := True]
     b `on` GTK.toggled $ do
      values <- readIORef valuesIORef
      value <- get b toggleButtonActive
      let
       newValues =
        map
         (\(k,v)->
          case k == key of
           True -> (k,JSBool value)
           False -> (k,v))
         values
      writeIORef valuesIORef newValues
      onUpdate $ Ok newValues
     GTK.boxPackStart vb b GTK.PackNatural 0
     return $ castToWidget b
    r@JSRational{} -> do
     hb <- hBoxNew False 0
     l <- labelNew $ Just key
     GTK.boxPackStart hb l GTK.PackNatural 0
     e <- GTK.entryNew
     entrySetText e $ encode r
     e `on` GTK.focusOutEvent $ liftIO $ do
      values <- readIORef valuesIORef
      text <- get e entryText
      let
       valueR' = decode text
       valueR  =
        case valueR' of
         Ok (rational@JSRational{}) -> Ok rational
         Ok _ -> Error "Not a rational."
         Error err -> Error err
       newValuesR =
        case valueR of
         Ok val ->
          Ok $ map
           (\(k,v)->
            case k == key of
             True -> (k, val)
             False -> (k,v))
           values
         Error err -> Error err
      case newValuesR of
       Ok newValues ->  writeIORef valuesIORef newValues
       _ -> return ()
      onUpdate $ newValuesR
      return False
     GTK.boxPackStart hb e GTK.PackNatural 0
     GTK.boxPackStart vb hb GTK.PackNatural 0
     return $ castToWidget hb
    JSString jsstring -> do
     hb <- vBoxNew False 0
     l <- labelNew $ Just key
     GTK.boxPackStart hb l GTK.PackNatural 0
     tb <- GTK.textBufferNew Nothing
     GTK.textBufferSetText tb $ fromJSString jsstring
     tv <- GTK.textViewNewWithBuffer tb
     tv `on` GTK.focusOutEvent $ liftIO $ do
      values <- readIORef valuesIORef
      text <- get tb textBufferText
      let
       newValuesR =
          Ok $ map
           (\(k,v)->
            case k == key of
             True ->  (k, JSString $ toJSString text)
             False -> (k,v))
           values
      case newValuesR of
       Ok newValues ->  writeIORef valuesIORef newValues
       _ -> return ()
      onUpdate $ newValuesR
      return False
     GTK.boxPackStart hb tv GTK.PackGrow 0
     GTK.boxPackStart vb hb GTK.PackGrow 0
     return $ castToWidget hb
    _ -> return $ error "Unsupported value type.  We only support Bool Rational and String, sorry!"
   return ()
 mapM addFeild feilds
 return $ castToWidget vb
