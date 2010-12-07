{-# LANGUAGE FlexibleInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module : Generics.Regular.XmlPickler
-- Copyright : (c) 2009, typLAB
-- License : BSD3
--
-- Maintainer : typLAB <code@typlab.com>
-- Stability : Experimental
--
-- 'XmlPickler' instance for 'Bool' which converts to and from the Strings
-- \"true\" and \"false\", and 'GXmlPickler' instance for 'K' 'String', which
-- allows whitespace. These instances are automatically used if you
-- import 'Generics.Regular.XmlPickler'.
--
-------------------------------------------------------------------------------
module Generics.Regular.XmlPickler.Instances() where

import Generics.Regular
import Generics.Regular.XmlPickler.Function
import Text.XML.HXT.Arrow.Pickle

-- * Boolean instance for XmlPickler.

instance XmlPickler Bool where
  xpickle = (toBool, fromBool) `xpWrap` xpText

toBool :: String -> Bool
toBool "true"  = True
toBool "false" = False
toBool _       = error "No parse for bool in toBool (XmlPickler)."

fromBool :: Bool -> String
fromBool True  = "true"
fromBool False = "false"

-- * GXmlPickler instance for String.

instance GXmlPickler (K String) where
  gxpicklef _ = (K, unK) `xpWrap` xpText0
