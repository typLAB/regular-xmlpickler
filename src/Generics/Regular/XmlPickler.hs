-------------------------------------------------------------------------------
-- |
-- Module : Generics.Regular.XmlPickler
-- Copyright : (c) 2009, typLAB
-- License : BSD3
--
-- Maintainer : typLAB <code@typlab.com>
-- Stability : Experimental
--
-- Generic 'XmlPickler'. This module allows you to generically convert
-- your datatype to and from XML. If you don't want the (G)XmlPickler
-- instances for 'Bool' and 'String', use
-- 'Generic.Regular.XmlPickler.Function' instead.
--
-------------------------------------------------------------------------------
module Generics.Regular.XmlPickler (gxpickle, GXmlPickler(..)) where

import Generics.Regular.XmlPickler.Function
import Generics.Regular.XmlPickler.Instances()
