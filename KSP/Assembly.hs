module KSP.Assembly where

import Data.Maybe

import Math.Linear

import KSP.Data.Parts

import KSP.Stage

fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)

-- If a part's asymetrical we have to add two of them the first time we add one.
extend_symetric :: Part Rational -> Stage -> Stage
extend_symetric p s =
    if (asymetrical . geometry) p && notElem p (components s)
    then (extend p . extend p) s
    else extend p s
