module Main where

import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Matrix

type P2 = (Int, Int)
type P3 = (Int, Int, Int)
type P2f = (Float, Float)
type P3f = (Float, Float, Float)
type Light = Float
type Color = Int
type PGM = String

width = 1000
height = 4000
depth = 255
wavelength = 10
ampl = 100

sources = [ (200, 500, 100), (800, 500, 100), (500, 900, 100) ]

interact' point = abs . sum . map ((*ampl) . sin . (/wavelength) . dist3D point)
dist3D (x,y,z) (x',y',z') = sqrt $ ((x-x')**2) + ((y-y')**2) + ((z-z')**2)
calc (x,y) = floor $ interact' (fromIntegral x, fromIntegral y, 0) sources
imgMatrix = matrix width height calc
imagePoints = [(x,y) | y <- [0..height-1], x <- [0..width-1]]
matrixL = chunksOf width $ fmap show $ toList $ transpose imgMatrix
image = "P2\n" ++ show width ++ " " ++ show height ++ "\n" ++ show depth ++ "\n"
      ++ (unlines $ fmap unwords matrixL)
main = putStrLn image
