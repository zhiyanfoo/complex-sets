import Data.Complex (Complex((:+)))
import qualified Data.Complex as C (realPart)
import qualified Data.Map as M (fromList, Map)

main = print $ mandelbrot (-2, 2) (-2, 2) 20 20 40

quadratic :: Num a => a -> a -> a
quadratic c z = z^2 + c

isBounded f lim z k 
    | lim < k = True
    | C.realPart z <= 2 = isBounded f lim result (k+1)
    | otherwise = False
    where result = f z

isMandelbrot c lim = isBounded (quadratic c) lim 0 0

mandelbrot (x1, x2) (y1, y2) n1 n2 lim = M.fromList l
    where points = [ (x, y) | x <- linspace (x1, x2) n1, 
                              y <- linspace (y1, y2) n2 ]
          pointMandel point = isMandelbrot (uncurry (:+) point) lim
          l = [ point | point <- points , pointMandel point ]


linspace (x1, x2) n = x1 : linspaceNext difference [x2] (n - 2)
    where difference = abs (x1 - x2) / (n - 1)

linspaceNext d li n
    | n == 0 = li
    | otherwise = linspaceNext d (ele : li) (n - 1)
    where ele = head li - d
