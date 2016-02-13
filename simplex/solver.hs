import Data.Eigen.Matrix

nvars = 2
p = 3 -- conditions
n = nvars + p -- vars

b = fromLists [[11],[27],[90],[0],[0]]
a = fromLists [[-1,1,1,0,0],
	[1,1,0,1,0],
	[2,5,0,0,1]]
c = fromLists [[4],[6],[0],[0],[0]]


bi = submatrix 1 p (nvars+1) n a
ni = submatrix 1 p 1 nvars a
xb = 