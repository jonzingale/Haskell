module Main where
import RayTracer.Transport
import RayTracer.FileToVector

{--
Todo:
* Simulate 1M rays from gaussian point source
* Parallelize code
* Monadic Run?
* Tests: Verify / Validate
* Is Strictness on FileToVector valuable?
--}

main = do
  myArray <- anArray
  return $ totalAttenuation (pi/4) myArray -- error on pi/2
