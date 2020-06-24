module Module where

import Data.List
import Data.Map as M
-- import module in prelude
-- :m + moduleName

-- import module exclude some 
-- specific functions
-- import moduleName hiding (functions)

-- only import some functions
-- import moduleName (functions)

-- dealing clashes of function name 
-- (overloading) when import modules
-- import qualified moduleName
-- 
-- e.g. import qualified Data.Map
--      filter (from prelude)
--      Data.Map.filter (from module)

-- abbreviation the module name when 
-- calling the function.
-- Have to be capital letter
-- import qualified moduleName as M
-- import moduleName as M