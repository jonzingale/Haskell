module CSVWriter where
import qualified Data.ByteString.Lazy as BL
import HotelAssignment hiding (main)
import NamedCSVParser hiding (main)

import Data.List.Split
import Data.List

addRows :: String -> IO()
addRows str = appendFile "./tmp.csv" str

rwCsv file = do
    let rows = ["Wyndham,Tulin Kurban,71219,80270,WYNDHAM GARDEN DRESDEN,OPMS,DRESDENLEUBNITZ,GERMANY,\"Europe, Middle East, Eurasia and Africa\",EMEA,-,1219,WILHELM-FRANKE-STRASSE 90,1/31/14,262","Days Inn,,6789,,Days Inn Dresden,,,,,,,,,,","Ramada,,12334,,Ramada Dresden,,,,,,,,,,","Wyndham,,10415,,Wyndham Santa Fe,,,,,,,,,,","Days Inn,,448,82272,DAYS INN VERNON,SYNXI,VERNON,UNITED STATES,North America,NAMER,CT,6066,451 HARTFORD TURNPIKE,8/28/13,64","Ramada,,206,80886,RAMADA FRESNO NORTH,SYNXI,FRESNO,UNITED STATES,North America,NAMER,CA,93710,324 E. SHAW AVENUE,1/3/69,168","Howard Johnson,,245,84248,HOWARD JOHNSON EXPRESS INN - WILMINGTON,SYNXI,WILMINGTON,UNITED STATES,North America,NAMER,NC,28403,3901 MARKET STREET,7/16/91,79","Ramada,,252,80909,RAMADA ANTIOCH,OPMS,ANTIOCH,UNITED STATES,North America,NAMER,CA,94509,2436 MAHOGANY WAY,2/18/91,116","Howard Johnson,,291,84164,HOWARD JOHNSON INN - WARRENTON,SYNXI,WARRENTON,UNITED STATES,North America,NAMER,VA,20186,6 BROADVIEW AVENUE,8/13/63,79","Howard Johnson,,298,84249,HOWARD JOHNSON INN BEAUFORT/PARRIS ISLAND,SYNXI,BEAUFORT,UNITED STATES,North America,NAMER,SC,29906,3651 TRASK PARKWAY,2/14/90,63","Ramada,,318,80855,RAMADA LIMITED NORFOLK,OPMS,NORFOLK,UNITED STATES,North America,NAMER,VA,23502,515 NORTH MILITARY HIGHWAY,12/30/93,149","Ramada,,320,80910,RAMADA PLAZA WEST HOLLYWOOD HOTEL & SUITES,OPMS,WEST HOLLYWOOD,UNITED STATES,North America,NAMER,CA,90069,8585 SANTA MONICA BLVD.,4/1/89,176","Howard Johnson,,322,84165,HOWARD JOHNSON PLAZA HOTEL MADISON,OPMS,MADISON,UNITED STATES,North America,NAMER,WI,53704,3841 E.WASHINGTON AVE,2/6/06,197","Howard Johnson,,358,84250,HOWARD JOHNSON HOTEL - TOMS RIVER,OPMS,TOMS RIVER,UNITED STATES,North America,NAMER,NJ,8753,955 HOOPER AVE.,6/10/65,96","Days Inn,,359,82373,DAYS INN OCALA NORTH,SYNXI,OCALA,UNITED STATES,North America,NAMER,FL,34475,3811 NW BLITCHTON ROAD,1/11/94,65","Days Inn,,361,82466,DAYS INN WAYNE,SYNXI,WAYNE,UNITED STATES,North America,NAMER,NJ,7470,1850 NEW JERSEY 23,1/19/17,146","Ramada,,395,80911,RAMADA SOUTH EL MONTE,OPMS,SOUTH EL MONTE,UNITED STATES,North America,NAMER,CA,91733,1089 S. SANTA ANITA AVE.,11/13/90,106","Ramada,,426,80913,RAMADA PLAZA FORT WALTON BEACH RESORT/DESTIN,OPMS,FORT WALTON BEA,UNITED STATES,North America,NAMER,FL,32548,1500 MIRACLE STRIP PKY SE,9/14/85,335","Days Inn,,448,82272,DAYS INN VERNON,SYNXI,VERNON,UNITED STATES,North America,NAMER,CT,6066,451 HARTFORD TURNPIKE,8/28/13,64","Ramada,,456,80926,RAMADA TOMS RIVER,OPMS,TOMS RIVER,UNITED STATES,North America,NAMER,NJ,8755,2373 ROUTE 9,8/17/90,154","Howard Johnson,,466,84251,HOWARD JOHNSON - PRINCETON/LAWRENCEVILLE,SYNXI,LAWRENCEVILLE,UNITED STATES,North America,NAMER,NJ,8648,2995 BRUNSWICK PIKE,2/19/62,104"]
    let siteids = [448,206,245,252,291,298,318,320,322,358,359,361,395,426,448,456,466]
    csv <- readFile file
    -- splits and removes header
    let (header:rows) = splitOn "\r" csv

    -- function here which filters for matching site ids.
    let filteredRows = filterRows rows siteids

    -- rebuilds file from array to string with header
    let str = intercalate "\r" (header : filteredRows)
    addRows str

inSiteIdList :: String -> [Integer] -> Bool
inSiteIdList rec = any ((==).read.(!! 2).splitOn "," $ rec)

filterRows rs sids = filter (\x -> inSiteIdList x sids) rs

-- test
main = do
  google <- BL.readFile "./google.csv"
  wyndham <- BL.readFile "./wyndham.csv"
  let grecords = googleRecords google
  let wrecords = wyndhamNamedRecords wyndham
  let siteids = missing wrecords grecords

  wynCsv <- readFile "./wyndham.csv"
  let (header:rows) = splitOn "\r" wynCsv
  let filteredRows = filterRows rows siteids
  let csv = intercalate "\r" (header : filteredRows)
  addRows csv