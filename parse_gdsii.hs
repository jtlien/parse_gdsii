    
import List
import Data.Bits
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Char

header    = "0002"                  --  2-byte integer 
bgnlib    = "0102"                  --  12 2-byte integers 
libname   = "0206"                  --  ASCII string 
reflibs   = "1F06"                  --  2 45-character ASCII strings 
fonts     = "2006"                  --  4 44-character ASCII strings 
attrtable = "2306"                  --  44-character ASCII string 
generations = "2202"                --   2-byte integer 
format    = "3602"                  --   2-byte integer 
mask      = "3706"                  --   ASCII string 
endmasks  = "3800"                  --   No data 
units     = "0305"                  --  2 8-byte floats 
  
-- File Tail Records: Bytes 3 and 4 Parameter Type 
 
endlib    = "0400"                  --  No data 
  
-- Structure Header Records:   Bytes 3 and 4 Parameter Type 

bgnstr    = "0502"                  --  12 2-byte integers 
strname   = "0606"                  --  Up to 32-characters ASCII string 
  
-- Structure Tail Records: Bytes 3 and 4 Parameter Type 

endstr = "0700"                  --  No data 
 
-- Element Header Records: Bytes 3 and 4   Parameter Type 
 
boundary = "0800"                    --  No data 
path     = "0900"                    --  No data 
sref     = "0A00"                    --  No data 
aref     = "0B00"                    --  No data 
text     = "0C00"                    --  No data 
node     = "1500"                    --  No data 
box      = "2D00"                    --  No data 
  
 
elflags      = "2601"                   --  2-byte integer 
plex         = "2F03"                   --  4-byte integer 
layer        = "0D02"                   --  2-byte integers 
datatype     = "0E02"                   --  2-byte integer 
xy           = "1003"                   --  Up to 200 4-byte integer pairs 
pathtype     = "2102"                   --  2-byte integer 
width        = "0F03"                   --  4-byte integer 
sname        = "1206"                   --  Up to 32-character ASCII string 
strans       = "1A01"                   --  2-byte integer 
mag          = "1B05"                   --  8-byte float 
angle        = "1C05"                   --  8-byte float 
colrow       = "1302"                   --  2 2-byte integers 
texttype     = "1602"                   --  2-byte integer 
presentation = "1701"                   --  2-byte integer 
ascii_string = "1906"                   --  Up to 512-character string 
nodetype     = "2A02"                   --  2-byte integer 
boxtype      = "2E02"                   --  2-byte integer 
propattr     = "2B02"                   --  property attribute
propvalue    = "2C06"                   --  property value
libdirsize   = "3902"                   -- number of pages in library directory
bgnextn      = "3003"                   -- for type 4 path
endextn      = "3103"                   -- for type 4 path
srfname      = "3A06"                   -- sticks file name
libsecure    = "3B02"                   -- libsecure  ancient access rights
strclass     = "3401"                   -- structure class for calma

nodata       = "0004"
byte2_header = "0006"
byte4_header = "0008"
float_header = "000C"

endel        = "1100"
datareal8    = "0008"
date_data    = "001C"
datareal8x2  = "0014"

archived = "0000"
filtered = "0001"

type Xytype = (Int,Int)
type Rowcoltype = (Int,Int)
type Accmodtype = (String,String)
type Attributetype = (Int,String)
type Transformtype = (Int,Double,Double)
type Formatmasktype = ( Int,[String])
type Boxtype = ( Int,Int,Int,Int,[Xytype] ) -- Layer,Boxtype [Xy]
type Nodetype = ( Int, Int, Int ,Int, Xytype )
type Sreftype = ( String, Int, Int, Transformtype , Xytype )
type Areftype = ( String, Int,Int, Transformtype, Rowcoltype, Xytype )
type Pathtype = ( Int, Int, Int, Int, [Xytype], Int,Int )
type Texttype = ( Int,Int,Int,Int,Int,Int,Int,Transformtype,Xytype,String  )
type Boundarytype = (Int,Int, Int, Int , [Xytype])
type Structuretype = (String,[Int],Accmodtype,[Elempairtype])

-- Stream = (admd, sz, srf, ls, ln, rl, fn, at, gn, fm,un,stl) 
--   accmod = access and mod (String,String)
--   sz = library directory size (Calma only?) Integer
--   srf = stream file name, String
--   ls = Integer (Calma access control rights) libsecure
--   ln = library name , String
--   rl = reference lib, String
--   fn = fonts, String  Name of font file
--   at = attribute table file , String
--   gn = generations, Integer
--   fm = format (Int,[String]) = Formatmasktype
--   un = (db in units, db in meters ) (double,double)
--   stl = list of structures

type Streamtype = (Accmodtype,Int,String,Int,String,String,String,String,Int,
                       Formatmasktype, (Double,Double), [Structuretype] )

data Elementtype = Boundary Boundarytype  | Path Pathtype | Aref Areftype 
                        | Sref Sreftype
                        | Text Texttype | Node Nodetype | Box Boxtype

type Elempairtype = (Elementtype, [Attributetype])

showcolrow::(Int,Int)->String
showcolrow (a,b) = "\n  Column = " ++ (show a ) ++ " ; Row = " ++ (show b)

showtransbits::Int->String
showtransbits x = (showtrans15 x) ++ (showtrans2 x ) ++ (showtrans1 x)
         
showtrans15::Int->String
showtrans15 x | (x .&. 32768 ) > 0 = "Reflected "
              | otherwise = ""
             
showtrans2::Int ->String
showtrans2 x  | (x .&. 2 ) > 0 = "Abs Magnitude "
              | otherwise = ""

showtrans1::Int ->String
showtrans1 x  | (x .&. 1 ) > 0 = "Abs angle "
              | otherwise = ""

showpresentbits::Int->String
showpresentbits x = (presentbits01 x ) ++ (presentbits23 x) ++ " font " ++ (show (( x .&. 48 ) `div` 16 ))

presentbits01::Int->String
presentbits01 x | ((x .&. 3) == 0 ) = "left "
                | ((x .&. 3) == 1 ) = "center "
                | ((x .&. 3) == 2 ) = "right "
                | otherwise = " "

presentbits23::Int->String
presentbits23 x | ((x .&. 12) == 0 ) = "top "
                | ((x .&. 12) == 4 ) = "middle "
                | ((x .&. 12) == 8 ) = "bottom "
                | otherwise = " "

showxy::(Int,(Int,Int))->String
showxy (a,(b,c)) = "\n    X[" ++ (show a) ++ "]  value = " ++ (show b ) ++ "  Y[" ++ (show a) ++ "]  value = " ++ (show c)

showelem::Elementtype->String 
showelem (Boundary (d,e,a,b,c)) = "\nBoundary " ++ "\n  Layer = " ++ 
  (show a) ++ "\n  Datatype = " ++ (show b) ++ "\n  Elvalue = " ++ 
  (show d) ++ "\n  Plex = " ++ (show e) ++ "\n   XY data " ++ 
  (concat ( map ( showxy ) (zip [0..] c) )) 

showelem (Path (a,b,c,d,e,f,g)) = "\nPath" ++ "\n  Layer = " ++ 
   (show a ) ++ "\n  Width = " ++ (show d ) ++ 
   ( concat ( map ( showxy ) ( zip [0..] e ) ) )

showelem (Aref (a,g,h,(b,c,d),e,f)) = "\nAref"  ++ "\n  Name = " ++ 
  a  ++ "\n  Elvalue = " ++ (show e ) ++ "\n Plex " ++ (show f ) ++
  "\n  Mag = " ++ (show c ) ++ "\n  Angle = " ++ (show d ) ++ 
  "\n  Colrow" ++ (showcolrow e)

showelem (Sref (a,f,g,(b,c,d),e)) = "\nSref"  ++ "\n  Name = " ++ 
 (show a ) ++ "\n  Elvalue = " ++ (show f) ++ "\n  Plex = " ++ 
 (show g) ++ "\n  Transbits = " ++ (showtransbits b) ++ "\n  Mag = " ++
 (show c ) ++ "\n  Angle = " ++ (show d) ++  (showxy (0,e) ) 

showelem (Text (a,b,c,d,e,f,g,(h,m,an),i,j)) = "\nText" ++ "\n  Elvalue = "  ++
 (show a) ++ "\n  Plex = " ++ (show b) ++ "\n  Layer = " ++ (show c ) ++ 
 "\n  Texttype = " ++  (show d ) ++ "\n  Presenttype = " ++ 
 (showpresentbits  e) ++  "\n  Pathtype = " ++ (show f ) ++ "\n  Width= " ++
 (show g) ++  "\n  Transbits= " ++ (showtransbits h) ++ "\n  Mag = " ++
 (show m ) ++ "\n  Angle = " ++ (show an) ++ (showxy (0,i) ) ++
  "\n  String = " ++ j 

showelem (Node (d,e,a,b,c)) = "\nNode" ++ "\n  Elvalue = " ++ (show d) ++
   "\n  Plex = " ++  (show e) ++ "\n  Layer = " ++ (show a ) ++ 
  "\n  Nodetype = " ++ (show b ) ++ (showxy (0,c) )

showelem (Box (d,e,a,b,c)) = "\nBox" ++ "\n  Elvalue = " ++ (show d) ++
   "\n  Plex = " ++  (show e) ++ "\n  Layer = " ++ (show a ) ++
   "\n  Boxtype = " ++ (show b ) ++ 
   ( concat ( map ( showxy ) ( zip [0..] c ) ) )

showunits::(Double,Double)->String
showunits (a,b) = "\n  User units = " ++ (show a ) ++ "\n  Physical units = " ++ (show b)

showdates::(String,String)->String
showdates (a,b) = "\n  Modify date = " ++ a ++ "\n  Access date = " ++ b

showsdates::(String,String)->String
showsdates (a,b) = "\n  Create date = " ++ a ++ "\n  Modify date = " ++ b

showelempair::Elempairtype->String
showelempair (a,b) = showelem a ++ ( concat ( map (showattr) b ) )

showattr::Attributetype->String
showattr (a,b) = "\n  Attribute number = " ++ (show a ) ++ "\n  Attribute value = " ++ b

showmasks::[String]->String
showmasks [] = []
showmasks (x:xs) = "\n  Mask = " ++ x ++ showmasks xs

showformatmask::Formatmasktype->String
showformatmask (a,b) = "\n  Format = " ++ show a ++ showmasks b 

showstrclass::[Int]->String
showstrclass [] = []
showstrclass (x:xs) = "\n  Structure class = " ++ (show x) ++ showstrclass xs

showstruct::Structuretype->String
showstruct (a,d,b,c)= "\n Structure name = " ++ a ++ " " ++ showsdates b ++ " " ++ showstrclass d ++  concat ( map (showelempair ) c)  ++ "\nEnd structure"
  
showstream::Streamtype->String
showstream (admd, sz, srf, ls, ln, rl, fn, at, gn, fm,un,stl) = showdates admd ++
        "\n  Library size = " ++ (show sz ) ++ "\n  Sticks rule file = " ++ srf 
        ++ "\n  Lib name = " ++ ln ++ " " ++ "\n  Reflib ="  ++ rl ++ "\n  Font file = "        ++ 
        fn ++ " " ++ "\n  Attribute table file = " ++ at ++ "\n  Generations = " ++
        (show gn) ++  showformatmask fm  ++ (showunits un) ++ " " ++ concat ( map (showstruct ) stl) 
         ++ "\nEnd stream"

parsestreamfile:: Parser Streamtype
parsestreamfile = do {
                   parseheader;
                   admd <- parsebeginlib;
                     sz <- parseoptlibsize;
                    srf <- parseoptsrfname;
                     ls <- parseoptlibsecure;
                     ln <- parselibname;
                     rl <- parseoptreflibname;
                     fn <- parseoptfonts;
                     at <- parseoptattrtable;
                     gn <- parseoptgeneration;
                     fm <- parseoptformat;
                     un <- parseunits;
                    stl <- manyTill parsestructure ( try parseendlib );
                   return( (admd, sz, srf, ls, ln, rl, fn, at, gn, fm,un,stl) )
               }


parsestructure::Parser Structuretype
parsestructure = do {
                     admd   <- parsebgnstr;
                     s      <- parsestrname;
                     sc     <- parseoptstrclasslist;
                     ellist <- parseelementlist;
                     return( (s,sc,admd,ellist))
                     }
                 <?> " structure"

parseoptstrclasslist::Parser [Int]
parseoptstrclasslist = do { scl <- many parsestrclass ;
                            return ( scl)
                          }
                               
parsestrclass::Parser Int
parsestrclass = do { try (parsestrclasshead);
                     w <- hexwordsp;
                     return (hextoInt w)
                   }
               
                               
parsestrclasshead::Parser Int
parsestrclasshead = do { string byte2_header;
                     spaces;
                     string strclass;
                     spaces;
                     return 0
                   }
               
                        
parseelementlist:: Parser [Elempairtype]
parseelementlist = do {
                        ellist <- manyTill parseallelement ( try parseendstr );
                        return(ellist);
                       }

parseallelement::Parser Elempairtype
parseallelement  = do {
                         string nodata 
                        ; spaces
                        ; elp <- parseelement
                        ; return elp
                      }



parseelement::Parser Elempairtype
parseelement = do { try ( string boundary )
                   ; spaces
                   ; el <- parseboundary
                   ; atl <-  many parseattrlist
                   ; parseendel 
                   ; return ( (el,atl)  )  }
           <|> do { try ( string aref )
                   ; spaces 
                   ; el <- parsearef
                   ; atl <-  many parseattrlist 
                   ; parseendel
                   ; return (el,atl) }
           <|> do { try ( string sref )
                   ; spaces
                   ; el <- parsesref
                   ; atl <-  many  parseattrlist
                   ; parseendel 
                   ; return (el,atl) }        
           <|> do { try ( string path )
                   ; spaces
                   ; el <- parsepath
                   ; atl <-  many parseattrlist 
                   ; parseendel
                   ;  return (el,atl) }
           <|> do { try ( string text )
                   ; spaces
                   ; el <- parsetext
                   ; atl <-  many parseattrlist
                   ; parseendel 
                   ; return (el,atl) }     
           <|> do { try (  string node )
                   ; spaces
                   ; el <- parsenode
                   ; atl <-  many parseattrlist 
                   ; parseendel 
                   ; return (el,atl) }  
           <|> do { try ( string box ) 
                   ; spaces
                   ; el <- parsebox
                   ; atl <-  many parseattrlist 
                   ; parseendel
                   ; return (el,atl) } 
           <?> " boundary, path, sref, aref, text, node or box"

         
parseoptelflags:: Parser Int
parseoptelflags = do
                    {
                       try (parseelflaghead );
                       pt <- parseelflagrest;
                       return ( pt )
                    }
                <|> do
                   {
                      return( 0 )
                   }

parseelflaghead:: Parser Int
parseelflaghead = do { string byte2_header
                   ; spaces
                   ; string elflags
                   ; return (0 )
                }

parseelflagrest:: Parser Int
parseelflagrest = do {  spaces
                  ; a <- hexword 
                  ; spaces
                  ; return ( hextoInt a )
                }
              <?> " EL FLAG data "
                        
parseoptplex:: Parser Int
parseoptplex = do
                    {
                       try (parseplexhead );
                       pt <- parseplexrest;
                       return ( pt )
                    }
                <|> do
                   {
                      return( 0 )
                   }

parseplexhead:: Parser Int
parseplexhead = do { string byte4_header
                   ; spaces
                   ; string plex
                   ; return (0 )
                }

parseplexrest:: Parser Int
parseplexrest = do {  spaces
                  ; a <- hexword 
                  ; spaces
                  ; b <- hexword
                  ; spaces
                  ; return ( hex32toInt a b )
                }
              <?> " PLEX data "
                        
parseboundary::Parser Elementtype
parseboundary = do {
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                  l <- parselayer;
                  d <- parsedatatype;
                xyl <- parsexy;
--                  parseendel;
                  return( Boundary (el,pl,l,d,xyl));
                 }

parseendel::Parser Int
parseendel = do {
                  string nodata
                 ; spaces
                 ; string endel
                 ; spaces
                 ; return 0
                }
             <?> " ENDEL keyword"


parsetext::Parser Elementtype
parsetext = do {
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                  l <- parselayer;
                  t <- parsetexttype;
                prt <- parseoptpresenttype;
                 pt <- parseoptpathtype;
                 wi <- parseoptwidth;
                ptr <- parseoptstrans;
                 xy <- parseonexy;
                 st <- parsestring;
                 return( Text (el,pl,l,t,prt,pt,wi,ptr,xy,st))
                 }

parseoptpresenttype::Parser Int
parseoptpresenttype = do { try ( parsepresenthead );
                                 spaces;
                                 w <- hexword;
                                 spaces;
                                 return ( hextoInt w )
                         }
                     <|> do
                          {
                            return 0 
                          }

parseoptlibsize::Parser Int
parseoptlibsize = do { try ( parselibsizehead );
                                 spaces;
                                 w <- hexword;
                                 spaces;
                                 return ( hextoInt w )
                         }
                     <|> do
                          {
                            return 0 
                          }

parselibsizehead::Parser Int
parselibsizehead =  do { string byte2_header
                         ; spaces
                         ; string libdirsize
                         ; return 0
                        }

parseoptlibsecure::Parser Int
parseoptlibsecure = do { try ( parselibsecurehead );
                                 spaces;
                                 w <- hexword;
                                 spaces;
                                 return ( hextoInt w )
                         }
                     <|> do
                          {
                            return 0 
                          }

parselibsecurehead::Parser Int
parselibsecurehead =  do { string byte2_header
                         ; spaces
                         ; string libsecure
                         ; return 0
                        }

parsepresenthead::Parser Int
parsepresenthead =  do { string byte2_header
                         ; spaces
                         ; string presentation
                         ; return 0
                        }
                         

parsetexttype::Parser Int
parsetexttype = do { string byte2_header
                    ; spaces
                    ; string texttype
                    ; spaces
                    ; w <- hexword
                    ; spaces
                    ; return (hextoInt w )
                   }
                <?> " TEXTTYPE keyword "

parsenode::Parser Elementtype
parsenode = do { 
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                  l <- parselayer;
                  n <- parsenodetype;
                 xy <- parseonexy;
                  return( Node (el,pl,l,n,xy))
                 }

parsenodetype::Parser Int
parsenodetype = do { string byte2_header
                    ; spaces
                    ; string nodetype
                    ; spaces
                    ; w <- hexword
                    ; spaces
                    ; return (hextoInt w )
                   }
                <?> " NODETYPE keyword "

parsebox::Parser Elementtype
parsebox = do {    
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                  l <- parselayer;
                  b <- parseboxtype;
                 xyl <- parsexy;
                  if ((length xyl) < 5 ) then (fail "Box has fewer than 5 xy points" )
                  else return( Box (el,pl,l,b,xyl))
                 }

parseboxtype::Parser Int
parseboxtype = do { string byte2_header
                   ; spaces
                   ; string boxtype
                   ; spaces
                   ; w <-  hexword 
                   ; spaces
                   ; return ( hextoInt w )
                  }
                <?> " BOXTYPE keyword "

parseoptdatatype:: Parser Int
parseoptdatatype = do
                  {
                     try (parsedatatypehead );
                     dt <- parsedatatyperest;
                     return ( dt )
                  }
              <|> do
                 {
                    return( 0 )
                 }

parseoptpathtype:: Parser Int
parseoptpathtype = do
                    {
                       try (parsepathtypehead );
                       pt <- parsepathtyperest;
                       return ( pt )
                    }
                <|> do
                   {
                      return( 0 )
                   }
           

parseoptsrfname:: Parser String
parseoptsrfname = do
                    {
                       try (parsesrfnamehead )
                   }
               <|> do
                    {
                      return ""
                    }
                     

parsesrfnamehead::Parser String
parsesrfnamehead = do { w <- hexword
                 ; spaces
                 ; string srfname
                 ; spaces
                 ; wordl <- count (((hextoInt w) `div` 2 ) -2 ) hexwordsp
                 ; return ( concat ( map ( wordtoStr ) wordl ) )
               }


parseoptreflibname:: Parser String
parseoptreflibname = do
                    {
                       try (parsereflibnamehead )
                   }
               <|> do
                    {
                      return ""
                    }
  
           

parsereflibnamehead::Parser String
parsereflibnamehead = do { w <- hexword
                 ; spaces
                 ; string reflibs
                 ; spaces
                 ; wordl <- count (((hextoInt w) `div` 2 ) -2 ) hexwordsp
                 ; return ( concat ( map ( wordtoStr ) wordl ) )
               }
             
parseoptfonts:: Parser String
parseoptfonts = do
                    {
                       try (parsefonthead )
                   }
               <|> do
                    {
                      return ""
                    }


parsefonthead::Parser String
parsefonthead = do { w <- hexword
                 ; spaces
                 ; string fonts
                 ; spaces
                 ; wordl <- count (((hextoInt w) `div` 2 ) -2 ) hexwordsp
                 ; return ( concat ( map ( wordtoStr ) wordl ) )
               }
                     

parseoptendext:: Parser Int
parseoptendext = do
                    {
                       try (parseendexthead );
                       ; spaces
                       ; a <-  hexword
                       ; spaces
                       ; b <-  hexword
                       ; spaces
                       ; return ( hex32toInt a b )
                   }
               <|> do
                    {
                      return 0 
                    }
                     
parseendexthead::Parser Int
parseendexthead = do {
                      string byte4_header
                     ; spaces
                     ; string endextn
                     ; return 0 
                   }
           

parseoptbegext:: Parser Int
parseoptbegext = do
                    {
                       try (parsebegexthead );
                       ; spaces
                       ; a <-  hexword
                       ; spaces
                       ; b <-  hexword
                       ; spaces
                       ; return ( hex32toInt a b )
                   }
               <|> do
                    {
                      return 0 
                    }
                     
parsebegexthead::Parser Int
parsebegexthead = do {
                      string byte4_header
                     ; spaces
                     ; string bgnextn
                     ; return 0 
                   }
           
                  
parsepath::Parser Elementtype
parsepath = do {
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                 l  <- parselayer;
                 dt <- parsedatatype;  
                 pt <- parseoptpathtype;  -- optional              
                 w <- parseoptwidth;
                 be <- parseoptbegext;
                 ee <- parseoptendext;

                 xyl <- parsexy;
                 return( Path (l,dt,pt,w,xyl,el,pl) )
               }

parsewidth::Parser Int
parsewidth = do {  string byte4_header
                 ; spaces
                 ; string width
                 ; spaces
                 ; a <-  hexword
                 ; spaces
                 ; b <-  hexword
                 ; spaces
                 ; return ( hex32toInt a b )
               }

parseoptgeneration::Parser Int
parseoptgeneration = do {  try ( parsegenerationhead )
                     ; a <-  hexword
                     ; spaces
                     ; return ( hextoInt a )
                   }
               <|> do
                    {
                      return 0 
                    }


parsegenerationhead::Parser Int
parsegenerationhead = do {
                 ; string  byte2_header
                 ; spaces
                 ; string generations
                 ; spaces
                 ; return 0
               }
                

parseoptattrtable::Parser String
parseoptattrtable = do {  try ( parseattrtablehead )
                   }
               <|> do
                    {
                      return ([]) 
                    }


parseattrtablehead::Parser String
parseattrtablehead = do { w <- hexword
                 ; spaces
                 ; string attrtable
                 ; spaces
                 ; wordl <- count (((hextoInt w) `div` 2 ) -2 ) hexwordsp
                 ; return ( concat ( map ( wordtoStr ) wordl ) )
               }
             

parseoptformat::Parser Formatmasktype
parseoptformat = do {  try ( parseformathead )
                      ; do {  try (string archived )     
                             ; spaces          
                             ; return((0,[]))
                          }
                      <|> do { try (string filtered )  
                             ; spaces          
                             ; ml <- manyTill parsemask ( try parsemaskend)
                             ; return((1, ml))
                          }
                      <|> do { a <- hexword                     
                              ; spaces
                              ; return ( (hextoInt a, []))
                             }
                      }



parsemask::Parser String
parsemask = do { w <- hexword
                 ; spaces
                 ; string mask 
                 ; spaces
                 ; wordl <- count (((hextoInt w) `div` 2 ) -2 ) hexwordsp
                 ; return ( concat ( map ( wordtoStr ) wordl ) )
                      
               }


parsemaskhead::Parser Int
parsemaskhead = do {
                    try (string mask)
                    ; spaces
                    ; return 0
                   }
               
      
parsemaskend::Parser Int
parsemaskend = do {
                  string nodata
                 ; spaces
                 ; string endmasks
                 ; spaces
                 ; return 0
                }
     

parseformathead::Parser Int
parseformathead = do {
                  string  byte2_header
                 ; spaces
                 ; string format
                 ; spaces
                 ; return 0
               }
                

parseoptwidth::Parser Int
parseoptwidth = do {  try ( parsewidthhead )
                     ; a <-  hexword
                     ; spaces
                     ; b <-  hexword
                     ; spaces
                     ; return ( hex32toInt a b )
                   }
               <|> do
                    {
                      return 0 
                    }


parsewidthhead::Parser Int
parsewidthhead = do {
                 ; string  byte4_header
                 ; spaces
                 ; string width
                 ; spaces
                 ; return 0
               }
                

parsearef::Parser Elementtype
parsearef = do {
                 el <- parseoptelflags;
                 pl <- parseoptplex;
                 s <- parsesname;
                 tr <- parseoptstrans;
                 r <- parserows;
                 xy <- parseonexy;
                 return( Aref ( s,el, pl, tr ,r ,xy) )
                }


                
parserows::Parser (Int,Int)
parserows = do { try ( do {
                             string byte4_header
                              ; spaces
                              ; string colrow
                               ; spaces
                           }
                        )
                ; a <- hexword
                ; spaces
                ; b <-  hexword
                ; spaces 
                ; return( (hextoInt a, hextoInt b) )
               }
           <?> " COLROW keyword "

parsesref::Parser Elementtype
parsesref = do {
         
                el <- parseoptelflags;
                pl <- parseoptplex;
                 s <- parsesname;
                tr <- parseoptstrans;
                xy <- parseonexy;
                return( Sref( s, el, pl, tr, xy ) )
                }  
                         

parseoptstrans::Parser Transformtype
parseoptstrans = do { try ( parsestranshead);
                      spaces;
                      w <- hexword;
                      spaces;
                      m <- parseoptmag;
                      a <- parseoptangle;
                      return( ( hextoInt w, m , a) )
                     }
                 <|> return( 0, -1, -1 )
           
parsestranshead:: Parser Int
parsestranshead = do { string byte2_header ;
                       spaces;
                       string strans;
                       return ( 0 ) 
                     }

parsestrans::Parser Transformtype
parsestrans = do { z <- hexword
                 ; spaces
                 ;  string strans
                 ;  spaces
                 ; w <- hexword
                 ;  spaces
                 ; m <- parsemag
                 ; a <- parseangle
                 ; return ( ( hextoInt w,m,a ) )
             }
                  
parselayer:: Parser Int
parselayer = do { string byte2_header
                 ; spaces
                 ; parselayerkeyword
                 ; spaces
                 ; l <- hexword 
                 ; spaces
                 ; return ( hextoInt l )
                }
             <?> "  LAYER keyword "

parselayerkeyword:: Parser Int
parselayerkeyword = do { 
                         string layer;
                         return 0
                       }
                <?> "Missing layer keyword = 0x0D02 "

parseoptmag::Parser Double
parseoptmag = do {
                  try ( parsemaghead );
                  spaces;
                  a <- hexword;
                  spaces;
                  b <- hexword;
                  spaces;
                  c <- hexword;
                  spaces;
                  d <- hexword;
                  spaces;
                  return ( hexwordstoDouble a b c d )
                }

            <|> return( -1 )

parseoptangle::Parser Double
parseoptangle = do {
                  try ( parseanglehead );
                  spaces;
                  a <- hexword;
                  spaces;
                  b <- hexword;
                  spaces;
                  c <- hexword;
                  spaces;
                  d <- hexword;
                  spaces;
                  return ( hexwordstoDouble a b c d )
                }
            <|> return( -1 )

parsemaghead::Parser Int
parsemaghead = do 
                 {
                   string float_header;
                   spaces;
                   string mag;
                   return(0)
                  }
parseanglehead::Parser Int
parseanglehead = do 
                 {
                   string float_header;
                   spaces;
                   string angle;
                   return(0)
                  }
               
parsemag::Parser Double
parsemag = do {
                  string float_header
                 ; spaces
                 ; string mag
                 ; spaces
                 ; a <- hexword
                 ; spaces
                 ; b <- hexword
                 ; spaces
                 ; c <- hexword
                 ; spaces
                 ; d <- hexword
                 ; spaces
                 ; return( hexwordstoDouble a b c d )
                }
             <?> " MAG keyword "

parseangle::Parser Double
parseangle = do {
                  string float_header
                 ; spaces
                 ; string angle
                 ; spaces
                 ; a <- hexword
                 ; spaces
                 ; b <- hexword
                 ; spaces
                 ; c <- hexword
                 ; spaces
                 ; d <- hexword
                 ; spaces
                 ; return( hexwordstoDouble a b c d )
                }
             <?> "ANGLE keyword "

mantissa::Int->Int->Int->Int->Double
mantissa a b c d = (((((( (fromIntegral ((a .&. 255) )) * 65536) + (fromIntegral b ) ) * 65536) + (fromIntegral c )) * 65536) + (fromIntegral d )) / ( 2 ^ 56) 

mantissasing::Int->Int->Double
mantissasing a b  = ( (fromIntegral (a .&. 255)) * 65536) + (fromIntegral b )  

pow::Double->Double->Double
pow a b = a ** b 

exponenter::Int->Double
exponenter a | ( (a .&. 0x8000) > 0 ) = - ( 16 `pow` ( fromIntegral ((( a .&. 0x7F00 ) `div` 256 ) - 64 )  ) )
             | otherwise =                ( 16 `pow` ( fromIntegral ((( a .&. 0x7F00 ) `div` 256 ) - 64 )  ) )


exponentersing::Int->Double
exponentersing a | ( (a .&. 0x8000) > 0 ) = - fromIntegral ( 16 ^ ((( a .&. 0x7F00 ) `div` 256 ) - 64 )  )
             | otherwise =  fromIntegral ( 16 ^ ((( a .&. 0x7F00 ) `div` 256 ) - 64 ) )


hexwordstoDouble::String->String->String->String->Double
hexwordstoDouble a b c d = (exponenter (hextoInt a ) ) *    ( mantissa (hextoInt a) (hextoInt b) (hextoInt c) (hextoInt d ) )    -- for now

hexwordstoSingle::String->String->Double
hexwordstoSingle a b  = (exponentersing (hextoInt a ) ) *    ( mantissasing (hextoInt a) (hextoInt b)  )    -- for now


parseheader:: Parser Int
parseheader = do { string byte2_header
                 ; spaces
                 ; string header
                 ; spaces
                 ; t <- hexword
                 ; spaces
                 ; return( hextoInt t )
                }
             <?> "  HEADER keyword "

parsebgnstr:: Parser (String,String)
parsebgnstr = do { string date_data
                 ; spaces
                 ; string bgnstr
                 ; spaces
                 ; cd <- parsedate
                 ; md <- parsedate
                 ; return( ( cd,md) )
                }
             <?> "  BGNSTR keyword "


parsebeginlib:: Parser (String,String)
parsebeginlib = do { string date_data
                 ; spaces
                 ; string bgnlib
                 ; spaces
                 ; md <- parsedate
                 ; ad <- parsedate
                 ; return( (md,ad) )
                }
             <?> "  BGNLIB keyword "

parsedate:: Parser String
parsedate = do {  yy <- hexword
                ; spaces
                ; mo <- hexword
                ; spaces
                ; da <- hexword
                ; spaces
                ; ho <- hexword
                ; spaces
                ; mi <- hexword
                ; spaces
                ; se <- hexword
                ; spaces
                ; return ( todatestr yy mo da ho mi se )
               }
            
todatestr::String->String->String->String->String->String->String
todatestr a b c d e f = (show ( hextoInt a ))  ++ "/" ++ (show ( hextoInt b )) ++ "/"
                         ++ ( show ( hextoInt c )) ++ "  " ++ (show (hextoInt d ))
                          ++ ":" ++ (show (hextoInt e)) ++ ":"  ++ (show(hextoInt f))

parseunits:: Parser (Double,Double)
parseunits = do { string datareal8x2
                 ; spaces
                 ; string units
                 ; spaces
                 ; uu <- parseuserunit
                 ; um <- parsemeterunit
                 ; return( (uu,um) )
                }
             <?> "  BGNLIB keyword "

parseuserunit:: Parser Double
parseuserunit = do { a <-  hexword 
                     ; spaces
                     ; b <- hexword
                     ; spaces
                     ; c <- hexword
                     ; spaces
                     ; d <- hexword
                     ; spaces
                    ; return( hexwordstoDouble a b c d )
                   }

parsemeterunit:: Parser Double
parsemeterunit = do { a <-  hexword 
                     ; spaces
                     ; b <- hexword;
                     ; spaces
                     ; c <- hexword;
                     ; spaces
                     ; d <- hexword;
                     ; spaces
                    ; return( hexwordstoDouble a b c d )
                   }

hexwordsp::Parser  String
hexwordsp = do {
              w <-hexword;
              spaces;
              return w
            }


parseendlib:: Parser Int
parseendlib = do { string nodata
                    ; spaces
                    ; string endlib
--                    ; spaces
                    ; return 0 
                   }
              <?> " ENDLIB keyword"

parseendstr:: Parser Int
parseendstr = do { string nodata
                    ; spaces
                    ; string endstr
                    ; spaces
                    ; return 0 
                   }
             <?> " ENDSTR keyword "

parsestrname:: Parser String
parsestrname = do { w <- hexword
                    ; spaces
                    ; string strname
                    ; spaces
                    ; wordl <- count (((hextoInt w ) `div` 2 )-2) hexwordsp
                    ; return( concat (  map (wordtoStr ) wordl ) )
                   }
               <?> " STRNAME keyword "

parsesname:: Parser String
parsesname = do { w <- hexword 
                    ; spaces 
                    ; string sname
                    ; spaces
                    ; wordl <- count (((hextoInt w) `div` 2 )-2) hexwordsp
                    ; return( concat (  map (wordtoStr ) wordl ) )
                   }
               <?> " SNAME keyword "

parselibname:: Parser String
parselibname = do { w <-  hexword 
                     ; spaces
                     ; string libname
                     ; spaces
                     ; wordl <- count (((hextoInt w ) `div` 2 )-2) hexwordsp
                    ; return( concat (  map (wordtoStr ) wordl ) )
                   }
               <?> "  LIBNAME keyword "

parseonexyhead::Parser Int
parseonexyhead = do {
                  ; hexword
                  ; spaces
                  ; string xy
                  ; return (0)
                  }

parseonexy:: Parser Xytype
parseonexy = do { w <- hexword
                  ; spaces
                  ; string xy
                  ; spaces
                  ; a <- hexword
                  ; spaces
                  ; b <- hexword
                  ; spaces
                  ; c <- hexword
                  ; spaces
                  ; d <- hexword
                  ; spaces
                  ; return( (hex32toInt a b , hex32toInt  c d) )
                }
              <?> " XY keyword "

parseonexyrest:: Parser Xytype
parseonexyrest = do {
                    spaces
                  ; a <- hexword
                  ; spaces
                  ; b <- hexword
                  ; spaces
                  ; c <- hexword
                  ; spaces
                  ; d <- hexword
                  ; spaces
                  ; return( (hex32toInt a b , hex32toInt  c d) )
                }
              <?> " XY data  "

parsexy:: Parser [Xytype]
parsexy = do { w <-  hexword 
                  ; spaces
                  ; string xy
                  ; spaces
                  ; xyl <- count (((hextoInt w ) `div` 2 )-2) hexwordsp
                  ; return(  toxy  xyl )
                }
              <?> " XY keyword "

-- can't figure the technology for getting parsepropval to work
--
parseattrlist:: Parser Attributetype
parseattrlist = 
               do {
                       atn <- parseattrnum;
                       do {  pv <-  try( parsepropval );
                             return( ( atn, pv ) )
                          }
                      <|> do {
                             return( (atn,""))
                           }
                   }
            

     
                           
parseattrnum:: Parser Int
parseattrnum = do { try ( string byte2_header )
                  ; spaces
                  ; string propattr
                  ; spaces
                  ; x <- hexword   -- property number
                  ; spaces
                  ; return ( hextoInt x )
                 }
            

parsepropval:: Parser String
parsepropval = do {
                        w <- hexwordsp 
                        ; string propvalue
                        ; spaces 
                        ; xyl <- count ( ( (hextoInt w)  `div` 2 )-2) hexwordsp
                        ; return (  concat ( map ( wordtoStr )  xyl ) )
                    }
            


parsestring:: Parser String
parsestring = do { w <- hexword
                  ; spaces
                  ; string ascii_string
                  ; spaces
                  ; xyl <- count (((hextoInt w ) `div` 2 )-2) hexwordsp
                  ; return( concat ( map ( wordtoStr ) xyl ))
                }
              <?> " String keyword "

--parsexmirror:: Parser String
--parsexmirror = do { len <- hextoInt hexword 
--                ; string XMIRROR
--                ; xyl <- count ((len `div` 2 )-2) hexwordsp
--                ; return(  toxy  xyl )
--              }
--            <?> " XMIRROR keyword "

parsedatatype:: Parser Int
parsedatatype = do { string byte2_header
                  ; spaces
                  ; string datatype
                  ; spaces
                  ; a <- hexword 
                  ; spaces
                  ; return ( hextoInt a )
                }
              <?> " DATATYPE keyword "


parsedatatypehead:: Parser Int
parsedatatypehead = do { string byte2_header
                   ; spaces
                   ; string datatype
                   ; return (0 )
                }

parsedatatyperest:: Parser Int
parsedatatyperest = do {  spaces
                  ; a <- hexword 
                  ; spaces
                  ; return ( hextoInt a )
                }
              <?> " DATATYPE data "
   

parsepathtypehead:: Parser Int
parsepathtypehead = do { string byte2_header
                   ; spaces
                   ; string pathtype
                   ; return (0 )
                }

parsepathtyperest:: Parser Int
parsepathtyperest = do {  spaces
                  ; a <- hexword 
                  ; spaces 
                  ; return ( hextoInt a )
                }
              <?> " DATATYPE data "
   

toxy::[String]->[(Int,Int)]
toxy [] = []
toxy (w:x:y:z:xs) = [( hex32toInt w x , hex32toInt y z )] ++ toxy xs

hexchartoInt::Char->Int
hexchartoInt a | (a=='0') = 0
               | (a=='1') = 1
               | (a=='2') = 2
               | (a=='3') = 3
               | (a=='4') = 4
               | (a=='5') = 5
               | (a=='6') = 6
               | (a=='7') = 7
               | (a=='8') = 8
               | (a=='9') = 9
               | (a=='A') = 10
               | (a=='B') = 11
               | (a=='C') = 12
               | (a=='D') = 13
               | (a=='E') = 14
               | (a=='F') = 15
               | (a=='a') = 10
               | (a=='b') = 11
               | (a=='c') = 12
               | (a=='d') = 13
               | (a=='e') = 14
               | (a=='f') = 15
               | otherwise = 0


hextoInt::String->Int
hextoInt [] = 0
hextoInt x = hexchartoInt ( head (take 1 ( reverse x ) )) + (16 * ( hextoInt (reverse ( drop 1 ( reverse x ) ) ) ) )

hex32toInt::String->String->Int
hex32toInt a b  = (65536 * (hextoInt a)) + (hextoInt b )
                     
hextoChar::Char->Char->Char
hextoChar a b = toEnum (( 16 * ( hexchartoInt a )) + (hexchartoInt b))

wordtoStr::String->String
wordtoStr [] = []
wordtoStr (x:y:xs) = [ hextoChar x y ] ++ wordtoStr xs


unscramble::String->String
unscramble s | (length s < 4 ) = []
unscramble s | (length s > 4 ) = []
unscramble (a:b:c:d) = [ c] ++ (take 1 d) ++ [a] ++ [b]

hexword::Parser String
hexword = many1 hexDigit


run input 
  = case (runParser  parsestreamfile () "fname" input ) of 
      Right n  -> putStrLn ("Structure begin:  " ++ showstream n ++ " ")
      Left err -> do{ putStr "parse error at "
                    ; print ( err )
                    }
			
toupperWord::String->String
toupperWord s = map ( toUpper) s
		
reformat::String->String
reformat [] = []
reformat s = (take 40 s ) ++ ['\n'] ++ reformat ( drop 40 s )
					
--pr = print( "Result is " ++ show ( parse parsestructure "0004 0502 CCCC" ) )

testin =  "0006 0002 0258 001C 0102 07D5 0003 0011 000E 0014 0033 07D5 0003 0011 000E 0014 0033 0010 0206 6F77 6C76 6973 696F 6E2E 6462 0014 0305 3E41 8937 4BC6 A7F0 3944 B82F A09B 5A50 001C 0502 07D1 000B 000B 0004 002B 0022 07D4 0008 001F 000D 0027 0025 0008 0606 7669 6100 0004 0800 0006 0D02 0031 0006 0E02 0000 002C 1003 FFFF F830 FFFF F830 0000 07D0 FFFF F830 0000 07D0 0000 07D0 FFFF F830 0000 07D0 FFFF F830 FFFF F830 0004 1100 0004 0700 0004 0400 "

--main run testin
main = do {
            s <- getArgs;
            g <- readFile (head s );
            run  ( reformat ( unwords ( map ( unscramble . toupperWord ) ( words g ) ) ) )
           }



 




