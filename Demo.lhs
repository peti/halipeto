%
% Halipeto -- Haskell static web page generator
% Copyright 2004 Andrew Cooke (andrew@acooke.org)
% Copyright 2007-2010 Peter Simons (simons@cryp.to)
%
%     This program is free software; you can redistribute it and/or modify
%     it under the terms of the GNU General Public License as published by
%     the Free Software Foundation; either version 2 of the License, or
%     (at your option) any later version.
%
%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%     GNU General Public License for more details.
%
%     You should have received a copy of the GNU General Public License
%     along with this program; if not, write to the Free Software
%     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
% EXCEPT
%
% Files in FromHaxml are from HaXml - http://www.cs.york.ac.uk/HaXml -
% see the COPYRIGHT and LICENSE in that directory.  The files included
% are a subset of the full HaXml distribution and have been modified to
% originate from the FromHaxml module (so that install on Win32 is
% easy).
%

\section{Demonstration}

This section shows the basic code I used to generate a website
displaying some images generated with Pancito.  The site is bilingual
and has a navigation tree displayed in the margin of each page.

\subsection{Database}

For the database I decided on the following structure:

\begin{verbatim}
database/
+- default.haldx (fixed values for the site)
+- group-list.haldx
|  +- 1 = name of group 1
|  |  :
+- locales.haldx
|  +- 1 = intnleng
|  +- 2 = chile
+- tradn/ (translation of common words, titles)
|  +- intnleng.haldx
|  |  +- title = title
|  |  :
|  +- chile.haldx
|     :
+- groups/
|  +- <group1>/
|  |  +- images.haldx
|  |  |  +- 1 = path name of first image in group (name1)
|  |  |  :
|  |  +- text/
|  |     +- intnleng
|  |     |  +- body.hals
|  |     |  :
|  |     +- chile
|  :        :
|  +- <groupn>/
|     :
+- images/
   +- <name1>/
   |  +- details.haldx (language independent)
   |  |  +- title = ...
   |  |  +- src = ...
   |  |  :
   |  +- intnleng/
   |  |  +- details.haldx (language dependent)
   |  |  |  +- process = ...
   |  |  |  :
   |  |  +- body.hals
   |  |  +- products.haldx
   |  |  |  +- 1.type = print
   |  |  |  +- 1.details = 6" x 6" signed...
   |  |  |  +- 1.price = $70 USD (inc p+p)
   |  |  |  +- 2.type = ...
   |  |  |  :x
   |  |  : (other text as appropriate)
   |  +- chile/
   |        + body.hals
   :        :
   +- <namen>/
      :
\end{verbatim}

In designing this, I made sure that data is either ordered by integer
path elements (as in groups.haldx), or an index file that defines the
ordering of the path elements exists (groups.*.name and locale.*).

\subsection{Site Structure}

The site itself has the following structure:

\begin{verbatim}
pancito/
+- index.html (simple bilingual page with basic links)
+- intnleng/
|  +- index.html (english language intro/background)
|  +- order.html (english language ordering info)
|  +- <group1>.html
|  :
|  +- <name1>.html
|  :
+- chile/
|  +- index.html (chilean/spanish into/background)
|  :
+- images/
   +- <name1>.png
   :
\end{verbatim}

Group pages show thumbnails/details of the pictures in the group.  All
files are in a single directory for one locale - this imposes a
certain restriction on file names, but makes upload simpler (there
aren't so many images that name conflicts will be a serious problem).

\subsection{Margin Menu}

The structure of the navigation menu depends on the page.

\subsubsection{Front page}

\begin{verbatim}
[pancito] (front page)
+- english (intro)
+- castellano
\end{verbatim}

\subsubsection{English, Intro}

\begin{verbatim}
pancito
+- [english] (intro)
|  +- group1
|  :
|  +- groupn
|  +- order
+- castellano
\end{verbatim}

\subsubsection{English, Group}

\begin{verbatim}
pancito
+- english
|  +- [group1]
|  |  +- image1
|  |  :
|  :  +- imagen
|  +- groupn
|  +- order
+- castellano
\end{verbatim}

\subsubsection{English, Image}

Note that the direct translation is available.

\begin{verbatim}
pancito
+- english
|  +- group1
|  |  +- [image1]
|  |  :
|  :  +- imagen
|  +- groupn
|  +- order
+- castellano
   +- group1
      +- image1
\end{verbatim}

\subsection{Generating the Site}

Given the above, we first construct the site structure.

\begin{code}
import Prelude hiding (repeat, all)
import Utilities
import SimpleDB
import Dictionary
import Text.XML.HaXml.Types
import Template
import Functions hiding (repeat)
import Pages
import Maybe

import System.IO.Unsafe
\end{code}

%%Haddock: Define the site structure
\begin{code}
mkStruct :: SubDictionary s => PageGen s
mkStruct =
    (page ["index.html"] "front-page.html" idD
      (repeat "locale" "locale-list"
        (page ["{locale}", "index.html"] "index-locale.html" idD
          (append
            (repeat "group" "group-list"
              (page ["{locale}", "{group}.html"] "index-group.html" idD
                (repeat "image" "groups.{group}.images"
                  (page ["{locale}", "{image}.html"] "image.html" idD leafP))))
            (page ["{locale}", "order.html"] "order.html" idD leafP)))))
\end{code}

We use a case insensitive dictionary (emptyNC) for the functions
because case is insignificant in XML attribute names (I believe).

\begin{code}
main :: IO ()
main = buildPancitoSite "./demo/database" "./demo/pancito" "./demo/templates"


buildPancitoSite :: FilePath -> FilePath -> FilePath -> IO ()
buildPancitoSite db html tmpl =
    do dct <- readDB' noCVS (setSiteDetails empty html tmpl) db
       ste <- return $ mkStruct dct dct
       fns <- return $ addAll' (addDefaultsFn emptyNC)
                               [("pan.menu", menu),
                                ("pan.menulink", menuLink)]
       ctx <- return $ Ctx (groupContents dct) fns ste
       generate ctx
\end{code}

\subsection{Group Contents}

For the group index page I want a table of image sections.  Each
column in the table is a different group; in each column are samples
from each image.

Tables in HTML are constructed row by row, but this table can only be
generated directly from the state dictionary by column.  There may be
a devastatingly beautiful solution to this problem that involves
re-arranging the tree data in the state, but I can't see it (the main
problem is that some columns may be empty on a particular row, and
they aren't necessarily the later columns).

Instead, it seems simpler to construct a table that we can iterate over
using the normal constructs.  The following code constructs
\begin{verbatim}table.row.column.[image,group]\end{verbatim}
where image is either a valid image name or "none", row and column
index the complete table, and [...] encloses alternative child nodes.

We could associate this function with an HTML attribute, but it's
simpler to call it at the start of the program that generates the
site.

\begin{code}
groupContents :: SubDictionary s => s String -> s String
groupContents dct =
    pad $ foldl byGroup (dct, 1, 0) $ children' dct "group-list"

byGroup :: SubDictionary s =>
  (s String, Int, Int) -> s String -> (s String, Int, Int)
byGroup (dct, col, mx) chld = (dct', col+1, mx'')
  where
    (dct', mx') = foldl (byImage grp col) (dct, 1) $ children dct pth
    mx'' = max mx mx'
    grp = fromJust' "missing child group" $ search' chld ""
    pth = ["groups", grp, "images"]

byImage :: SubDictionary s =>
  String -> Int -> (s String, Int) -> s String -> (s String, Int)
byImage grp col (dct, row) chld = (dct', row+1)
  where
    dct' = addAll dct [(pth ++ ["group"], grp),
                       (pth ++ ["image"], img)]
    pth = ["table", show row, show col]
    img = fromJust' "missing child image" $ search' chld ""

pad :: SubDictionary s => (s String, Int, Int) -> s String
pad (dct, mxcol, mxrow) =
    foldl pad' dct [(row, col) | row <- [1..(mxrow-1)], col <- [1..(mxcol-1)]]

pad' :: SubDictionary s => s String -> (Int, Int) -> s String
pad' dct (row, col) = case search dct pth of
                        Just _  -> dct
                        Nothing -> add dct (pth, "none")
  where
    pth = ["table", show row, show col, "image"]
\end{code}

\subsection{Menu Code}

Since the menu includes links inside pages we don't use the basic
listMenu from Pages.  Instead, we do the row construction within the
Label implementation (so that a page can contrinute more than one row
to the menu table).

We could generate the menu from the page structure in the context, but
in this case it makes more sense to use the information in the
dictionary (the same information used to construct the page structure
above).  This duplication of work is necessary because we want to (1)
fix the order of the pages and (2) include quite fancy logic to
``prune'' the menu tree.

\begin{code}
tableMenu :: TreeSite s -> s String -> Label s -> Element
tableMenu ste dct lab = Elem "table" menuClass (map CElem rows)
  where
    rows = baseMenu (++) ste dct lab

mkRow :: [Content] -> [Content] -> [Element]
mkRow ims cnt = [Elem "tr" menuClass
                  [CElem $ Elem "td" []
                    [CElem $ Elem "table" []
                      [CElem $ Elem "tr" []
                        [CElem $ Elem "td" menuClass ims,
                         CElem $ Elem "td" menuClass cnt]]]]]

menu :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
menu ctx arg =
    do return $ (ctx, Xml Before [tableMenu ste dct label])
  where
    ste = site ctx
    dct = state ctx

nbsp = CRef (RefChar "nbsp")

data PageType = FP | LOC | GRP | ORD | IMG | X deriving (Eq)

whichTempl :: String -> PageType
whichTempl "front-page.html"   = FP
whichTempl "index-locale.html" = LOC
whichTempl "index-group.html"  = GRP
whichTempl "order.html"        = ORD
whichTempl "image.html"        = IMG
whichTempl _                   = error "unexpected template"

fromJust' :: String -> Maybe a -> a
fromJust' msg (Just val) = val
fromJust' msg Nothing    = error msg

label :: SubDictionary s => Label s
label dct pg =
    case whichTempl $ template pg of

      FP ->
        Nothing

      LOC ->
        if typ == LOC && lcl == Just lcl'
          then
            Just $ mkRow (sqr 1) [CString True txt]
          else
            Just $ mkRow (aro 1)
                     [CElem $ Elem "a" [menuClass',
                                        ("href", AttValue [Left lnk])]
                                       [CString True txt]]
          where
            txt = fromJust' "missing locale"
                    $ search dct ["locales", lcl', "tradn", lcl']

      GRP ->
        if typ == GRP && lcl == Just lcl' && grp == Just grp'
          then
            Just $ mkRow (sqr 2) [CString True grp'']
          else if lcl == Just lcl' || (grp == Just grp' && typ == IMG)
                 then Just $ mkRow (aro 2)
                               [CElem $ Elem "a"
                                             [menuClass',
                                              ("href", AttValue [Left lnk])]
                                             [CString True grp'']]
                 else Nothing

      IMG ->
        if typ == IMG && lcl == Just lcl' &&
             grp == Just grp'&& img == Just img'
          then Just $ (mkRow (sqr 3) [CString True txt]) ++ imageMenu
          else if lcl == Just lcl' && grp == Just grp'
                 then Just $ mkRow (aro 3)
                               [CElem $ Elem "a"
                                             [menuClass',
                                              ("href", AttValue [Left lnk])]
                                             [CString True txt]]
                 else if grp == Just grp' && img == Just img'
                        then Just $ mkRow (aro 3)
                               [CElem $ Elem "a"
                                             [menuClass',
                                              ("href", AttValue [Left lnk])]
                                             [CString True txt]]
                        else Nothing
          where
            txt = fromJust' "missing title"
                    $ search dct ["images", img', "details", "title"]

      ORD ->
        if typ == ORD && lcl == Just lcl'
          then Just $ (mkRow (sqr 2) [CString True txt]) ++ orderMenu
          else if lcl == Just lcl'
            then Just $ mkRow (aro 2)
                          [CElem $ Elem "a" [menuClass',
                                             ("href", AttValue [Left lnk])]
                                            [CString True txt]]
            else Nothing
          where
            txt = fromJust' "missing order"
                    $ search dct ["locales", lcl', "tradn", "order"]

  where
    typ = whichTempl . fromJust' "missing template"
                         $ search dct [hal, "template"]
    lcl = search dct ["locale"]
    lcl' = fromJust' "missing menu locale"
             $ search dct [hal, "menu", "locale"]
    grp = search dct ["group"]
    grp' = fromJust' "missing menu group"
             $ search dct [hal, "menu", "group"]
    grp'' = fromJust' ("missing menu name: " ++ grp')
              $ search dct ["groups", "names", grp']
    img = search dct ["image"]
    img' = fromJust' "missing menu image"
             $ search dct [hal, "menu", "image"]
    lnk = (fromJust' "missing root" $ search dct [hal, "root"])
            `slash` (toSlash $ path pg)
    aro n = imgList "arrow.png" ">" dct n
    sqr n = imgList "square.png" "." dct n
    orderMenu = map mkRow' cols
      where
        cols = map (fromJust' "missing child" . flip search [""])
                   (children dct ["order-list"])
        mkRow' key = Elem "tr" menuClass
                       [CElem $ Elem "td" menuClass
                         [CElem $ Elem "a"
                            [menuClass',
                             ("href", AttValue [Left $ "#" ++ key])]
                            ((aro 3) ++ [nbsp] ++
                             [CString True
                               (substitute dct $ "{locales.{locale}.tradn."
                                                 ++ key ++ "}")])]]
    imageMenu = mkRow (aro 4) $ map CElem (parseElements $
        "<a hal:attribute='href #discussion'"++
           "hal:textAfter='{locales.{locale}.tradn.discussion}'>"++
         "</a>")


imgList :: SubDictionary s => String -> String -> s String -> Int -> [Content]
imgList name alt dct 0 = [nbsp]
imgList name alt dct n =
    (CElem $ Elem "img"
       [menuClass',
        ("src",
         AttValue [Left (substitute dct $
                          "{hal.root}/{default.imgdir}/" ++ name)]),
        ("alt", AttValue [Left alt])
       ] [])
    :(imgList name alt dct (n-1))
\end{code}

\begin{code}
menuLink :: CustomFn s f
menuLink =
    mkElements Before $
      "<p class='menulink'>"++
        "<a hal:attribute='href #menu'"++
           "hal:text='{locales.{locale}.tradn.menu}'>"++
           "&nbsp;"++
           "<img hal:attribute='src {hal.root}/{default.imgdir}/worra.png'" ++
               " alt='&lt;'/>"++
        "</a>"++
      "</p>"++
      "<div class='hr'><hr/></div>"
\end{code}

That's it.  All the rest of the site is defined by the database
contents and the templates (in the demo/database and dem/templates
directories).  Adding another image, group or language, even, would
require modifications to the database (new directories and files, and
additional entries in existing files), but should not require any
changes to the code above.
