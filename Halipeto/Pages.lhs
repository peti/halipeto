%
% Halipeto 2.0 -  Haskell static web page generator
% Copyright 2004 Andrew Cooke (andrew@acooke.org)
% Copyright 2007 Peter Simons (simons@cryp.to)
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

\section{Pages}

A website is a collection of related pages.  The code here fixes these
relationships, relating page names, directories and templates.

\begin{code}
module Halipeto.Pages (
  PageGen, PageGenS, idD, page, noPage, append, repeat, leafP, foldT,
  setSiteDetails, generate,
  menuClass, menuClass', Label, Collect, baseMenu, listMenu
) where

import Prelude hiding (repeat, all)
import Halipeto.Template
import Halipeto.Dictionary
import Halipeto.Utilities
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Types
import Maybe
import IO
import Monad
import System.Directory
\end{code}

First we extend the definition of the Page type defined in the
Template section.

\begin{code}
instance Show (Page s) where
  show p = (toSlash $ path p) ++ ": " ++ (template p)

instance Eq (Page s) where
  (==) a b = (==) (path a) (path b)

instance Show (TreeSite s) where
  show t = foldT (\p ts -> (show p) ++ ":" ++ (show ts)) "" t
\end{code}

This fold uses foldr to preserve the order of children (for some
reason I keep getting the order wrong when I try to use a foldl and
correct in the folded function).

%%Haddock: Fold over the TreeSite structure
\begin{code}
foldT :: (Maybe (Page s) -> a -> a) -> a -> TreeSite s -> a
foldT f a (TreeSite p ts) = f p (foldr (flip $ foldT f) a ts)
\end{code}

\subsection{Using the Database}

We can use use the information in Halipeto's SimpleDB database to help
define paes.  In particular, we can iterate over groups just as we do
in templates.  The aim is to support code like (copied from the
Demo code):

\begin{verbatim}
(page ["index.html"] "front-page.html" idD
  (repeat "locale" "locale-list"
    (page ["{locale}", "index.html"] "index-locale.html" idD
      (append
        (repeat "group" "group-list"
          (page ["{locale}", "{group}.html"] "index-group.html" idD
            (repeat "image" "groups.{group}.images"
              (page ["{locale}", "{image}.html"] "image.html" idD leafP))))
        (page ["{locale}", "order.html"] "order.html" idD leafP)))))
\end{verbatim}

That is applied to a suitable dictionary to generate site structure
like:

\begin{verbatim}
+- index
+- locale-1
:  +- group-1
   |  +- image-1
   |  +- image-2
   |  :
   +- group...
   :
   +- order
\end{verbatim}

So page associates a parent with children pages; repeat generates a
set of siblings (and modifies the dictionary), and append adds a page
to others at the same level.

Hopefully the parallels with iterating over values in a template (see,
for example, the ``How To'' section near the start of this
documentation) are clear.

Of course, you are also free to specify the site structure as you
like.  Simply construct the appropriate TreeSite instances.

One final feature --- these functions store changes to the state
dictionary with the site information.  These changes are re--applied
when the page is generated.  So templates can assume that the
variables used by repeat during the definition of the page structure
are available during page generation (they appear as children of
``hal.menu'' to avoid conflicts with simialr values in the template).

In the example above ``hal.menu.locale'' will be defined for all pages
except the initial index.

%%Haddock: The intermediate function used to construct a page
%%Haddock: within a hierarchy
\begin{code}
type PageGen s = s String -> s String -> TreeSite s
\end{code}
%%Haddock: The intermediate function used to construct a list
%%Haddock: of pages within a hierarchy
\begin{code}
type PageGenS s = s String -> s String -> [TreeSite s]
\end{code}

It is also possible to define additional additional values in the
state for a particular page.  IdD should be used when no extra values
are equired (see example above).

%%Haddock: Default function when no additional values are added
%%Haddock: to the state for this page.
\begin{code}
idD :: UpdateDict s
idD d _ = d
\end{code}

%%Haddock: Construct a page with its children
\begin{code}
page :: SubDictionary s =>
  [String] -> String -> UpdateDict s -> PageGenS s -> PageGen s
page pth tmpl upd ts dct dc0 =
    TreeSite (Just $ Page pth' tmpl' upd') (ts dct' dc0)
  where
    dct' = upd dct []
    pth' = subAll dct' pth
    tmpl' = substitute dct' tmpl
    (dif, _, _) = diff dct' dc0
    upd' d s = addAll d $ map (\(k, v) -> (s++k, v)) $ dif
\end{code}

LeafP is used in place of the list of child pages when the page is a
leaf node.
%%Haddock: Placeholder for when a page has no children
\begin{code}
leafP :: PageGenS s
leafP _ _ = []
\end{code}

A group of pages may exist in the hierarchy without a parent page.
%%Haddock: Group pages without a parent page
\begin{code}
noPage :: SubDictionary s => PageGenS s -> PageGen s
noPage ts dct dc0 = TreeSite Nothing (ts dct dc0)
\end{code}

%%Haddock: Append a page to a list of pages
\begin{code}
append :: PageGenS s -> PageGen s -> PageGenS s
append pg1 pg2 dct dc0 = (pg1 dct dc0) ++ [(pg2 dct dc0)]
\end{code}

%%Haddock: Iterate over the children of a node in the state dictionary
\begin{code}
repeat :: SubDictionary s => String -> String -> PageGen s -> PageGenS s
repeat to frm pg dct dc0 = repeat' to' frm (children' dct frm') pg dct dc0
  where
    [to', frm'] = subAll dct [to, frm]

repeat' :: SubDictionary s =>
  String -> String -> [s String] -> PageGen s -> PageGenS s
repeat' _  frm [] _  dct  _  = error $ "nothing to repeat for " ++ frm ++
                                          "\n" ++ (show $ contents dct)
repeat' to  _  ch pg dct dc0 = foldr f [] ch
  where
    f ch pgs = (pg (adopt' dct (to, ch)) dc0):pgs
\end{code}

\subsection{Standard State}

The following values are assumed to be present in the context state.
They can be provided by calling setSiteDetails.

\begin{tabular}{l|l}
key&value\\
\hline
hal.destination&Path to prepend to generated file names\\
hal.templates&Path to prepend to template names
\end{tabular}

\begin{code}
destination = [hal, "destination"]
templates = [hal, "templates"]

orBlank :: Maybe String -> String
orBlank = fromMaybe ""
\end{code}
%%Haddock: Define standard state
\begin{code}
setSiteDetails :: SubDictionary s =>
  s String -> String -> String -> s String
setSiteDetails dct dest tmpl = addAll dct [(destination, dest),
                                           (templates, tmpl)]
\end{code}

\subsection{Generation}

These functions generate the pages that were previously defined in the
context's TreeSite structure.

%%Haddock: Generate the pages described for the site
\begin{code}
generate :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> IO ()
generate ctx = do foldT (generate' ctx) (do return ()) (site ctx)

generate' :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Maybe (Page s) -> IO () -> IO ()
generate'  _  Nothing prv = prv
generate' ctx (Just pg) prv =
    do putStrLn $ "generating " ++ htmlPath ++ " from " ++ tmplPath
       tmpl <- readTemplate tmplPath
       html <- evalDocument ctx' tmpl
       checkDir htmlPath
       hOut <- openFile htmlPath WriteMode
       hPutStr hOut $ show (document html)
       hClose hOut
       prv
  where
    dct = updateState (state ctx) pg
    ctx' = ctx {state = dct}
    to = orBlank $ search dct destination
    htmlPath = toSlash $ [to] ++ path pg
    frm = orBlank $ search dct templates
    tmplPath = frm `slash` (template pg)

updateState :: (SubDictionary s) => s String -> Page s -> s String
updateState dct pg = addAll (dictionary pg dct [])
                       [rootPath pth,
                        pathPath pth,
                        ([hal, "template"], template pg)]
  where
    pth = path pg

-- the path to the current page
pathPath :: [String] -> ([String], String)
pathPath pth = ([hal, "path"], toSlash pth)

-- the path "back" to home
rootPath :: [String] -> ([String], String)
rootPath pth = ([hal, "root"], stepUp (depth pth - 1))

depth :: [String] -> Int
depth []       = 0
depth ("..":p) = depth p - 1
depth (".":p)  = depth p
depth ("":p)   = depth p
depth (_:p)    = 1 + depth p

stepUp :: Int -> String
stepUp 0 = "."
stepUp n = (stepUp $ n-1) ++ "/.."

checkDir :: FilePath -> IO String
checkDir pth = checkDir' (allButOne $ fromSlash pth)

checkDir' :: [String] -> IO String
checkDir' pth = foldl f (do return "") pth
  where
    f p s = do p' <- p
               p'' <- return $ p' `slash` s
               ok <- doesDirectoryExist p''
               if ok then return () else createDirectory p''
               return p''

allButOne :: [a] -> [a]
allButOne []     = [] -- or error
allButOne [_]    = []
allButOne (x:xs) = x:(allButOne xs)
\end{code}

\subsection{Menu}

Menus for sites can be pretty complex.  Here we provide some basic
infrastructure.  The functions below are modified using two functions.

Label selects pages and gives the HTML associated with each.

%%Haddock: Define a menu label for a page
\begin{code}
type Label s = s String -> Page s -> Maybe [Element]
\end{code}

Collect groups the HTML associated with pages.

%%Haddock: Combine menu labels
\begin{code}
type Collect = [Element] -> [Element] -> [Element]
\end{code}

\begin{code}
menuClass' = ("class", AttValue [Left "menu"])
menuClass = [menuClass']
\end{code}

Traverse the page structure collecting the menu labels for each page.

%%Haddock: Generate a menu
\begin{code}
baseMenu :: Collect -> TreeSite s -> s String -> Label s -> [Element]
baseMenu col ste dct lab = foldT (baseNode col dct lab) [] ste

baseNode :: Collect -> s String -> Label s
  -> Maybe (Page s) -> [Element] -> [Element]
baseNode  _   _   _  Nothing   rows = rows
baseNode col dct lab (Just pg) rows =
    case lab dct' pg of
      Nothing  -> rows
      Just els -> col els rows
  where
    dct' = (dictionary pg) dct [hal, "menu"]
\end{code}

ListMenu is a menu generating function that assumes the Label will
return a list of elements that should be encapsulated in a table row.
The generated menu is a table containing those rows.

%%Haddock: Generate a flat menu
\begin{code}
listMenu :: TreeSite s -> s String -> Label s -> Element
listMenu ste dct lab = Elem "table" menuClass (map CElem rows)
  where
    rows = baseMenu makeRow ste dct lab

makeRow :: Collect
makeRow els rows = (Elem "tr" menuClass
                     [CElem $ Elem "td" menuClass (map CElem els)]) : rows
\end{code}
