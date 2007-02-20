%  
% Halipeto 1.0 -  Haskell static web page generator 
% Copyright 2004 Andrew Cooke (andrew@acooke.org) 
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

\section{Dictionary}

This section provides a data structure to associate strings with
values (much like a hash table).  Key value pairs are added by
generating a new instance, reusing the old structure where
appropriate.

Since Dictionaries are purely functional data structures they provide
stack--like semantics on return (ie. if modified dictionaries are only
passed downwards then returning from a function ``pops'' the data that
was added within that function's scope).

The dictionary may be sensitive to the case of the keys or not (two
different implementations of a single class that share much underlying
code).  Since the class interfaces is general the same functions
manipulate either type of dictionary.  Functions oustide the class
interface are identified by appending ``NC'' to the case insensitive
version.

\begin{code}
module Dictionary (
  Dictionary, null, empty, emptyNC, toDot, fromDot,
  SubDictionary, OrdDictionary,
  add, add', addAll, addAll', search, search', keys, keys',
  contents, contents', values, children, children', adopt, adopt', merge,
  substitute, subAll, search'', diff, diff'
) where

import Prelude hiding (null)
import Char
import List hiding (find, null, partition, insert)
import Maybe
import Utilities
\end{code}

\subsection{Namespaces and Subsets}

Keys in the dictionary can be used to construct a hierarchical
namespace by following the convention that the null character
separates ``words''.

%%Haddock: The namespace separator
\begin{code}
null :: Char
null = chr 0
\end{code}

There's a slight ugliness in the code here, because the field
separators are stored in the same way as the fields themselves (as
text).  This could lead to confusing results if searches are made that
include null.

Since null is messy for the end user to manipulate (which is why the
problem above is not so serious in practice), two alternative
interfaces are provided to this hierarchical namespace.  One uses
``.'' to represent the separator in keys, the other represents keys as
a list of words.

%%Haddock: Convert a list of strings to a ``dot'' separated string
\begin{code}
toDot :: [String] -> String
toDot = toSep '.'
\end{code}
%%Haddock: Convert a ``dot'' separated string to a list of strings
\begin{code}
fromDot :: String -> [String]
fromDot = fromSep '.'
\end{code}

%%Haddock: Convert an array of strings to a null-separated string
\begin{code}
toNull :: [String] -> String
toNull = toSep null
\end{code}
%%Haddock: Convert a null separated string to a list of strings
\begin{code}
fromNull :: String -> [String]
fromNull = fromSep null
\end{code}

Functions in the class interface that use simple strings have a tick
(single quote) suffix (in general, the list based interfaces should be
used in Haskell; the string based interfaces give a simpler interface
to paths embedded in HTML templates).

\subsection{Class}

The general dictionary class.  This requires -fglasgow-exts for
compilation with ghc because more than one type parameter is present
(a is needed to make the SubDictionary class, as far as I can see).

%%Haddock: Associate string keys with values
\begin{code}
class Dictionary d a where
  add       :: d a -> ([String], a) -> d a   -- ^ Add a key-value pair
  add'      :: d a -> (String, a) -> d a     -- ^ Add .-format
  addAll    :: d a -> [([String], a)] -> d a -- ^ Add a key-value list
  addAll'   :: d a -> [(String, a)] -> d a   -- ^ Add list .-format
  search    :: d a -> [String] -> Maybe a    -- ^ Lookup a key
  search'   :: d a -> String -> Maybe a      -- ^ Lookup .-format
  keys      :: d a -> [[String]]             -- ^ All keys
  keys'     :: d a -> [String]               -- ^ All keys .-format
  contents  :: d a -> [([String], a)]        -- ^ All key-value pairs
  contents' :: d a -> [(String, a)]          -- ^ All pairs .-format
  values    :: d a -> [a]                    -- ^ All values
  children  :: d a -> [String] -> [d a]      -- ^ Sub-dictionaries
  children' :: d a -> String -> [d a]        -- ^ Children .-format
  adopt     :: d a -> ([String], d a) -> d a -- ^ Append sub-dictionary
  adopt'    :: d a -> (String, d a) -> d a   -- ^ Adopt .-format
  merge     :: d a -> d a -> d a             -- ^ Combine two dictionaries
\end{code}

\subsection{Case Sensitive}

This builds directly on the tree--based implementation described below.

\begin{code}
data DictCase a = DictCase (Dict a) (Maybe a)

instance (Show a) => Show (DictCase a) where
  show (DictCase d v) = "{" ++ show d ++ "," ++ show v ++ "}"
\end{code}
%%Haddock: An empty dictionary
\begin{code}
empty :: Dictionary DictCase a => DictCase a
empty = DictCase Empty Nothing

pack :: Unpacked a -> DictCase a
pack (d, v) = DictCase d v

unpack :: DictCase a -> Unpacked a
unpack (DictCase d v) = (d, v)

instance Dictionary DictCase a where
  add d (k, v)      = pack $ insert (unpack d) (toNull k, v)
  add' d (k, v)     = add d (fromDot k, v)
  addAll            = foldl add
  addAll'           = foldl add'
  search d k        = find (unpack d) (toNull k)
  search' d k       = search d (fromDot k)
  keys              = map fst . contents
  keys'             = map toDot . keys
  contents          = map (\(k, v) -> (fromNull k, v)) . contents'' . unpack
  contents'         = map (\(k, v) -> (toDot k, v)) . contents
  values            = map snd . contents
  children d k      = map pack $ children'' (unpack d) (toNull k)
  children' d k     = children d (fromDot k)
  adopt d1 (k, d2)  = pack $ adopt'' (unpack d1) (toNull k) (unpack d2)
  adopt' d1 (k, d2) = adopt d1 (fromDot k, d2)
  merge d1 d2       = pack $ merge'' (unpack d1) (unpack d2)
\end{code}

\subsection{Case Insensitive}

Again, this builds on the tree--based implementation described below.

Is there a better way to generalise this packing/unpacking to a common
form that occurs in the code above and below?

\begin{code}
data DictNoCase a = DictNoCase (Dict a) (Maybe a)

instance (Show a) => Show (DictNoCase a) where
  show (DictNoCase d v) = "{" ++ show d ++ "," ++ show v ++ "}"
\end{code}
%%Haddock: An empty case-insensitive dictionary
\begin{code}
emptyNC :: Dictionary DictNoCase a => DictNoCase a
emptyNC = DictNoCase Empty Nothing

packNC :: Unpacked a -> DictNoCase a
packNC (d, v) = DictNoCase d v

unpackNC :: DictNoCase a -> Unpacked a
unpackNC (DictNoCase d v) = (d, v)

uncase :: String -> String
uncase = map toLower

instance Dictionary DictNoCase a where
  add d (k, v)      = packNC $ insert (unpackNC d) (uncase $ toNull k, v)
  add' d (k, v)     = add d (fromDot k, v)
  addAll            = foldl add
  addAll'           = foldl add'
  search d k        = find (unpackNC d) (uncase $ toNull k)
  search' d k       = search d (fromDot k)
  keys              = map fst . contents
  keys'             = map toDot . keys
  contents          = map (\(k, v) -> (fromNull k, v)) . contents'' . unpackNC
  contents'         = map (\(k, v) -> (toDot k, v)) . contents
  values            = map snd . contents
  children d k      = map packNC $ children'' (unpackNC d) (uncase $ toNull k)
  children' d k     = children d (fromDot k)
  adopt d1 (k, d2)  = 
    packNC $ adopt'' (unpackNC d1) (uncase $ toNull k) (unpackNC d2)
  adopt' d1 (k, d2) = adopt d1 (fromDot k, d2)
  merge d1 d2       = packNC $ merge'' (unpackNC d1) (unpackNC d2)
\end{code}

\subsection{Partition}

Diff generates the differences between two dictionaries.  The first
list in the result contains entries in d1 that are not present in d2;
the second contains entries in d2 that are not present in d1; the
third entries comomn to both.

%%Haddock: A dictionary whose values can be ordered (and so sorted)
\begin{code}
class (Ord a, Dictionary d a) => OrdDictionary d a where
  diff      :: d a -> d a 
                 -> ([([String], a)], [([String], a)], [([String], a)])
                 -- ^ Partition into common and distinct values
  diff'     :: d a -> d a 
                 -> ([(String, a)], [(String, a)], [(String, a)])
                 -- ^ Partition into common and distinct values .-format

instance Ord a => OrdDictionary DictCase a where
  diff d1 d2  = mapT3 (map (\(k, v) -> (fromNull k, v))) $ 
                  diff'' (unpack d1) (unpack d2)
  diff' d1 d2 = mapT3 (map (\(k, v) -> (toDot k, v))) $ diff d1 d2

instance Ord a => OrdDictionary DictNoCase a where
  diff d1 d2  = mapT3 (map (\(k, v) -> (fromNull k, v))) $ 
                  diff'' (unpackNC d1) (unpackNC d2)
  diff' d1 d2 = mapT3 (map (\(k, v) -> (toDot k, v))) $ diff d1 d2

diff'' :: Ord a => Unpacked a -> Unpacked a 
  -> ([(String, a)], [(String, a)], [(String, a)])
diff'' d1 d2 = partition [] [] [] (contents'' d1) (contents'' d2)

partition :: Ord a => [(String, a)] -> [(String, a)] -> [(String, a)]
  -> [(String, a)] -> [(String, a)]
  -> ([(String, a)], [(String, a)], [(String, a)])
partition o1 o2 b [] [] = (o1, o2, b)
partition o1 o2 b d1 [] = (o1++d1, o2, b)
partition o1 o2 b [] d2 = (o1, o2++d2, b)
partition o1 o2 b d1'@((k1, v1):d1) d2'@((k2, v2):d2) =
    case compare k1 k2 of
      LT -> partition (o1++[(k1, v1)]) o2 b d1 d2'
      GT -> partition o1 (o2++[(k2, v2)]) b d1' d2
      EQ -> case compare v1 v2 of 
              LT -> partition (o1++[(k1, v1)]) o2 b d1 d2'
              GT -> partition o1 (o2++[(k2, v2)]) b d1' d2
              EQ -> partition o1 o2 (b++[(k1, v1)]) d1 d2
\end{code}

\subsection{Substitution}

A very simple ``language'' for substituting values from dictionaries
into strings simplifies several parts of Halipeto.  Clearly the
dictionary must return string values for this to work.

The syntax is simple: text within curly braces is taken as a path name
and substituted for the corresponding text.  If the key does not
correspond to any value it is left as literal text.

Braces can be nested (inner braces are necesarily evaluated first)
and can be escaped using the ``$\backslash$'' character (which itself
must be escaped if required as a literal).

For example, given the dictionary:
\begin{verbatim}
foo.bar = baz
foo.baz = hello
\end{verbatim} 
the string ``\{foo.\{foo.bar\}\} $\backslash$$\backslash$ world'' will
evaluate to ``hello $\backslash$ world''.

%%Haddock: A dictionary that supports recursive substitution
\begin{code}
class (OrdDictionary d String) => SubDictionary d where
  substitute     :: d String -> String -> String         -- ^ Replace keys
  substitute d s = unescape $ txt d s
  subAll         :: d String -> [String] -> [String]     -- ^ Replace on all
  subAll d       = map (substitute d)
  search''       :: d String -> [String] -> Maybe String -- ^ Replace & search
  search'' d s   = search d $ subAll d s

instance SubDictionary DictCase

instance SubDictionary DictNoCase
\end{code}

The following code processes the text from left to right (the mutually
recursive structure of the code, with no returns and multiple passes
along the string was a surprise --- I was intending to write a
traditional recursive descent parser, but I think this is different
--- and any comments would be welcome).

\begin{code}
txt, txt' :: (Dictionary d String) => d String -> String -> String
txt _ ""                = ""
txt d (c:s) | c == '\\' = c : (txt' d s)
            | c == '{'  = pth d [] "" s
            | otherwise = c : (txt d s)
txt' _ ""               = error "end of string during character escape"
txt' d (c:s)            = c : (txt d s)

pth, pth' :: (Dictionary d String) => 
  d String -> [String] -> String -> String -> String
pth _ _ _ "" = error $ "end of string during substitution\n" ++
                       " (probably missing '}')"
pth d l p (c:s) | c == '\\' = pth' d l (p++[c]) s
                | c == '.'  = pth d (l++[p]) "" s
                | c == '{'  = pth d l p (pth d [] "" s)
                | c == '}'  =
    case search d (l++[p]) of
--      Nothing -> error $ "cannot find translation for " ++ (toDot (l++[p]))
      Nothing -> (toDot $ l++[p]) ++ (txt d s)
      Just x  -> (txt d x) ++ (txt d s)
                | otherwise = pth d l (p++[c]) s
pth' d l p ""               = error "end of string during character escape"
pth' d l p (c:s)            = pth d l (p++[c]) s

unescape, unescape' :: String -> String
unescape ""                = ""
unescape (c:s) | c == '\\' = unescape' s
               | otherwise = c : (unescape s)
unescape' ""               = error "end of string during character escape"
unescape' (c:s)            = c : (unescape s)
\end{code}

\subsection{Unpacked}

The Unpacked type adds support to Dict for values associated with the
empty string.

\begin{code}
type Unpacked a = (Dict a, Maybe a)
\end{code}

\subsection{Access}

Insert and find are the two basic dictionary operations.

\begin{code}
insert :: Unpacked a -> (String, a) -> Unpacked a
insert (d, _) ("", x) = (d, Just x)
insert (d, v) sx      = (copy f d sx, v)
  where
    f n v = n {value = Just v}

find :: Unpacked a -> String -> Maybe a
find (d, v) "" = v
find (d, _) s  = apply f d s
  where
    f Empty = Nothing
    f n     = value n
\end{code}

The keys available in a dictionary can be listed with keys''.

\begin{code}
contents'' :: Unpacked a -> [(String, a)]
contents'' (d, Just x)  = ("", x) : (contents'' (d, Nothing))
contents'' (d, Nothing) = map rev $ foldD f (\s -> []) d ""
  where
    f c (Just v) l m r s = [(c:s, v)] ++ (l s) ++ (m $ c:s) ++ (r s)
    f c Nothing  l m r s = (l s) ++ (m $ c:s) ++ (r s)
    rev (s, x) = (reverse s, x)
\end{code}

Merge adds values from the second dictionary into the first (I suspect
there's a more efficient version of this that works on nodes
directly).

\begin{code}
merge'' :: Unpacked a -> Unpacked a -> Unpacked a
merge'' d1 d2 = foldl insert d1 (contents'' d2)
\end{code}

\subsection{Subtrees}

The children and adopt functions make explicit use of this namespace.

Children returns a labelled forest under a given key.  For example, if
a dictionary included
\begin{verbatim}
mytext.1.a = "A1"
mytext.2.a = "A2"
mytext.2.b = "B2"
\end{verbatim}

then the children of mytext would be two dictionaries, associated with
the lablels ``1'' and ``2'':
\begin{verbatim}
1: a = "A1"
2: a = "A2"; b = "B2"
\end{verbatim}

The labels are sorted so that they will be correctly ordered when they
are integer values, as above.

In addition, empty keys are handled correctly (not consistently,
necesarily, but in a way that is intuitively correct when iterating
over values in a template).  So, in the example above, if
\begin{verbatim}
mytext.3 = "3 with no key"
\end{verbatim}

then the forest would also contain a dictionary with label ``3'' that
associates the empty string with the value ``3 with no key''.

A sub--tree can be re--inserted into the dictionary at a different
path using adopt.  This provides a natural way of iterating over data
in the dictionary (note that a level of hierarchy --- the values that
are returned as lables by children --- is removed by adopting a
child).

The SU type is a wrapper for (String, Unpacked a) that allows sorting
via the Ord class.

\begin{code}
children'' :: Unpacked a -> String -> [Unpacked a]
children'' (d, Just x) "" = 
  combine $ (SU "" (Empty, Just x)):(sort $ subTree d [null])
children'' (d, x) s  = combine . sort $ subTree d (s ++ [null])

subTree :: Dict a -> String -> [SU a]
subTree = apply f
  where
    f Empty = []
    f n     = collect (match n)
\end{code}

Collect should gather each key and the associated subtree.  So it
should collect each node below a null.

\begin{code}
collect :: Dict a -> [(SU a)]
collect d = foldD' f g d ("", Nothing)
  where
    f n c v l m r pre'@(pre, vp) | c == null = 
      (l pre') ++ [SU (reverse pre) (match n, vp)] ++ (r pre')
                                 | otherwise =
      (l pre') ++ (m $ (c:pre, v)) ++ (r pre')
    g (pre, Just vp) = [SU (reverse pre) (Empty, Just vp)]
    g (pre, Nothing) = []

data SU a = SU String (Unpacked a)

instance Ord (SU a) where
  compare (SU s1 _) (SU s2 _) = compare' s1 s2 EQ

instance Eq (SU a) where
  (==) a b = EQ == compare a b

compare' :: String -> String -> Ordering -> Ordering
compare' []     []     def = def
compare' _      []     def = GT
compare' []      _     def = LT
compare' (a:as) (b:bs) EQ  = compare' as bs (compare a b)
compare' (_:as) (_:bs) def = compare' as bs def

combine :: [SU a] -> [Unpacked a]
combine l = map (\(SU s x) -> x) $ foldr f [] l
  where
    f x                     []                                        = [x]
    f a@(SU sa (da, Just xa)) (b@(SU sb (db, Just xb)):bs)            = a:b:bs
    f a@(SU sa (_, Just xa))  (b@(SU sb (db, _)):bs)       | sa == sb =
      (SU sa (db, Just xa)):bs
    f a@(SU sa (da, _))       (b@(SU sb (_, Just xb)):bs)  | sa == sb =
      (SU sa (da, Just xb)):bs
    f a@(SU sa _)             (b@(SU sb _):bs)                        = a:b:bs

adopt'' :: Unpacked a -> String -> Unpacked a -> Unpacked a
adopt'' d      s (d', Just x)  = adopt'' (insert d (s, x)) s (d', Nothing)
adopt'' (d, x) s (d', Nothing) = (copy f d (s ++ [null], d'), x)
  where
    f n1 n2 = n1 {match = fst $ merge'' (match n1, Nothing) (n2, Nothing)}
\end{code}

\subsection{Tree Implementation}

The underlying implementation for all this is a simple ternary tree.
It's not very efficient, but will do for now.  Here we define the
structure and basic folds.  The second fold includes the node itself,
which saves us from having to reconstruct sub-trees (an efficiency
hack).

\begin{code}
data Dict a = Node {char  :: Char,
                    value :: Maybe a,
                    left  :: Dict a, 
                    match :: Dict a, 
                    right :: Dict a}
            | Empty

foldD :: (Char -> Maybe b -> a -> a -> a -> a) -> a -> Dict b -> a
foldD _ a Empty            = a
foldD f a (Node c v l m r) = f c v (foldD f a l) (foldD f a m) (foldD f a r)

foldD' :: (Dict b -> Char -> Maybe b -> a -> a -> a -> a) -> a -> Dict b -> a
foldD' _ a Empty              = a
foldD' f a n@(Node c v l m r) = f n c v (foldD' f a l) 
                                  (foldD' f a m) (foldD' f a r)

instance (Show a) => Show (Dict a) where
  show x = showDict x

showDict Empty = "-"
showDict (Node c v l m r) = "[" ++ [c] ++ ":" ++ (show v) ++ "," ++
                              (show l) ++ "," ++ (show m) ++ "," ++ 
                              (show r) ++ "]"
\end{code}

The support functions copy and apply descend the tree to the point
specified by the string and then apply a function.  On the way down,
copy copies the tree.

\begin{code}
apply :: (Dict a -> b) -> Dict a -> String -> b
apply fn = foldD' f (\s -> fn Empty)
  where
    f n c _ l m r s'@[s]    | s == c = fn n
                            | s < c  = l s'
                            | s > c  = r s'
    f n c _ l m r s'@(s:ss) | s == c = m ss
                            | s < c  = l s'
                            | s > c  = r s'

copy :: (Dict a -> b -> Dict a) -> Dict a -> (String, b) -> Dict a
copy fn = foldD' f g
  where
    f n c v l m r a@((s:ss), v') | s == c && ss == [] = fn n v'
                                 | s == c             = n {match = m (ss, v')}
                                 | s < c              = n {left = l a}
                                 | s > c              = n {right = r a}
    g ((s:ss), v') | ss == []  = fn (Node s Nothing Empty Empty Empty) v'
                   | otherwise = Node s Nothing Empty (g (ss, v')) Empty
\end{code}
