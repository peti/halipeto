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
\section{Utilities}

Various general routines.

\begin{code}
module Utilities (
  slash, toSlash, fromSlash, toSep, fromSep, dropSpace,
  mapT2, mapT3,
  thenMaybe
) where

import Char
\end{code}

Concatenation of paths and files.

\begin{code}
slash' = '/'
\end{code}
%%Haddock: Concatenate two directories
\begin{code}
slash :: String -> String -> String
slash p f = if null f || head f == slash' || null p || last p == slash'
              then p ++ f
              else p ++ [slash'] ++ f
\end{code}
%%Haddock: Convert a list of directories to a file path
\begin{code}
toSlash :: [String] -> FilePath
toSlash = foldl slash ""
\end{code}
%%Haddock: Convert a file path to a list of directories
\begin{code}
fromSlash :: FilePath -> [String]
fromSlash = fromSep slash'
\end{code}

Separation and expansion of strings.

%%Haddock: Join a list with a given separator character
\begin{code}
toSep :: Char -> [String] -> String
toSep sep [s]    = s
toSep sep (s:ss) = s ++ [sep] ++ toSep sep ss
\end{code}
%%Haddock: Split a list on a given separator character
\begin{code}
fromSep :: Char -> String -> [String]
fromSep sep s = uncurry (:) $ foldr f ("", []) s
  where
    f c (s, l) | c == sep && l == [] && s == "" = ("", l)
               | c == sep                       = ("", s:l)
               | otherwise                      = (c:s, l)
\end{code}

Remove space from the start of a string.

%%Haddock: Drop leading spaces
\begin{code}
dropSpace :: String -> String
dropSpace ""                = ""
dropSpace (c:s) | isSpace c = dropSpace s
                | otherwise = (c:s)
\end{code}

Maps over uniform tuples.

%%Haddock: Map over a uniform tuple
\begin{code}
mapT2 :: (a -> b) -> (a, a) -> (b, b)
mapT2 f (x1, x2) = (f x1, f x2)
\end{code}
%%Haddock: Map over a uniform triple
\begin{code}
mapT3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapT3 f (x1, x2, x3) = (f x1, f x2, f x3)
\end{code}

Chain functions that return Maybe.

%%Haddock: Chain Maybe functions
\begin{code}
thenMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
thenMaybe f1 f2 a = case f1 a of
                     Nothing -> Nothing
                     Just a' -> f2 a'
\end{code}
