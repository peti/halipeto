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

\section{Simple Database}

This is a very simple database, implemented on top of the file system
using the Dictionary class.  The Dictionary namespace reflects the
file path to the text (plus, in some cases, an additional name that
depends on the file type as described below).

There is currently no support for writing data.

Some care must be taken with the cases of characters in filenames on
Windows systems.  It may be wise to force all filenames to lower case
via the translate function, or to use a dictionary that is case
insensitive.

The main disadvantage of the SimpleDB implementation, to my mind, is
that there is no formal specification of the structure that verifies
that the information in related drectories is consistent.  Nothing
warns the user that the second image directory is missing a
details.haldx file, for example.  This could be fixed, but it's
difficult to find the energy to make a ``quick fix'' better.  A full
SQL interface should be developed instead.

\begin{code}
module SimpleDB (
  Translate, noCVS, allFiles, ReadDB, addDefaultsDB, readDB, readDB'
) where

import Template
import Dictionary
import Utilities
import IO
import Directory
import Monad
import List
import Char
\end{code}

\subsection{File Formats}

Three different file formats are supported, giving three ways of
associating the content with the dictionary namespace.  The formats
are distinguished by the file extension.

\begin{itemize}

\item {\bf hal} Files ending in ``.hal'' have all their contents stored
under the file's main name (with the associated directory path).  For
example, the contents of file diary/2004/jul/text.hal would be stored
in ``diary.2004.jul.text''.

\begin{code}
readHal :: Dictionary d String =>
  Translate -> d String -> FilePath -> [String] -> IO (d String)
readHal _ dct fp ky = do txt <- readFile fp
                         return $ add dct (ky, txt)
\end{code}

\item {\bf hals} Each paragraph (separated by one or more blank lines)
in a file with extension ``.hals'' is numbered.  So if the contents of
file months.hals had the contents

\begin{verbatim}
jan

feb

mar
..
\end{verbatim}

then the dictionary would associate, for example, ``months.5'' with
``May'' (this could be used to order the months for display, for
example --- a simpler solution might be available via the translation
facility described below).

More often this format is used to store several paragraphs of text
that are iterated over in a template so that each paragraph is enlosed
within its own $<$p$>$ element.

\begin{code}
readHals :: Dictionary d String =>
  Translate -> d String -> FilePath -> [String] -> IO (d String)
readHals tr dct fp ky = do h <- openFile fp ReadMode
                           dct' <- readParas h tr dct ky 0
                           hClose h
                           return dct'

readParas :: Dictionary d String =>
  Handle -> Translate -> d String -> [String] -> Int -> IO (d String)
readParas h tr dct ky n =
  do done <- hIsEOF h
     if done
       then return $ dct
       else do txt <- hGetPara h
               case txt of
                 Nothing   -> readParas h tr dct ky n
                 Just txt' ->
                   case tr $ ky ++ [(show n)] of
                     Nothing -> readParas h tr dct ky n
                     Just k  -> readParas h tr (add dct (k, txt')) ky (n+1)

hGetPara :: Handle -> IO (Maybe String)
hGetPara h = do done <- hIsEOF h
                if done
                  then return Nothing
                  else do txt <- hGetLine h
                          if "" == dropSpace txt
                            then hGetPara h
                            else do txt' <- collect h txt
                                    return $ Just txt'

collect :: Handle -> String -> IO String
collect h txt = do done <- hIsEOF h
                   if done
                     then return txt
                     else do txt' <- hGetLine h
                             if "" == dropSpace txt'
                               then return txt
                               else collect h $ txt ++ "\n" ++ txt'
\end{code}

\item {\bf haldx} The contents of files ending in ``.haldx'' are read as key
value pairs.  The first word on each line is the key, subsequent text,
starting with the first non--space character, is the value.  For
example, if the file diary/2004/highlights.haldx contains

\begin{verbatim}
1.month Jan
1.day   2
1.p     4
2.month July
2.day   20
2.p     1
\end{verbatim}

Then the dictionary would associate the value ``July'' with the key
``diary.2004.highlights.2.month''.

\begin{code}
readHaldx :: Dictionary d String =>
  Translate -> d String -> FilePath -> [String] -> IO (d String)
readHaldx tr dct fp ky = do h <- openFile fp ReadMode
                            dct' <- readLines h tr dct ky
                            hClose h
                            return dct'

readLines :: Dictionary d String =>
  Handle -> Translate -> d String -> [String] -> IO (d String)
readLines h tr dct ky = 
  do done <- hIsEOF h
     if done
       then return $ dct
       else do txt <- hGetLine h
               txt' <- return $ dropWindowsReturn txt
               case splitLine tr ky txt' of
                 Nothing     -> readLines h tr dct ky
                 Just (k, v) -> readLines h tr (add dct (k, v)) ky

dropWindowsReturn :: String -> String
dropWindowsReturn ""                = ""
dropWindowsReturn (c:s) | c == '\r' = dropWindowsReturn s
			| otherwise = c:(dropWindowsReturn s)

splitLine :: Translate -> [String] -> String -> Maybe ([String], String)
splitLine tr ky txt = 
  case keyVal txt of
    Nothing     -> Nothing
    Just (k, v) -> case tr $ ky ++ k of
                     Nothing  -> Nothing
                     Just ky' -> Just (ky', v)

keyVal :: String -> Maybe ([String], String)
keyVal s = keyVal' "" $ dropSpace s
  where
    keyVal' k ""                = Nothing
    keyVal' k (c:s) | isSpace c = Just (fromDot k, dropSpace s)
                    | otherwise = keyVal' (k ++ [c]) s
\end{code}

\end{itemize}

Given those formats it is possible to define two different values with
the same key.  In such cases the final result in the dictionary is
undefined (the key will be associated with one of the values, but the
choice will depend on implementation details).

The functions for parsing these file formats are stored in a
dictionary, indexed by file extension.  The database can be extended
to handle other formats by adding to this dictionary.

%%Haddock: Map file extension to file reading function
\begin{code}
type ReadDB d = Translate -> d String -> FilePath -> [String] -> IO (d String)
\end{code}
%%Haddock: Add the default file functions (hal, hals, haldx)
\begin{code}
addDefaultsDB :: (Dictionary d String, Dictionary r (ReadDB d)) =>
  r (ReadDB d) -> r (ReadDB d)
addDefaultsDB d = addAll' d [(hal,       readHal),
                             (hal++"s",  readHals),
                             (hal++"dx", readHaldx)]
\end{code}

\subsection{Translation}

Before a file is read, its full path (from the base directory of the
database, but excluding the file extension) is passed to a translate
function which returns either a (possibly modified) value or Nothing.
A valid value is used as the base key for the contents, a value of
Nothing will cause that file to be ignored.

Directories are also translated --- their names are not used as keys,
but a value of Nothing will prevent traversal of that directory.

%%Haddock: Type of translation function to select or modify file names
\begin{code}
type Translate = [String] -> Maybe [String]
\end{code}
%%Haddock: Block CVS directories
\begin{code}
noCVS :: Translate
noCVS s | last s == "CVS" = Nothing
        | otherwise       = Just s
\end{code}
%%Haddock: Select all files
\begin{code}
allFiles :: Translate
allFiles = Just
\end{code}

\subsection{HTML}

All file contents are read as plain text.  Functions called during the
generation of the site may parse some of these values as HTML, but
that functionality is not part of this database.

\subsection{Reading Data}

So here's the code...

\begin{code}
safety :: Translate
safety []                        = Nothing
safety l | last l        == ""   = Nothing
         | head (last l) == '.'  = Nothing
         | head (last l) == '/'  = Nothing
         | head (last l) == '\\' = Nothing
         | otherwise             = Just l
\end{code}
%%Haddock: Read a database from disk using the default file functions
\begin{code}
readDB' :: Dictionary d String =>
  Translate -> d String -> FilePath -> IO (d String)
readDB' = readDB (addDefaultsDB empty)
\end{code}
%%Haddock: Read a database from disk
\begin{code}
readDB :: (Dictionary d String, Dictionary r (ReadDB d)) =>
  r (ReadDB d) -> Translate -> d String -> FilePath -> IO (d String)
readDB d tr dc dr = readFP d (safety `thenMaybe` tr) dc dr []

readFP :: (Dictionary d String, Dictionary r (ReadDB d)) =>
  r (ReadDB d) -> ReadDB d
readFP d tr dct fp ky = 
    do isD <- doesDirectoryExist fp
       if isD
          then readDir d tr dct fp ky $ getDirectoryContents fp
          else readHFile d tr dct fp ky

readDir :: (Dictionary d String, Dictionary r (ReadDB d)) =>
  r (ReadDB d) -> Translate -> d String -> FilePath -> [String]
  -> IO [FilePath] -> IO (d String)
readDir d tr dct fp ky l = do l' <- l
                              foldM fn dct l'
  where
    fn dct' f = case tr $ ky ++ [beforeDot f] of
                  Nothing  -> do return dct'
                  Just ky' -> readFP d tr dct' (fp `slash` f) ky'
      where
        beforeDot ""                = ""
        beforeDot (c:s) | c == '.'  = ""
                        | otherwise = c:(beforeDot s)

readHFile :: (Dictionary d String, Dictionary r (ReadDB d)) =>
  r (ReadDB d) -> ReadDB d
readHFile d tr dct fp ky = case search' d (suffix fp) of
                             Nothing -> do putStrLn $ "no match for " ++ fp
                                           return dct
                             Just fn -> do putStrLn $ "reading " ++ fp
                                           fn tr dct fp ky

suffix :: String -> String
suffix = suffix' ""

suffix' :: String -> String -> String
suffix' x ""                = x
suffix' x (c:s) | c == '.'  = suffix' s s
                | otherwise = suffix' x s
\end{code}
