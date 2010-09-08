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

\section{Functions}

This section describes the Custom Functions available by default in
the hal namespace.

\begin{code}
module Halipeto.Functions (
  split, parse,
  eval, attribute, text, textReplace, textAfter, repeat,
  addDefaultsFn,
  parseElements, parseElement, mkElements, mkElement, xhtml, element
) where

import Prelude hiding (repeat)
import Halipeto.Template
import Halipeto.Dictionary
import Halipeto.Utilities
import Data.Char
import Text.XML.HaXml.Parse hiding ( element )
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
\end{code}

\subsection{Argument Lists}

A function may require more than one argument, but attributes have
only a single value.  The convention used here is for a fixed number
of space delimited arguments, with the possibility of spaces in the
final value (usually text).

%%Haddock: Separate the argument into the expected number of values
\begin{code}
split :: Int -> String -> [String]
split n s =
    case split' n [] "" (dropSpace s) of
      Just ss -> ss
      Nothing -> error $ "too few arguments (<" ++ (show n) ++ "): " ++ s

split' :: Int -> [String] -> String -> String -> Maybe [String]
split' _ _ _ ""                = Nothing
split' 1 l _ s                 = Just $ l ++ [dropSpace s]
split' n l a (c:s) | isSpace c = split' (n-1) (l++[a]) "" (dropSpace s)
                   | otherwise = split' n l (a++[c]) s
\end{code}

\subsection{Argument Parsing}

I had to decide whether to do substittion before or after splitting an
argument.  If it introduces spaces then early substitution may alter
how the argument is split.  While this might have lead to cool
meta-programming hacks, I thought it more likely to confuse.  So
substitution is later.  The only danger with this that I can see is
that one might incorrectly assume that initial arguments, after
parsing and substitution, never contain embedded spaces.

%%Haddock: Separate argument and do substituion from state
\begin{code}
parse :: SubDictionary d => d String -> Int -> String -> [String]
parse d n = subAll d . split n
\end{code}

\subsection{hal:eval}

Evaluate a function.  Probably pointless, but I can't resist the
temptation.  All substitution takes place during the first evaluation.

\begin{verbatim}
<element hal:eval="fn-path arg"/>
-> <element fn="arg"/>
-> ...

<element hal:eval="hal.attribute name value"/>
-> <element hal:attribute="name value"/>
-> <element name="value"/>
\end{verbatim}

%%Haddock: Evaluate a named function
\begin{code}
eval :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
eval ctx arg = case search' (funcs ctx) nm of
                 Nothing -> return $ error $ "cannot find function " ++ nm
                 Just fn -> fn ctx val
  where
    [nm,val] = parse (state ctx) 2 arg
\end{code}

\subsection{hal:attribute}

Add an attribute to the current element.

\begin{verbatim}
<element hal:attribute="name value"/>
-> <element name="value"/>
\end{verbatim}

%%Haddock: Add an attribute to the current element
\begin{code}
attribute :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
attribute ctx arg = do return (ctx, Attr nm val)
  where
    [nm, val] = parse (state ctx) 2 arg
\end{code}

\subsection{hal:text}

Append / prepend text to the contents of the current element.

\begin{verbatim}
<element hal:text="value"/>
-> <element>value</element>
\end{verbatim}

\begin{code}
text' :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Position -> CustomFn s f
text' pos ctx arg = do return (ctx, Text pos $ substitute (state ctx) arg)
\end{code}
%%Haddock: Prepend text to the current element
\begin{code}
text ::
  (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
text = text' Before
\end{code}
%%Haddock: Append text to the current element
\begin{code}
textAfter ::
  (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
textAfter = text' After
\end{code}
%%Haddock: Replace text to the current element
\begin{code}
textReplace ::
  (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
textReplace = text' Replace
\end{code}

\subsection{hal:repeat}

Assign the sub-elements of root (in the state dictionary) to name, one
at a time, evaluating the template sub--tree.

\begin{verbatim}
root.1 = a
root.2 = b
<element hal:repeat="name root">
  <p hal:text="{name}"/>
</element>
-> <element>
     <p>a</p>
     <p>b</p>
   </element>
\end{verbatim}

%%Haddock: Repeat the evaluation of the remaining attributes and
%%Haddock: contents while the related function returns a Return
%%Haddock: value
\begin{code}
repeat :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
repeat ctx arg = repeat' nm vals ctx ""
  where
    dct = state ctx
    [nm, val] = parse dct 2 arg
    vals = children' dct val

repeat' :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  String -> [s String] -> CustomFn s f
repeat' p []     ctx _ = do putStrLn $ "end of repeat: " ++ p
                            return (ctx, Skip)
repeat' p (s:ss) ctx _ = do putStrLn $ "repeat: " ++ p ++ ": " ++
                              (show (contents s))
                            return (ctx', Repeat $ repeat' p ss)
  where
    ctx' = ctx {state = adopt' (state ctx) (p, s)}
\end{code}

\subsection{hal:eq, hal:neq}

Test for (non--)equality of the two arguments.

\begin{verbatim}
<element hal:eq="a b">foo</element> -> <element/>
\end{verbatim}

%%Haddock: Continue evaluation of contents if two arguments equal
\begin{code}
eq :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
eq  ctx arg = eq' ctx True  $ parse (state ctx) 2 arg
\end{code}
%%Haddock: Continue evaluation of contents if two arguments inequal
\begin{code}
neq :: (SubDictionary s, Dictionary f (CustomFn s f)) => CustomFn s f
neq ctx arg = eq' ctx False $ parse (state ctx) 2 arg

eq' :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Bool -> [String] -> IO (Context s f, Result s f)
eq' ctx x (a:[b]) = do return $ (ctx, if x `xor` (a /= b)
                                        then Continue
                                        else Skip)
  where
    p `xor` q = (p && not q) || (q && not p)
\end{code}

\subsection{Support for Insertion}

These functions provide basic support for meta-templates.  You can
place template HTML in the database then insert it in a ``skeleton''
template.  They're also useful for writing small ``abbreviation''
functions.

%%Haddock: Parse text as XML
\begin{code}
parseElements :: String -> [Element Posn]
parseElements txt = fromElement $ parseElement "parseelements" txt
  where
    fromElement (Elem "parseelements" _ els) = map unContent els
    unContent (CElem x _) = x
    unContent s@(CString _ _ _) = Elem "p" [] [s]
    unContent _ = error "cannot parse xml as element"
\end{code}
%%Haddock: Parse text as XML within an element
\begin{code}
parseElement :: String -> String -> Element Posn
parseElement elt txt = fromDoc $ xmlParse txt txt'
  where
    txt' = "<?xml version='1.0' encoding='iso-8859-1'?><" ++ elt ++ ">"
             ++ txt ++ "</" ++ elt ++ ">"
    fromDoc (Document _ _ el _) = el
\end{code}
%%Haddock: Generate a function that inserts XML parsed from text
\begin{code}
mkElements :: Position -> String -> CustomFn s f
mkElements pos txt ctx _ = do return (ctx, Xml pos (parseElements txt))
\end{code}
%%Haddock: Generate a function that inserts XML parsed from text,
%%Haddock: within an element
\begin{code}
mkElement :: Position -> String -> String -> CustomFn s f
mkElement pos elt txt ctx _ = do return (ctx, Xml pos [parseElement elt txt])
\end{code}

\subsection{hal:xhtml}

Insert a group of elements (bare text is enclosed in p tags).

\begin{verbatim}
link = foo
text = hello <a hal:attribute="href {link}">world</a>
<element hal:xhtml="{text}"/>
-> <element>
     <p>hello</p>
     <a href="foo">world</a>
   </element>
\end{verbatim}

%%Haddock: A custom function for inserting XHTML
\begin{code}
xhtml :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Position -> CustomFn s f
xhtml pos ctx txt = do return (ctx, Xml pos (parseElements txt'))
  where
    txt' = substitute (state ctx) txt
\end{code}

\subsection{hal:element}

Insert a named element.

\begin{verbatim}
link = foo
text = hello <a hal:attribute="href {link}">world</a>
<element hal:element="p {text}"/>
-> <element>
     <p>hello <a href="foo">world</a></p>
   </element>
\end{verbatim}

%%Haddock: A custom function for inserting XHTML within an element
\begin{code}
element :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Position -> CustomFn s f
element pos ctx txt = do return (ctx, Xml pos [parseElement tag txt''])
  where
    [tag, txt'] = split 2 txt
    txt'' = substitute (state ctx) txt'
\end{code}

For an example of the element tag in use, see the image.html teplate
in the demo.  Insertion of a p element allows the body text to contain
template functions without the need for explicit $<$p$>$ markup around
each paragraph.

\subsection{Load Defaults}

Make the custom functions above available in the funcs dictionary
under the hal namespace.

%%Haddock: Add standard functions to a dictionary
\begin{code}
addDefaultsFn :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  f (CustomFn s f) -> f (CustomFn s f)
addDefaultsFn fn = addAll fn fns
  where
    fns = map (\(n,f) -> ([hal, n], f)) lst
    lst = [("eval",             eval),
           ("attribute",        attribute),
           ("text",             text),
           ("textafter",        textAfter),
           ("textreplace",      textReplace),
           ("repeat",           repeat),
           ("eq",               eq),
           ("neq",              neq),
           ("xhtml",            xhtml Before),
           ("xhtmlafter",       xhtml After),
           ("xhtmlreplace",     xhtml Replace),
           ("element",          element Before),
           ("elementafter",     element After),
           ("elementreplace",   element Replace)]
\end{code}

