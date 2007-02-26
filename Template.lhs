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

\section{Templates}

Templates describe the structure of related pages.  It also defines
the basic data structures used elsewhere (to avoid circular module
references).

\begin{code}
module Halipeto.Template (
  CustomFn, Result (Attr, Text, Xml, Repeat, Continue, Skip),
  Position (Before, After, Replace), hal,
  UpdateDict, Page (Page), TreeSite (TreeSite), path, template, dictionary,
  Context (Ctx), state, funcs, site,
  readTemplate, evalElement, evalDocument
) where
\end{code}

\begin{code}
import Maybe
import Char
import Halipeto.Dictionary
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
\end{code}

\subsection{Context}

The context is the environment in which the template is processed.
Functions may modify the context (ie, return a new instance) if
necessary.  Generally only the state will be modified.

The context contains three dictionaries (the type below is more
general, but all functions in Halipeto assume that f implements the
Dictionary class and s the SubDictionary class).  The Dictionary type
is described later.  It is a pure data structure that associates keys
(strings) with values.

The funcs dictionary contains the custom functions available to the
template engine (see below).  The state dictionary provides a unified
way for the system to store and retrieve text.  The site component
defines the structure of the site that is being generated.

Non--textual data, or large quantities of text, should be manipulated
indirectly.  Functions may include references to other data or execute
IO actions, for example.  In such cases the state dictionary would
still be used to manage the associated meta--data (eg table names or
keys for SQL access).

%%Haddock: The context within which a template is evaluated
\begin{code}
data Context s f = 
    Ctx {state :: s String,          -- ^ State dictionary
         funcs :: f (CustomFn s f),  -- ^ Functions dictionary
         site  :: TreeSite s         -- ^ Site structure
        }
\end{code}

\subsection{Custom Functions}

The template is modified by custom functions that are stored within
the context.  Functions are invoked by appearing as attributes in the
template, under a non-empty namespace (the name of the function
corresponds to the attribute name; namespace and function name
together define a hierarchical key for the dictionary --- see the
Dictionary documentation for more details).

The argument supplied to the function is the value of the
corresponding attribute.

%%Haddock: The attribute value
\begin{code}
type Arg = String
\end{code}
%%Haddock: A function invoked by the template
\begin{code}
type CustomFn s f = Context s f -> Arg -> IO (Context s f, Result s f)
\end{code}

Note that currently fuctions do not have access to the XML structure
of the template.  This has not been necessary so far, and I'm
reluctant to introduce it until I find a compelling need (partly
because I'm not sure how until I have an example, and partly because
I'm not at all sure it's necessary).

The return code of the function controls subsequent processing.

%%Haddock: Control the position of inserted elements
\begin{code}
data Position = Before   -- ^ Insert data before current contents
              | After    -- ^ Insert data after current contents
              | Replace  -- ^ Replace current contents
\end{code}
%%Haddock: The result from a function called by the template engine
\begin{code}
data Result s f = Attr Name String        -- ^ Add an attribute
                | Text Position String    -- ^ Add text
                | Xml Position [Element]  -- ^ Add XML
                | Repeat (CustomFn s f)   -- ^ Recurse on function
                | Continue                -- ^ Process next attribute
                | Skip                    -- ^ Delete contents
\end{code}
%%Haddock: The XML namespace for builtin functions
\begin{code}
hal :: String
hal = "hal"
\end{code}

These types are discussed in more detail in the documentation for
Custom Functions.

\subsection{Pages}

The Page data type carries information used to process a particular
page.  Pages are grouped together in a hierarchy using TreeSite.

In addition, some information related to site structure is stored in
the state dictionary.  See the Pages documentation for more details.

%%Haddock: Modify the state to contain values for this page
\begin{code}
type UpdateDict s = s String -> [String] -> s String
\end{code}
%%Haddock: Description of a page
\begin{code}
data Page s = 
    Page {path       :: [String],     -- ^ Path to page
          template   :: String,       -- ^ Page template
          dictionary :: UpdateDict s  -- ^ State for page
         }
\end{code}
%%Haddock: Hierarchical site structure
\begin{code}
data TreeSite s = 
    TreeSite {page     :: Maybe (Page s),  -- ^ Parent page
              children :: [TreeSite s]     -- ^ Sub-pages
             }
\end{code}

\subsection{Reading a Template}

Templates are read as XML (so you have to use XHTML).  This is
necessary because the HaXml parser ``corrects'' HTML --- an important
feature of HaXml, but a problem here because it will discard something
like

\begin{verbatim}
<b hal:text="foo"/>
\end{verbatim}

which is invalid (or at least pointless) HTML, but a valid template.

%%Haddock: Current implementation uses HaXml
\begin{code}
type Template = Document
\end{code}
%%Haddock: Read and parse a template
\begin{code}
readTemplate :: String -> IO Template
readTemplate name = do text <- readFile name
                       return $ xmlParse name text
\end{code}

\subsection{Applying Functions}

We visit the elements in the HTML document in pre-order, checking for
attributes with associated namespaces.  If an attribute exists, we
evaluate the function and modify the HTML accordingly.

Note that changes to the HTML document are global, but changes to the
context only apply to sub--nodes in the tree.  If we use recursive
functions without accumulators then this corresponds to passing both
context and tree as function arguments, but returning only modified
HTML (so returning to a higher level function uses an earlier context,
as expected).

All changes are restricted to sub-trees, so there is no need to
re-evaluate elements.  Repeating functions repeat over the initial
context and structure, not over modified structure (but the
sub-structure may itself change each iteration).

%%Haddock: Evaluate the element (and its contents)
\begin{code}
evalElement :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Element -> IO Element
evalElement ctx e@(Elem _ _ _) =
    evalAttributes ctx evalContents e []
\end{code}

The function nxt below is a continuation that evaluates the element
contents.  In general, when we are evaluating a template, we evaluate
the attributes and then the contents (which is why evalElement above
passes evalContents as the continuation).

\begin{code}
evalAttributes :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> (Context s f -> Element -> IO Element) -> Element 
  -> [Attribute] -> IO Element
evalAttributes ctx nxt (Elem nm [] cn) at = 
    nxt ctx $ Elem nm (reverse at) cn
evalAttributes ctx nxt (Elem nm (a@(anm, val):as) cn) at = 
    if pth == []
      then evalAttributes ctx nxt (Elem nm as cn) (a:at)
      else case fn of
        Just f  ->
          evalFunction ctx nxt (Elem nm as cn) at f $ attVal val
        Nothing -> 
          evalAttributes ctx nxt (Elem nm as cn) (a:at)
  where
    fnm@[pth,_] = parseFunction anm
    fn = search (funcs ctx) fnm
\end{code}

No error is flagged if function lookup fails because not all
attributes with namespaces need to be functions (consider the xml
namespace).  A future improvement might let the user specify which
namespaces should be associated with functions.

\begin{code}
parseFunction :: Name -> [String]
parseFunction = parseFunction' "" ""

parseFunction' :: String -> String -> String -> [String]
parseFunction' p  n ""                = [p,n]
parseFunction' "" n (c:s) | c == ':'  = parseFunction' n "" s
                          | otherwise = parseFunction' "" (n++[c]) s
parseFunction' p  n (c:s)             = parseFunction' p (n++[c]) s
\end{code}

If a function returns a Repeat constructor (with a ``repeat
function'') then we do the following:

\begin{itemize}

\item create a new element that contains the remaining attributes and
all the content of the current element

\item evaluate the new element and sub-elements (continuation
evalContents) --- this is the ``first iteration''

\item after executing the first iteration, call evalFunction with the
initial element, the repeat function, and the modified context
(continuation skip - we're already managing the contents) --- this
gives the ``remaining iterations''

\item combine the results from the first iteration and the remaining
iterations to give the final result

\end{itemize}

Note the recursion on calling evalFunction, which is terminated by a
return value of Continue.  This is not tail recursive, so we may need
to improve things later.

\begin{code}
evalFunction :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> (Context s f -> Element -> IO Element) -> Element 
  -> [Attribute] -> CustomFn s f -> String -> IO Element
evalFunction ctx nxt (Elem nm at cn) at' f val =
    do (ctx', res) <- f ctx val
       case res of
         Attr n v  -> evalAttributes ctx' nxt (Elem nm at cn) 
                        ((n, AttValue [Left v]):at')
         Text p s  -> 
           case p of
             Before  -> evalAttributes ctx' nxt 
                          (Elem nm at ([CString False s]++cn)) at'
             After   -> evalAttributes ctx' nxt 
                          (Elem nm at (cn++[CString False s])) at'
             Replace -> evalAttributes ctx' nxt 
                          (Elem nm at [CString False s]) at'
         Xml p e  -> 
           case p of
             Before  -> evalAttributes ctx' nxt 
                          (Elem nm at ((map CElem e)++cn)) at'
             After   -> evalAttributes ctx' nxt 
                          (Elem nm at (cn++(map CElem e))) at'
             Replace -> evalAttributes ctx' nxt 
                          (Elem nm at (map CElem e)) at'
         Repeat f' -> loopAttribute ctx' (Elem nm at cn) at' f' val
         Continue  -> evalAttributes ctx' nxt (Elem nm at cn) at'
         Skip      -> evalAttributes ctx' skip (Elem nm [] []) at'

skip :: Context s f -> Element -> IO Element
skip _ e = do return e

loopAttribute :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Element -> [Attribute] -> CustomFn s f -> String 
  -> IO Element
loopAttribute ctx e@(Elem nm _ _) at1 f val =
    do (Elem _ at2 cn2) <- evalAttributes ctx evalContents e []  -- first
       (Elem _ at3 cn3) <- evalFunction ctx skip e [] f val      -- remain
       return $ Elem nm (joinAtts at1 at2 at3) (cn2++cn3)

revAppend :: [a] -> [a] -> [a]
revAppend base []     = base
revAppend base (x:xs) = revAppend (x:base) xs

joinAtts :: [Attribute] -> [Attribute] -> [Attribute] -> [Attribute]
joinAtts at1 at2 at3 = reverse (revAppend (revAppend at1 at2) at3)

evalContents :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Element -> IO Element
evalContents ctx (Elem nm at cn) = do cn' <- mapM (evalContent ctx) cn
                                      return $ Elem nm at cn'

evalContent :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Content -> IO Content
evalContent ctx (CElem e) = do e' <- evalElement ctx e
                               return $ CElem e'
evalContent _  c          = do return c
\end{code}

\subsection{Driver}

The following code generates HTML from a template.

%%Haddock: Evaluate a complete template
\begin{code}
evalDocument :: (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Document -> IO Document
evalDocument ctx (Document p st elt msc) =
    do elt' <- evalElement ctx elt
       return $ erase $ Document p st elt' msc
\end{code}

\subsection{Removing Elements}

Version 1.0 of Halipeto had some problems generating valid XHTML.  In
particular, repetition repeats the contents of an element, but not the
element itself.  This is for good reason --- the document's tree
structure is strictly respected so that the scope of any change to the
stae is always clearly defined.  However, the usual solution ---
adding an additional div or span element to carry the repeating
attribute --- is not always consistent with the XHTML DTD.

I'm unsure how to handle this.  Having "special" functions that don't
respect the document's tree structure sounds confusing.  Maybe I need
to introduce a completely different mechanism that involves re-writing
the tree (this would perhaps give a neater solution to the problem I
faced in the Pancito site, where database information was arranged by
row then column, but needed to be displayed by column then row).

For now, I'm going to implement a completely ad--hoc solution.  The
attribute hal:erase (unless defined as a function, probably in error)
will be used to indicate that an element should be removed from the
document.  The contents of the element are not removed, but included
in the parent element.

I'd appreciate feedback on this.  It implies that templates will still
not comply with DTDs, even though the final document will (but then
that has always been possible) --- is this a problem?

\begin{code}
erase :: Document -> Document
erase (Document p st elt msc) = Document p st (eraseChildren elt) msc

eraseChildren :: Element -> Element
eraseChildren (Elem nm at cn) = Elem nm at $ foldl eraseContent [] cn

eraseContent :: [Content] -> Content -> [Content]
eraseContent cn (CElem el) = if hasErase el'
                               then cn++cn'
                               else cn++[CElem el']
  where
    el'@(Elem _ _ cn') = eraseChildren el
eraseContent cn x          = cn++[x]

hasErase :: Element -> Bool
hasErase (Elem _ at _) = any f at
  where
    f ("hal:erase", _) = True
    f _                = False
\end{code}

\subsection{Attribute Values}

HaXml parses attributes as lists of strings and references, which is
nice and correct, but not the simple interface we want for Halipeto.
So these functions, pulled from the pretty printer internals of
HaXml, convert the attribute value to a string before the custom
function is called.

\begin{code}
attVal :: AttValue -> [Char]
attVal (AttValue esr) = concatMap (either id reference) esr

reference :: Reference -> [Char]
reference (RefEntity er) = entityref er
reference (RefChar cr)   = charref cr

entityref :: EntityRef -> [Char]
entityref n = "&" ++ show n ++ ";"

charref :: CharRef -> [Char]
charref c = "&" ++ show c ++ ";"
\end{code}

