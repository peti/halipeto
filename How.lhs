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

\section{How it Works}

\subsection{Templates}

Templates separate page contents from page structure.  Halipeto
combines the two to produce the final page.

This approach has several advantages:

\begin{itemize}

\item You can work on the layout without worrying about the content.

\item You can work on the content without worrying about the layout.

\item You can generate many pages with similar structure from the same
template, so you only need to worry about the layout once.

\item You can design the layout so that it automatically accomodates
changes in the content.  For example, the number of items in a list
could change automatically, depending on the content.

\end{itemize}

Later in this document I'll demonstrate how to store and manage page
content, but first let's look at how we use templates to control page
structure.

\subsection{Functions}

Halipeto lets you associate (fairly) arbitrary functions with
attributes in HTML markup.  These functions can return values that are
inserted into the page and/or control the page structure.  This is all
fairly similar to other dynamic web page generation schemes like Jsp
or Zope (unlike those schemes, Halipeto is intended to generate web
pages before the user requests them --- the pages are not dynamic).

Functions are identified using XML attrbutes.  For example, the
template:

\begin{verbatim}
<p hal:text="pointless but simple"/>
\end{verbatim}

calls the function text to produce HTML like:

\begin{verbatim}
<p>pointless but simple</p>
\end{verbatim}

The emphasis on attributes rather than tags is taken from Zope's Page
Templates.  It allows Halipeto functions to exist in web pages without
confusing other web design tools (many get upset by arbitrary non-HTML
tags, but ignore attributes that have an unexpected namespace).

Since you, the user, are free to add functions, the system is very
flexible.  You might add functions to read data from an SQL database,
for example.

To help simplify the interaction between functions, state management,
and the retrieval of information from external sources, Halipeto
provides a {\em context}.  The context provides basic information
about the current page and supports a lexically scoped (relative to
the structure of the HTML template) namespace for storing information.

Some examples should make all this clear, but first I need to
introduce the database so that we have some content to work with.

\subsection{Dictionaries}

The Halipeto database is a dictionary that associates a key with some
value.  Keys are hierachical names, like module names in Haskell or
package names in Java, and are represented either as ``.''--separated
strings, or as lists of names.

\begin{verbatim}
"data.names.address" <-> ["data", "names", "address"]
\end{verbatim}

A dictionary is also used to store the functions used during the
evaluation of page templates (in this case the dictionary stores
functions rather than text).

To get information into the dictionary, Halipeto has a simple
mechanism for reading information from text files on disk.  In
general, the sequence of directry names and file name provide the key;
the file contents provide the value(s).

\subsection{Example --- Using the SimpleDB}

Lets consider a simple customer database.

We store the information about customers in a directory called
customers.  Each customer has a file that defines information with the
format:

\begin{verbatim}
first-name      Andrew
last-name       Cooke
phone           +56 2 4753233
city            Santiago
...
\end{verbatim}

This particular file is acooke.haldx (the extension is important
because it describes how Halipeto parses the information; in this case
key / value pairs, one per line).

This information can be read into the state dictionary, where it will
be associated with the path ``customers.acooke''.

The following template will then generate a table of customer details
(name in one column, phone number in the other):

\begin{verbatim}
<table>
  <div hal:repeat="key customers">
    <tr>
     <td hal:text="{key.first-name} {key.last-name}"/>
     <td hal:text="{key.phone}"/>
    </tr>
  </div>
</table>
\end{verbatim}

There was no need to write any new functions --- we only used
functions already provided by Halipeto.

This approach has some limitations, which reflect limitations in
the simple Halipeto database.  The ordering of customers is not clearly
defined, for example, and there is no automated check that guarantees
that each customer file has the correct information.  Some of these
problems have solutions, others may indicate that a more complex
solution (eg an SQL interface) is required for that particular
application.

\subsubsection{Ordering}

To fix the order of customer names we can generate a file order.hals
something like:

\begin{verbatim}
acooke

bsmith

...
\end{verbatim}

The following template will then use that ordering:

\begin{verbatim}
<table>
  <div hal:repeat="key order">
    <tr>
     <td hal:text="{customer.{key}.first-name}
                   {customer.{key}.last-name}"/>
     <td hal:text="{customer.{key}.phone}"/>
    </tr>
  </div>
</table>
\end{verbatim}

Hopefully the process here is fairly intuitive --- key takes the
values from order in turn (the hals extension for the file order.hals
tells Halipeto to associate each paragraph with an implicit integer
index; repeat respects that ordering) and the nested ``{...}'' syntax
substitutes the value of key into the path provided to text.

So the database is structured something like:

\begin{verbatim}
+- order
|  +- 1 = acooke
|  +- 2 = bsmith
|  :
+- customers
   +- acooke
   |  +- first-name = Andrew
   :  :
\end{verbatim}

and on the first iteration key takes the value ``acooke''.  The
attribute value ``\{customer.\{key\}.first-name\}'' is evaluated to
``\{customer.acooke.first-name\}'' and then to ``Andrew''.

Each set of curly brackets is replaced with the appropriate database
contents.  If no match is found, the database key is left as text ---
this helps debug database errors.

\subsection{Example --- Iterating Over an SQL Database Table}

Perhaps you want to use an SQL database instead.  You already have a
function, rdCustomerList, that returns this information.  How do we
define a function that inserts that data into an HTML page?

Halipeto makes it simple to define a function that works with the
following template:

\begin{verbatim}
<table>
  <div myfun:phonelist="">
    <tr>
     <td hal:text="{customer}"/>
     <td hal:text="{phone}"/>
    </tr>
  </div>
</table>
\end{verbatim}

Since the td elements are simply inserting dictionary entries, the
function associated with phonelist must (1) set the customer and phone
values in the database and (2) cause the template within the div
element to be evaluated for each customer.

Most of the work is done by setCustomerPhone, which iterates over the
list of values, but first we must read the values from the database by
calling rdCustomerList.

Note that phoneListInit (which is called by the attribute phonelist --
I'll describe how in a moment) ignores the arg variable.  This is the
attribute value in the template, which we are not using here.

\begin{code}
phoneListInit :: CustomFn
phoneListInit ctx arg = do list <- rdCustomerList
                           setCustomerPhone list ctx arg
\end{code}

Once the list is read, phoneListInit calls setCustomerPhone.

\begin{code}
setCustomerPhone :: [(String, String)] -> CustomFn
setCustomerPhone [] ctx arg = do return (ctx, Continue)
setCustomerPhone ((name, phone):list) ctx arg =
    do return (ctx'', Repeat setCustomerPhone list)
  where
    ctx' = add' ctx ("customer", name)
    ctx'' = add' ctx' ("phone", phone)
\end{code}

If setCustomerPhone is called with a non-empty list it updates the
dictionary and returns a Repeat result back to the template code.  The
Repeat constructor includes a curried call to setCustomerPhone with
the remaining data.

The template code will receive this value, evaluate the enclosed
template (generating the row with the first customer) and then call
setCustomerPhone again.  The process repeats, and the template
generates another row with another customer.

This process continues until the list of customers is empty.
SetCustomerPhone the returns Continue instead of Repeat and the template
code moves on to the next part of the document.

Finally, somewhere we must associate phoneListInit with the attribute
myfun:phonelist.  We do this by updating the dictionary of functions
in the context.

\begin{code}
setMyFuns ::  (SubDictionary s, Dictionary f (CustomFn s f)) =>
  Context s f -> Context s f
setMyFuns ctx =
  ctx {funcs = add' (funcs ctx) ("myfun.phonelist", phoneListInit)}
\end{code}

A more useful implementation might use the attribute value to identify
the table in the database, so that a single function can be used for a
variety of different tables.

Note the use of a curried function to enumerate the contents of a
list.  Halipeto was designed to support this pattern.

\subsection{More Information}

The rest of this document explains how Halipeto implements the
functionality described above.  The source code is also included.

At the end of this document is a fully worked example of a web site
(you might want to skip to that part now, but you'll need to refer to
the rest of the document to understand details like adding values to
dictionaries, or to find what other functions are avilable in
templates).
