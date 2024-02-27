
-- this example shows that it may be necessary to
-- scan all use combinations in order to find the one
-- that minimizes the number of omegas.
-- try this example with and without the -m option and
-- look at the type of c (use -v option as well)

  new c in { a!c | b!c | b!c | c!1 }

