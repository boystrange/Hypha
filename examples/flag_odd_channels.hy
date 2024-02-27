-- this example shows the difference between
-- allowing odd channels (with different input/output uses)
-- vs forbidding them
-- run this example with and without the -o flag

  new c in { c!3 | a!c }

