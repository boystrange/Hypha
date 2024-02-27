
SOLVER = glpk
HYPHA = ../src/hypha
ARGS = --timing --partition

NULL =

DEADLOCK_TESTS = \
  1 \
  2 \
  3 \
  4 \
  id \
  factorial \
  fairy_ring \
  fibonacci \
  fibonacci_network \
  fibonacci_tail \
  first_tail \
  forwarder \
  full_duplex \
  half_duplex \
  list_fold \
  list_length \
  list_even_odd \
  math_session \
  pair \
  pair2 \
  pair_split \
  producer_consumer \
  reader \
  simplecase \
  tree_depth \
  tree_even_odd \
  tree_take_skip \
  tree_take_skip_def \
  tree_left_right \
  $(NULL)

LINEARITY_TESTS = \
  $(DEADLOCK_TESTS) \
  buffer \
  filter_even \
  if \
  math_server \
  one \
  overlap \
  pair3 \
  twice \
  $(NULL)

ALL_TESTS = $(LINEARITY_TESTS) $(DEADLOCK_TESTS)

BROKEN_TESTS = \
  firstList \
  $(NULL)

all: $(LINEARITY_TESTS:%=Linearity/%.out) $(DEADLOCK_TESTS:%=DeadLock/%.out)

Linearity/%.out: %.hy $(HYPHA)
	@$(HYPHA) $(ARGS) $< >$@

DeadLock/%.out: %.hy $(HYPHA)
	@$(HYPHA) -d --solver=$(SOLVER) $(ARGS) $< >$@

%: %.hs
	ghc --make $<

Generate%: Generate%.hs
	ghc --make $<

1_%.hy: Generate1
	./Generate1 $(@:1_%.hy=%) >$@

.PHONY: all clean
clean:
	rm -f *.out Linearity/*.out DeadLock/*.out *.o
