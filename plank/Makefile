all: plan plan_with_tracing

test: plan
	bash ./test.sh

plan: plan.c
	gcc $^ -o $@

plan_with_tracing: plan_with_tracing.c
	gcc $^ -o $@
