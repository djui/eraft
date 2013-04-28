REBAR=./rebar
PLT=.dialyzer_plt

all: deps compile

repl:
	@erl -pz ebin deps/*/ebin

deps:
	@$(REBAR) get-deps
	@$(REBAR) update-deps

lock-deps:
	@touch deps.lock

compile:
	@$(REBAR) compile

quick:
	@$(REBAR) compile skip_deps=true

clean:
	@$(REBAR) clean skip_deps=true

distclean: clean
	@rm -rf deps $(PLT)

## TESTING

test: eunit ct

eunit:
	@$(REBAR) eunit skip_deps=true

ct:
	@$(REBAR) ct skip_deps=true

xref:
	@$(REBAR) xref skip_deps=true

dialyzer: $(PLT)
	@dialyzer --plt $(PLT) -Wrace_conditions -Wunderspecs --src src
	# For < R15B02:
	# @dialyzer --plt $(PLT) -Wrace_conditions -Wunderspecs -Wno_behaviours --src src


$(PLT):
	@dialyzer --build_plt --output_plt $(PLT) \
		--apps erts kernel stdlib compiler \
		-r deps

.PHONY: deps test
