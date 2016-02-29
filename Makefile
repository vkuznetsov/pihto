REBAR = "./rebar"

all: deps compile

deps:
	@( $(REBAR) get-deps )

compile:
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run: compile
	@( erl -sname pihto -pa ebin deps/*/ebin -config sys.config -boot start_sasl -s pihto_app start )

.PHONY: all deps compile clean run
