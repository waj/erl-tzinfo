all:
	./rebar compile

run:
	erl -pa ebin

download-tzinfo:
	wget "http://www.iana.org/time-zones/repository/tzdata-latest.tar.gz"
	mkdir -p priv/tzdata
	tar xvfz tzdata-latest.tar.gz -C priv/tzdata
	rm tzdata-latest.tar.gz