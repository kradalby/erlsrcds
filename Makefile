shell:
	erl -make
	erl -pa ebin/

clean:
	rm -f ebin/*.beam

dialyzer:
	dialyzer src/*

.: shell
