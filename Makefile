shell:
	erl -make
	erl -pa ebin/

clean:
	rm -f ebin/*.beam

.: shell
