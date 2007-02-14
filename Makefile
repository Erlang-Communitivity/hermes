SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v
DIST_DIR=dist
SIGNING_KEY_ID=E96F1FA7

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

clean:
	rm -f $(TARGETS)

dist: all
	mkdir -p $(DIST_DIR)
	cp -r ebin src $(DIST_DIR)

distclean: clean
	rm -rf $(DIST_DIR)
	find . -name '*~' -exec rm {} \;

debian-package: clean
	tar -cf debian-package.tar .
	mkdir build
	cd build; tar -xf ../debian-package.tar
	cd build; dpkg-buildpackage -rfakeroot -k$(SIGNING_KEY_ID)
	rm -rf build debian-package.tar

test-compile:
	erlc $(ERLC_OPTS) $(wildcard test/*.erl)

RUN_ERL_CMD=erl -pa ebin -s smtp_server -s pop3_server

run: all
	$(RUN_ERL_CMD)

run_root: all
	sudo $(RUN_ERL_CMD) -smtp_server listen_port 25 -pop3_server listen_port 110
