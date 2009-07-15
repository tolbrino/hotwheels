LOAD_PATH = \
	ebin \
	mochiweb/ebin \
	$(NULL)

MNESIA_DIR = /tmp/janus.db

NAME = janus
HOST = `hostname`
NODE = $(NAME)@$(HOST)
EC2_USER_DATA = `priv/user_data`

BASIC_OPTS = \
	-pa $(LOAD_PATH) \
	+A 8 +K true +P 60000 -smp disable \
	$(NULL)

OPTS = \
	$(BASIC_OPTS) \
	-mnesia dir '"$(MNESIA_DIR)"' \
	-s mnesia start \
	$(NULL)

LOCAL_OPTS = \
	$(OPTS) \
	-janus cluster $(CLUSTER) \
	-janus listen_port 8081 \
	$(EXTRA_OPTS) \
	$(NULL)

EC2_OPTS = \
	$(OPTS) \
	-janus cluster "'$(EC2_USER_DATA)'" \
	$(EXTRA_OPTS) \
	$(NULL)

all: compile

compile:
	erl -make

make_boot: compile
	erl $(BASIC_OPTS) -s janus_admin make_boot -s init stop

run: compile
	erl $(LOCAL_OPTS) -name $(NODE) -s janus start

run1: compile
	erl $(LOCAL_OPTS) -name $(NODE) -s janus start

run2: compile
	erl $(LOCAL_OPTS) -name $(NODE) -boot janus

ec2: compile make_boot
	erl $(EC2_OPTS) -name $(NODE) -boot janus 

remsh:
	erl $(BASIC_OPTS) -name remote -remsh $(NODE)

sh: compile 
	erl $(LOCAL_OPTS) -name debug 

sh1: compile 
	erl $(LOCAL_OPTS) -name debug1 -remsh debug@$(NODE)

clean:
	rm -rf ebin/*.beam
	rm -fr $(MNESIA_DIR)



