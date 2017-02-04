SILENT_MAKE = $(MAKE) --no-print-directory --silent
DEPS_DIR = $(shell pwd)/_build/default/lib
AMQP_DIR = $(DEPS_DIR)/amqp_client
AMQP_REPO = "https://github.com/lfe-support/rabbitmq-erlang-client.git"
RABCOM_DIR = $(DEPS_DIR)/rabbit_common
RAB_VER = "master"

amqp-client:
	@cd $(AMQP_DIR) && \
	git checkout $(RAB_VER) && \
	$(SILENT_MAKE) && \
	mkdir -p $(RABCOM_DIR)/ebin && \
	mv $(AMQP_DIR)/deps/rabbit_common/ebin/* $(RABCOM_DIR)/ebin
