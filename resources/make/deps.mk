SILENT_MAKE = $(MAKE) --no-print-directory --silent
DEPS_DIR = $(shell pwd)/_build/default/lib
AMQP_DIR = $(DEPS_DIR)/amqp_client
AMQP_REPO = "https://github.com/rabbitmq/rabbitmq-erlang-client.git"
RABCOM_DIR = $(DEPS_DIR)/rabbit_common
RAB_VER = "rabbitmq_v3_7_0_milestone10"

amqp-client:
	@cd $(AMQP_DIR) && \
	git checkout $(RAB_VER) && \
	$(SILENT_MAKE) && \
	mkdir -p $(RABCOM_DIR)/ebin && \
	mv $(AMQP_DIR)/deps/rabbit_common/ebin/* $(RABCOM_DIR)/ebin
