SHELL := /bin/bash

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

ifndef LIGO
LIGO=docker run --platform linux/amd64 -u $(id -u):$(id -g) --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:0.67.1
endif
# ^ use LIGO en var bin if configured, otherwise use docker

compile = $(LIGO) compile contract --project-root ./lib ./lib/$(1) -o ./compiled/$(2) $(3) 
# ^ Compile contracts to Michelson or Micheline

test = @$(LIGO) run test $(project_root) ./test/$(1)
# ^ run given test file


.PHONY: test compile

install: ## install ligo libraries (FA2, FA1.2 dependencies for testing)
	install-ligo-lib

install-ligo-lib:
	@$(LIGO) install

compile: ## compile contracts to Michelson
	@mkdir -p compiled
	@$(call compile,../lib/cfmm/main_fa2_fa12.mligo,main_fa2_fa12.tz,)
	@$(call compile,../lib/cfmm/main_fa2_fa12.mligo,main_fa2_fa12.tz,--michelson-format json)
	@$(call compile,../lib/cfmm/main_fa2_fa2.mligo,main_fa2_fa2.tz,)
	@$(call compile,../lib/cfmm/main_fa2_fa2.mligo,main_fa2_fa2.tz,--michelson-format json)
	@$(call compile,../lib/cfmm/main_fa2_ctez.mligo,main_fa2_ctez.tz,)
	@$(call compile,../lib/cfmm/main_fa2_ctez.mligo,main_fa2_ctez.tz,--michelson-format json)
	@$(call compile,../lib/cfmm/main_fa12_fa12.mligo,main_fa12_fa12.tz,)
	@$(call compile,../lib/cfmm/main_fa12_fa12.mligo,main_fa12_fa12.tz,--michelson-format json)
	@$(call compile,../lib/cfmm/main_fa12_fa2.mligo,main_fa12_fa2.tz,)
	@$(call compile,../lib/cfmm/main_fa12_fa2.mligo,main_fa12_fa2.tz,--michelson-format json)
	@$(call compile,../lib/cfmm/main_fa12_ctez.mligo,main_fa12_ctez.tz,)
	@$(call compile,../lib/cfmm/main_fa12_ctez.mligo,main_fa12_ctez.tz,--michelson-format json)

test: ## run CFMM LIGO tests 
	test-ligo-cfmm 

##@Contracts - Run CFMM LIGO tests 
## For example : 
## make test-ligo FILE=set_position_fa12_fa12
## make test-ligo CONFIG=fa12_fa12
test-ligo-cfmm: 
ifdef FILE
	@$(call test,cfmm/$(FILE).test.mligo)
else ifdef CONFIG
	@$(call test,cfmm/set_position_$(CONFIG).test.mligo)
	@$(call test,cfmm/update_position_$(CONFIG).test.mligo)
	@$(call test,cfmm/x_to_y_$(CONFIG).test.mligo)
	@$(call test,cfmm/y_to_x_$(CONFIG).test.mligo)
	@$(call test,cfmm/complex_position_$(CONFIG).test.mligo)
else
	@$(call test,cfmm/set_position_fa2_fa2.test.mligo)
	@$(call test,cfmm/update_position_fa2_fa2.test.mligo)
	@$(call test,cfmm/x_to_y_fa2_fa2.test.mligo)
	@$(call test,cfmm/y_to_x_fa2_fa2.test.mligo)
	@$(call test,cfmm/complex_position_fa2_fa2.test.mligo)

	@$(call test,cfmm/set_position_fa12_fa12.test.mligo)
	@$(call test,cfmm/update_position_fa12_fa12.test.mligo)
	@$(call test,cfmm/x_to_y_fa12_fa12.test.mligo)
	@$(call test,cfmm/y_to_x_fa12_fa12.test.mligo)
	@$(call test,cfmm/complex_position_fa12_fa12.test.mligo)

	@$(call test,cfmm/set_position_fa2_fa12.test.mligo)
	@$(call test,cfmm/update_position_fa2_fa12.test.mligo)
	@$(call test,cfmm/x_to_y_fa2_fa12.test.mligo)
	@$(call test,cfmm/y_to_x_fa2_fa12.test.mligo)
	@$(call test,cfmm/complex_position_fa2_fa12.test.mligo)

	@$(call test,cfmm/set_position_fa12_fa2.test.mligo)
	@$(call test,cfmm/update_position_fa12_fa2.test.mligo)
	@$(call test,cfmm/x_to_y_fa12_fa2.test.mligo)
	@$(call test,cfmm/y_to_x_fa12_fa2.test.mligo)
	@$(call test,cfmm/complex_position_fa12_fa2.test.mligo)

	@$(call test,cfmm/set_position_fa2_ctez.test.mligo)
	@$(call test,cfmm/update_position_fa2_ctez.test.mligo)
	@$(call test,cfmm/x_to_y_fa2_ctez.test.mligo)
	@$(call test,cfmm/y_to_x_fa2_ctez.test.mligo)
	@$(call test,cfmm/complex_position_fa2_ctez.test.mligo)

	@$(call test,cfmm/set_position_fa12_ctez.test.mligo)
	@$(call test,cfmm/update_position_fa12_ctez.test.mligo)
	@$(call test,cfmm/x_to_y_fa12_ctez.test.mligo)
	@$(call test,cfmm/y_to_x_fa12_ctez.test.mligo)
	@$(call test,cfmm/complex_position_fa12_ctez.test.mligo)

	@$(call test,cfmm/set_position_sft_fa2.test.mligo)
	@$(call test,cfmm/update_position_sft_fa2.test.mligo)
	@$(call test,cfmm/x_to_y_sft_fa2.test.mligo)
	@$(call test,cfmm/y_to_x_sft_fa2.test.mligo)
	@$(call test,cfmm/complex_position_sft_fa2.test.mligo)
	
	@$(call test,cfmm/set_position_sft_fa12.test.mligo)
	@$(call test,cfmm/update_position_sft_fa12.test.mligo)
	@$(call test,cfmm/x_to_y_sft_fa12.test.mligo)
	@$(call test,cfmm/y_to_x_sft_fa12.test.mligo)
	@$(call test,cfmm/complex_position_sft_fa12.test.mligo)
endif

login:
	@$(LIGO) login

publish: ## publish package on packages.ligolang.org
	@$(LIGO) publish