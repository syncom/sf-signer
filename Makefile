mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
build_dir := $(mkfile_dir)/build

package := sf-signer

package_yaml := $(mkfile_dir)/package.yaml
build_options_yaml := $(mkfile_dir)/build-options.yaml
build_options_static := $(mkfile_dir)/build-options-static.yaml
build_options_dynamic := $(mkfile_dir)/build-options-dynamic.yaml
stack_yaml := STACK_YAML="$(mkfile_dir)/stack.yaml"
stack := $(stack_yaml) stack
stack_extra_flags := $(STACK_EXTRA_FLAGS)
# Workaround the expiry of hardcoded test certificate in test/Spec.hs, which
# expires on Dec 20 20:10:47 2023 GMT, 10 minutes after the faketime.
# The expiration time of the certificate is obtained with command
#     openssl x509 -enddate -in /path/to/cert-file -noout
# This fake time also should work for the root CA certificate in test/Spec.hs,
# which expires on Dec 20 20:10:48 2023 GMT.
# Ten minutes should be long enough for the tests to run to completion.
stack_faketime := $(stack_yaml) faketime "Dec 20 20:00:47 2023 GMT" stack

export PATH := $(PATH):$(build_dir)

.PHONY: build
build:
	ln -sf $(build_options_dynamic) $(build_options_yaml)
	$(stack) $(stack_extra_flags) --copy-bins --local-bin-path build build $(package)

# Build static binary for Linux.
static-build:
	ln -sf $(build_options_static) $(build_options_yaml)
	$(stack) $(stack_extra_flags) --copy-bins --local-bin-path build build $(package)

ifeq ($(shell uname -s),Darwin)
test_suite_exe := sf-signer-test

test:
	$(stack) $(stack_extra_flags) build --test --no-run-tests
	cp $(stack path --dist-dir)/build/$(test_suite_exe)/$(test_suite_exe) build/
	ls -l build/*
	faketime "Dec 20 20:00:47 2023 GMT" "build/$(test_suite_exe)"
else
test:
	$(stack_faketime) $(stack_extra_flags) test $(package)
endif

clean:
	rm -rf $(build_dir) $(mkfile_dir)/.stack-work

.PHONY: build test clean
