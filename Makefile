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

export PATH := $(PATH):$(build_dir)

build:
	ln -sf $(build_options_dynamic) $(build_options_yaml)
	$(stack) --copy-bins --local-bin-path build build $(package)

# Build static binary for Linux.
static-build:
	ln -sf $(build_options_static) $(build_options_yaml)
	$(stack) --copy-bins --local-bin-path build build $(package)

test:
	$(stack) test $(package)

clean:
	rm -rf $(build_dir) $(mkfile_dir)/.stack-work

.PHONY: build test clean