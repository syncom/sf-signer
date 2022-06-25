mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
build_dir := $(mkfile_dir)/build

package := sf-signer

package_yaml := $(mkfile_dir)/package.yaml
package_yaml_static := $(mkfile_dir)/package-static.yaml
package_yaml_dynamic := $(mkfile_dir)/package-dynamic.yaml
stack_yaml := STACK_YAML="$(mkfile_dir)/stack.yaml"
stack := $(stack_yaml) stack

export PATH := $(PATH):$(build_dir)

build:
	ln -sf $(package_yaml_dynamic) $(package_yaml)
	$(stack) --copy-bins --local-bin-path build build $(package)

# Build static binary for Linux.
static-build:
	ln -sf $(package_yaml_static) $(package_yaml)
	$(stack) --copy-bins --local-bin-path build build $(package)

test:
	$(stack) test $(package)

clean:
	rm -rf $(build_dir) $(mkfile_dir)/.stack-work

.PHONY: build test clean