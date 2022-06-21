mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
build_dir := $(mkfile_dir)/build

package := sf-signer

package_yaml := $(mkfile_dir)/package.yaml
stack_yaml := STACK_YAML="$(mkfile_dir)/stack.yaml"
stack := $(stack_yaml) stack

export PATH := $(PATH):$(build_dir)

build:
	$(stack) --copy-bins --local-bin-path build build $(package)

# Build static binary for Linux. Patch package.yaml to add corresponding
# options.
static-build:
	cp $(package_yaml) $(package_yaml).bak
	sed -i 's/\#REMOVEME|//g' $(package_yaml)
	$(stack) --copy-bins --local-bin-path build build $(package)
	mv $(package_yaml).bak $(package_yaml)

test:
	$(stack) test $(package)

clean:
	rm -rf $(build_dir) $(mkfile_dir)/.stack-work

.PHONY: build test clean