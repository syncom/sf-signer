mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))

all: sfsigner-r10e

sfsigner-r10e:
	$(mkfile_dir)/build.sh 2>/dev/null

clean:
	rm -rf $(mkfile_dir)/out

# Prune Docker image
dockerclean:
	docker image prune -a -f --filter=repo="sfsigner"

.PHONY: sfsigner-r10e clean dockerclean