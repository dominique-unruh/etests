# Proxy Makefile: the etests services are deployed on centroid from the qis-server
# repo (qis-server/etests/). These targets just forward to that Makefile, so you can
# build/deploy from here without leaving the etests repo.
#
# Override the deployment-repo path if it lives elsewhere:
#   make QIS=/path/to/qis-server/etests deploy

QIS ?= ../../qis-server/etests

default :
	$(MAKE) -C $(QIS)

build deploy try restart status logs shell apply :
	$(MAKE) -C $(QIS) $@

.PHONY: default build deploy try restart status logs shell apply
