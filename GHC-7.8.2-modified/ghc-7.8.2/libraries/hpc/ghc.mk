libraries/hpc_PACKAGE = hpc
libraries/hpc_dist-install_GROUP = libraries
$(if $(filter hpc,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/hpc,dist-boot,0)))
$(if $(filter hpc,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/hpc,dist-install,1)))
$(if $(filter hpc,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/hpc,dist-install,2)))
