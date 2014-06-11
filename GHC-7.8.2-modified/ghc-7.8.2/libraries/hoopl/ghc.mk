libraries/hoopl_PACKAGE = hoopl
libraries/hoopl_dist-install_GROUP = libraries
$(if $(filter hoopl,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/hoopl,dist-boot,0)))
$(if $(filter hoopl,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/hoopl,dist-install,1)))
$(if $(filter hoopl,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/hoopl,dist-install,2)))
