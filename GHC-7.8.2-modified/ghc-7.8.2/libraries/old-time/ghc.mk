libraries/old-time_PACKAGE = old-time
libraries/old-time_dist-install_GROUP = libraries
$(if $(filter old-time,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/old-time,dist-boot,0)))
$(if $(filter old-time,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/old-time,dist-install,1)))
$(if $(filter old-time,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/old-time,dist-install,2)))
