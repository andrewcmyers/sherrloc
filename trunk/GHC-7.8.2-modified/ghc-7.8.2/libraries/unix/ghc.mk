libraries/unix_PACKAGE = unix
libraries/unix_dist-install_GROUP = libraries
$(if $(filter unix,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/unix,dist-boot,0)))
$(if $(filter unix,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/unix,dist-install,1)))
$(if $(filter unix,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/unix,dist-install,2)))
