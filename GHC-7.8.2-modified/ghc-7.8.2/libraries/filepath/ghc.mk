libraries/filepath_PACKAGE = filepath
libraries/filepath_dist-install_GROUP = libraries
$(if $(filter filepath,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/filepath,dist-boot,0)))
$(if $(filter filepath,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/filepath,dist-install,1)))
$(if $(filter filepath,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/filepath,dist-install,2)))
