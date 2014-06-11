libraries/terminfo_PACKAGE = terminfo
libraries/terminfo_dist-install_GROUP = libraries
$(if $(filter terminfo,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/terminfo,dist-boot,0)))
$(if $(filter terminfo,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/terminfo,dist-install,1)))
$(if $(filter terminfo,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/terminfo,dist-install,2)))
