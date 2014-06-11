libraries/old-locale_PACKAGE = old-locale
libraries/old-locale_dist-install_GROUP = libraries
$(if $(filter old-locale,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/old-locale,dist-boot,0)))
$(if $(filter old-locale,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/old-locale,dist-install,1)))
$(if $(filter old-locale,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/old-locale,dist-install,2)))
