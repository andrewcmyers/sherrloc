libraries/pretty_PACKAGE = pretty
libraries/pretty_dist-install_GROUP = libraries
$(if $(filter pretty,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/pretty,dist-boot,0)))
$(if $(filter pretty,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/pretty,dist-install,1)))
$(if $(filter pretty,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/pretty,dist-install,2)))
