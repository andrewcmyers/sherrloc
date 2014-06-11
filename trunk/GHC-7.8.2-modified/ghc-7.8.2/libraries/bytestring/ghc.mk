libraries/bytestring_PACKAGE = bytestring
libraries/bytestring_dist-install_GROUP = libraries
$(if $(filter bytestring,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/bytestring,dist-boot,0)))
$(if $(filter bytestring,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/bytestring,dist-install,1)))
$(if $(filter bytestring,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/bytestring,dist-install,2)))
