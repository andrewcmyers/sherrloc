libraries/xhtml_PACKAGE = xhtml
libraries/xhtml_dist-install_GROUP = libraries
$(if $(filter xhtml,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/xhtml,dist-boot,0)))
$(if $(filter xhtml,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/xhtml,dist-install,1)))
$(if $(filter xhtml,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/xhtml,dist-install,2)))
