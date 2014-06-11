libraries/haskeline_PACKAGE = haskeline
libraries/haskeline_dist-install_GROUP = libraries
$(if $(filter haskeline,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/haskeline,dist-boot,0)))
$(if $(filter haskeline,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/haskeline,dist-install,1)))
$(if $(filter haskeline,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/haskeline,dist-install,2)))
