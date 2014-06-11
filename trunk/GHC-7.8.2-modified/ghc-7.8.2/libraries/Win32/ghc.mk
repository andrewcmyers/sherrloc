libraries/Win32_PACKAGE = Win32
libraries/Win32_dist-install_GROUP = libraries
$(if $(filter Win32,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/Win32,dist-boot,0)))
$(if $(filter Win32,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/Win32,dist-install,1)))
$(if $(filter Win32,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/Win32,dist-install,2)))
