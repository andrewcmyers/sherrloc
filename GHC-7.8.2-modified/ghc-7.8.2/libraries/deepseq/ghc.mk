libraries/deepseq_PACKAGE = deepseq
libraries/deepseq_dist-install_GROUP = libraries
$(if $(filter deepseq,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/deepseq,dist-boot,0)))
$(if $(filter deepseq,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/deepseq,dist-install,1)))
$(if $(filter deepseq,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/deepseq,dist-install,2)))
