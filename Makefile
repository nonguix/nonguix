NONGUIX_GIT_KEYRING = origin/keyring

channel_intro_commit = 897c1a470da759236cc11798f4e0a5f7d4d59fbc
channel_intro_signer = 2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5

authenticate:
	@echo "Authenticating Git checkout..." ;		\
	guix git authenticate					\
	     --keyring=$(NONGUIX_GIT_KEYRING)			\
	     --cache-key=channels/nonguix --stats		\
	     "$(channel_intro_commit)" "$(channel_intro_signer)"
