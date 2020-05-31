stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-12 build
	@echo
	stack --resolver lts-11 build
	@echo
	stack --resolver lts-10 build

stack-microlens:
	stack --resolver nightly build --flag pdc:microlens
	@echo
	stack --resolver lts build --flag pdc:microlens
	@echo
	stack --resolver lts-14 build --flag pdc:microlens
	@echo
	stack --resolver lts-13 build --flag pdc:microlens
	@echo
	stack --resolver lts-12 build --flag pdc:microlens
	@echo
	stack --resolver lts-11 build --flag pdc:microlens
	@echo
	stack --resolver lts-10 build --flag pdc:microlens
