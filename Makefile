lint:
	for fn in R/*.R; do\
		echo $$fn;\
		R --slave -e "setwd($fn); lintr::lint('$fn')";\
	done

build:
	echo "Building adobeanalyticsr on local..."
	echo "Built on local at $(date)" >> BUILD
	zip -r adobeanalyticsr.zip BUILD README.md DESCRIPTION NAMESPACE LICENSE R pkgdown _pkgdown.yml man
	rm -rf BUILD

install-package: build
	echo "Installing adobeanalyticsr on local..."
	R --slave -e "devtools::install_local('$(CURDIR)/adobeanalyticsr.zip')"

install: build install-package
	echo "Cleaning up..."
	rm -rf adobeanalyticsr.zip
