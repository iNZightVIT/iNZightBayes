R := R
RCMD := $(R) --slave

document: README.md
	@$(RCMD) -e "devtools::document()"

test:
	@$(RCMD) -e "devtools::test()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "devtools::use_revdep()"
	@$(RCMD) -f "revdep/check.R"

crancheck: document
	@$(R) CMD build .
	@$(R) CMD check *.tar.gz

install:
	$(R) CMD INSTALL ./

README.md: README.Rmd
	$(RCMD) -e "rmarkdown::render('README.Rmd')" && rm README.html

clean:
	@rm -rf *.tar.gz *.Rcheck revdep

pkgdown:
	@$(RCMD) -e "pkgdown::build_site()"
