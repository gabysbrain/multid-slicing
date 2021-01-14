
.PHONY: test test-cov clean

test:
	julia -e 'using Pkg; Pkg.build("MdSlicing"); Pkg.test("MdSlicing")'

test-cov:
	julia -e 'using Pkg; Pkg.build("MdSlicing"); Pkg.test("MdSlicing", coverage=true)'

clean:
	find . -name '*.cov' -exec rm '{}' \;
