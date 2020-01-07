
.PHONY: test test-cov clean

test:
	julia -e 'using Pkg; Pkg.build("Slicing"); Pkg.test("Slicing")'

test-cov:
	julia -e 'using Pkg; Pkg.build("Slicing"); Pkg.test("Slicing", coverage=true)'

clean:
	find . -name '*.cov' -exec rm '{}' \;
