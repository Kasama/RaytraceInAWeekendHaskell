all: run

compile:
	stack build

run: src/ app/
	stack run > ./image.ppm
