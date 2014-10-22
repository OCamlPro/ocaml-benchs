all:
	make -C chameneos-redux all
	make -C lexifi-g2pp all
	make -C sauvola all

.PHONY: clean

clean:
	make -C chameneos-redux clean
	make -C lexifi-g2pp clean
	make -C sauvola clean
