all:
	make -C chameneos-redux all
	make -C lexifi-g2pp all
	make -C sauvola all
	make -C thread all
	make -C valet all
	make -C async_smtp all
	make -C cohttp all

.PHONY: clean

clean:
	make -C chameneos-redux clean
	make -C lexifi-g2pp clean
	make -C sauvola clean
	make -C thread clean
	make -C valet clean
	make -C async_smtp clean
	make -C	cohttp clean
