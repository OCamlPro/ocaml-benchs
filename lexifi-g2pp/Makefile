###########################################################################
#  Copyright (C) 2000-2013 LexiFi SAS. All rights reserved.               #
#                                                                         #
# This program is free software: you can redistribute it and/or modify    #
# it under the terms of the GNU General Public License as published       #
# by the Free Software Foundation, either version 3 of the License,       #
# or (at your option) any later version.                                  #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program.  If not, see <http://www.gnu.org/licenses/>.   #
###########################################################################

FILES=\
  date.mli \
  date.ml \
  math.mli \
  math.ml \
  optimization.mli \
  optimization.ml \
  g2pp_calibration.mli \
  g2pp_calibration.ml \
  main.ml

all: run.exe

run.exe: $(FILES)
	ocamlopt -o $@ -w +A $^

run:
	time ./run.exe

pack:
	tar czf g2pp.tar.gz $(FILES) Makefile README.txt COPYING

clean:
	rm -rf *.cm* run.exe *.o *.obj *~ *.tar.gz
