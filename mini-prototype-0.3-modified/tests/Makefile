##########################################################################
#  Mini, a type inference engine based on constraint solving.            #
#  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               #
#  and Didier Rémy.                                                      #
#                                                                        #
#  This program is free software; you can redistribute it and/or modify  #
#  it under the terms of the GNU General Public License as published by  #
#  the Free Software Foundation; version 2 of the License.               #
#                                                                        #
#  This program is distributed in the hope that it will be useful, but   #
#  WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  General Public License for more details.                              #
#                                                                        #
#  You should have received a copy of the GNU General Public License     #
#  along with this program; if not, write to the Free Software           #
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         #
#  02110-1301 USA                                                        #
#                                                                        #
##########################################################################

.PHONY: check bad good clean all clean-silent annnounce expected

all:

announce:
	@ if [ x$$VERBOSE != x1 ]; then \
	    echo "HINT: set VERBOSE=1, to show tests inputs and results."; \
          fi

install-test:
	@ for i in bad/* good/*; do		\
	  cp Makefile.test $$i/Makefile;	\
	done

check:	announce							    
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i check 2> /dev/null || true;    	    \
	done;				
	@ echo "--------------------------------------------------------";  
	@ echo "Global results:"				    
	@ cat bad/*/bad-ok-counter > bad-ok-counter;			    
	@ cat bad/*/bad-counter > bad-counter;				    
	@ echo "BAD:   "`wc -l bad-ok-counter | cut -f1 -d' '`		    \
	/ `wc -l bad-counter | cut -f1 -d' '`				    
	@ cat good/*/good-ok-counter > good-ok-counter;			    
	@ cat good/*/good-counter > good-counter;			    
	@ echo "GOOD:  "`wc -l good-ok-counter | cut -f1 -d' '`		    \
	/ `wc -l good-counter | cut -f1 -d' '`				    
	@ echo "--------------------------------------------------------"; 


clean:
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i clean 2> /dev/null || true;    	    \
	done;				
	@ rm --force *counter *~

expected:
	@ for i in bad/* good/*; do					    \
	  $(MAKE) --silent -C $$i expected 2> /dev/null || true;    	    \
	done;				
