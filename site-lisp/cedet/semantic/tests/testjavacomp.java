/** testjavacomp.java --- 
 *
 * Copyright (C) 2009 Eric M. Ludlam
 *
 * Author: Eric M. Ludlam <eric@siege-engine.com>
 * X-RCS: $Id: testjavacomp.java,v 1.1 2009/02/01 16:28:25 zappo Exp $
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

package tests.testjavacomp;

class secondClass {
    private void scFuncOne() {    }
    public void scFuncOne() {    }
}


public class testjavacomp {

    private int funcOne() {    }
    private int funcTwo() {    }
    private char funcThree() {    }

    class nestedClass {
	private void ncFuncOne() {	}
	public void ncFuncOne() {	}
    }

    public void publicFunc() {

	int i;

	i = fu// -1-
	    // #1# ( "funcOne" "funcTwo" )
	    ;

	fu// -2-
	    // #2# ( "funcOne" "funcThree" "funcTwo" )
	    ;

	secondClass SC;

	SC.//-3-
	    // #3# ( "scFuncOne" )
	    ;

	nestedClass NC;

	// @todo - need to fix this?  I don't know if  this is legal java.
	NC.// - 4-
	    // #4# ( "ncFuncOne" )
	    ;
    }

} // testjavacomp
