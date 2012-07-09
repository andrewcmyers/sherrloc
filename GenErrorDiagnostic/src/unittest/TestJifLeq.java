package unittest;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import constraint.ast.LabelElement;

public class TestJifLeq {
	
	@Test
    public void testLEQ () {
    	LabelElement bot = new LabelElement("_->_");
    	LabelElement top = new LabelElement("*->*");
    	LabelElement xtoy = new LabelElement("x->y");
    	LabelElement xtobot = new LabelElement("x->_");
    	LabelElement xtotop = new LabelElement("x->");
    	
    	assertTrue(bot.leq_(top));
    	assertFalse(top.leq_(bot));
    	
    	assertTrue(bot.leq_(xtobot));
    	assertTrue(xtobot.leq_(bot));
    	
    	assertTrue(bot.leq_(xtotop));
    	assertFalse(xtotop.leq_(bot));
    	
    	assertTrue(xtobot.leq_(xtotop));
    	assertFalse(xtotop.leq_(xtobot));
    	
    	assertTrue(xtotop.leq_(top));
    	assertFalse(top.leq_(xtotop));
    	
    	assertTrue(xtobot.leq_(xtoy));
    	assertFalse(xtoy.leq_(xtobot));
    	
    	assertTrue(xtoy.leq_(xtotop));
    	assertFalse(xtotop.leq_(xtoy));
    }
}
