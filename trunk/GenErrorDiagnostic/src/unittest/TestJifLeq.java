package unittest;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import constraint.ast.Environment;
import constraint.ast.LabelElement;

public class TestJifLeq {
	
	@Test
    public void testLEQ () {
    	LabelElement bot = new LabelElement("_->_");
    	LabelElement top = new LabelElement("*->*");
    	LabelElement xtoy = new LabelElement("x->y");
    	LabelElement xtobot = new LabelElement("x->_");
    	LabelElement xtotop = new LabelElement("x->");
    	Environment env = new Environment();
    	
    	assertTrue(bot.leq_(top, env));
    	assertFalse(top.leq_(bot, env));
    	
    	assertTrue(bot.leq_(xtobot, env));
    	assertTrue(xtobot.leq_(bot, env));
    	
    	assertTrue(bot.leq_(xtotop, env));
    	assertFalse(xtotop.leq_(bot, env));
    	
    	assertTrue(xtobot.leq_(xtotop, env));
    	assertFalse(xtotop.leq_(xtobot, env));
    	
    	assertTrue(xtotop.leq_(top, env));
    	assertFalse(top.leq_(xtotop, env));
    	
    	assertTrue(xtobot.leq_(xtoy, env));
    	assertFalse(xtoy.leq_(xtobot, env));
    	
    	assertTrue(xtoy.leq_(xtotop, env));
    	assertFalse(xtotop.leq_(xtoy, env));
    }
}
