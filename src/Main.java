import syspro.tm.WebServer;

import static syspro.tm.Tasks.Parser.registerSolution;


public class Main {
    public static void main(String[] args) {
        //WebServer.start();
        registerSolution(new ParserSysPro());
        //WebServer.waitForWebServerExit();

        /*(new ParserSysPro()).parse("""
                class Bitwise
                    def shiftRight(value: Int64, positions: Int64): Int64
                        return value >> positions
                    def shiftLeft(value: Int64, positions: Int64): Int64
                        return value << positions
                    def compareValues(a: Int64, b: UInt64): Boolean
                        return a < b
                    def bad(a, b)
                        foo(a >> > b)
                    def foo(a: Int64)
                        val res = ArrayList<ArrayList<Int64>>()
                        if a < 128u32
                            return a < 128
                        else
                            return a << 128""");
         */
    }
}