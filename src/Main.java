import syspro.tm.WebServer;

import static syspro.tm.Tasks.Parser.registerSolution;


public class Main {
    public static void main(String[] args) {
        //WebServer.start();
        registerSolution(new ParserSysPro());
        //WebServer.waitForWebServerExit();

        /*(new ParserSysPro()).parse("""
                class ShiftsX
                  def foo(x: ArrayList<Int64>): Iterable<ArrayList<Int64>>
                    x.add(2 >> 1)
                    x.add(42 > > 2)
                    val res = ArrayList<ArrayList<Int64>>()
                    res.add(x)
                    return res
                """);
        */
    }
}