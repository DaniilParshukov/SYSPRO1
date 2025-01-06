import syspro.tm.WebServer;

import static syspro.tm.Tasks.Parser.registerSolution;


public class Main {
    public static void main(String[] args) {
        //WebServer.start();
        registerSolution(new ParserSysPro());
        //WebServer.waitForWebServerExit();
    }
}