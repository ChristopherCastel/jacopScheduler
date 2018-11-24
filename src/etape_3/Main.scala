package etape_3

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.servlet.ServletHolder
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.servlet.ServletHandler

object Main {
  def main(args: Array[String]) {
    val server = new Server(8080)
    val webAppContext = new WebAppContext()
    webAppContext.setResourceBase("www")
    webAppContext.addServlet(new ServletHolder(new DefaultServlet()), "/");
    webAppContext.addServlet(new ServletHolder(new ServletHoraire()), "/horaires")
    server.setHandler(webAppContext);
    server.start();
  }
}