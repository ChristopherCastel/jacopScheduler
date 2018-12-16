package etape_5

import org.xml.sax.helpers.DefaultHandler
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import com.sun.net.httpserver.HttpServer
import spray.json._
import DefaultJsonProtocol._
import sun.rmi.transport.proxy.HttpReceiveSocket
import com.sun.xml.internal.ws.transport.http.client.HttpResponseProperties

class ServletHoraire extends HttpServlet {

  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val action = req.getParameter("action")
    resp.setContentType("application/json")
    action match {
      case "getSchedule" =>
        getSchedule(req, resp)
      case _ =>
        resp.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }

  def getSchedule(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val serie = req.getParameter("serie")
    val bloc = req.getParameter("bloc")
    val data = Schedules.getScheduleSerie((Integer.parseInt(serie) - 1), (Integer.parseInt(bloc) - 1))
    val json = data.toJson.prettyPrint
    resp.setContentType("application/json");
    resp.getOutputStream().write(json.getBytes());
  }
}

