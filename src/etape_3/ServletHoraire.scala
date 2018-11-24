package etape_3

import org.xml.sax.helpers.DefaultHandler
import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import com.sun.net.httpserver.HttpServer
import com.owlike.genson.defaultGenson_ 

class ServletHoraire extends HttpServlet {
  
  val genson = new GensonBuilder()
  
  override def doPost(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
		val action = req.getParameter("action");
    resp.setContentType("application/json")
    action match {
      case "getSchedule" =>
        getSchedule(req, resp)
      case _ =>
        resp.sendError(HttpServletResponse.SC_NOT_FOUND)
    }
  }
  
  def getSchedule(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val serie = req.getParameter("serie");
    val data = Schedules.getScheduleSerie((Integer.parseInt(serie) - 1))
    genson.
    
  }
}

