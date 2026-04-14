package com.stackstate.pac4j.http

import org.apache.pekko.http.scaladsl.model.StatusCodes._
import org.apache.pekko.http.scaladsl.model.headers.Location
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.server.RouteResult.Complete
import com.stackstate.pac4j.PekkoHttpWebContext
import org.pac4j.core.context.WebContext
import org.pac4j.core.exception.http._
import org.pac4j.core.http.adapter.HttpActionAdapter

import scala.concurrent.Future

object PekkoHttpActionAdapter extends HttpActionAdapter {
  override def adapt(action: HttpAction, ctx: WebContext): Future[Complete] = {
    val context = ctx.asInstanceOf[PekkoHttpWebContext]
    Future.successful(Complete(action match {
      case r: UnauthorizedAction =>
        // XHR requests don't receive a TEMP_REDIRECT but a UNAUTHORIZED. The client can handle this
        // to trigger the proper redirect anyway, but for a correct flow the session cookie must be set
        context.addResponseSessionCookie()
        maybeWithContext(Unauthorized, r)
      case r: BadRequestAction =>
        maybeWithContext(BadRequest, r)
      case r: ForbiddenAction =>
        maybeWithContext(Forbidden, r)
      case a: FoundAction =>
        context.addResponseSessionCookie()
        HttpResponse(SeeOther, headers = List[HttpHeader](Location(Uri(a.getLocation))))
      case a: SeeOtherAction =>
        context.addResponseSessionCookie()
        HttpResponse(SeeOther, headers = List[HttpHeader](Location(Uri(a.getLocation))))
      case a: OkAction =>
        val contentBytes = a.getContent.getBytes
        val entity = context.getContentType.map(ct => HttpEntity(ct, contentBytes)).getOrElse(HttpEntity(contentBytes))
        HttpResponse(OK, entity = entity)
      case _: NoContentAction =>
        HttpResponse(NoContent)
      case r: StatusAction =>
        maybeWithContext(StatusCodes.getForKey(action.getCode).getOrElse(custom(action.getCode, "")), r)
      case _ =>
        HttpResponse(StatusCodes.getForKey(action.getCode).getOrElse(custom(action.getCode, "")))
    }))
  }

  private def maybeWithContext(statusCode: StatusCode, a: WithContentAction): HttpResponse = {
    if (a.getContent != null && a.getContent != "") {
      HttpResponse(statusCode, entity = HttpEntity(a.getContent))
    } else {
      HttpResponse(statusCode)
    }
  }
}
