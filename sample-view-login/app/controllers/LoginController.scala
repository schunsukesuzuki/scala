package controllers;

import play.mvc.*;
import play.data.*;
import javax.inject.*;

import views.html.*;

import models.*;
import dto.*;
import services.*;

public class LoginController extends Controller {
    public Result login() {
        return ok(login.render());
    }
}