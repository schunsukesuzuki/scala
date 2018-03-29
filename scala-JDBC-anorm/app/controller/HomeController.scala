//https://qiita.com/Yaruki00/items/ad61f1977571c399fdcd


@Singleton

class HomeController @Inject()(db: DBAccess) extends Controller {
...
  def languageList = Action {
    Ok(db.languageList.toString)
  }