//https://qiita.com/Yaruki00/items/ad61f1977571c399fdcd

package models

import javax.inject._
import play.api.mvc._
import play.api.db._
import anorm._
import anorm.SqlParser._

@Singleton
class DBAccess @Inject()(db: Database) {
}
