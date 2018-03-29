//https://qiita.com/Yaruki00/items/ad61f1977571c399fdcd

package models

import javax.inject._
import play.api.mvc._
import play.api.db._
import anorm._
import anorm.SqlParser._

@Singleton
class DBAccess @Injcct()(@NamedDatabase("DBname") db: Database) {

  val parser = int("id") ~ str("name") ~ int("experience")
  val mapper = parser.map {
    case id ~ name ~ experience => Map("id" -> id, "name" -> name, "experience" -> experience)
  }

  def laguaageList: List[Map[String,Any]] = {
    db.withConnection { implicit c =>
      SQL("SELECT * FROM language ORDER BY id").as(mapper.*)
    }
  }
  
  def insert: Option[Long] = {
    db.withConnection { implicit c =>
      SQL("insert into language(name, experience) values('Java', 4)").executeInsert()
    }
  }  
  
  def insertTwo: List[String] = {
    db.withConnection { implicit c =>

//http://seratch.hatenablog.jp/entry/2015/03/01/174115
// .executeInsert(str.+) << insertion returns a list of at least one string keys    

      SQL("insert into language(name, experience) values('C', 5), ('Ruby', 2)").executeInsert(str.+)
    }
  }
  
  def update: Int = {
    db.withConnection { implicit c =>
      SQL("update language set experience = 999 where name = 'Scala'").executeUpdate()
    }
  }

  def delete: Int = {
    db.withConnection { implicit c =>
      SQL("delete from language where name = 'C'").executeUpdate()
    }
  }  

  def search(language: String): Option[Map[String,Any]] = {
    db.withConnection { implicit c =>
      SQL("SELECT * FROM language where name = {language}").on("language" -> language).as(mapper.singleOpt)
    }
  }
  
//・・トランザクション
//db.withConnectionをdb.withTransactionに変えましょう。それだけ。　

}