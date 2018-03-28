//https://qiita.com/mtoyoshi/items/c95cc88de2910945c39d

//flatten

Seq(Seq(1,2,3), Seq(4), Seq(5,6)) flatmap { x => x }

Seq(Seq(1,2,3), Seq(), Seq() flatMap { x => x }

Seq(Seq(1,2,3), Seq(), Seq(5,6)) flatten

//flatten + map

Seq(Seq(1,2,3), Seq(), Seq(5,6)) flatMap { x => 10 +: x }

Seq(Some(1), Some(2), None, Some(4)) flatMap { x => x mkString(",")}
