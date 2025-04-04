import scala.annotation.tailrec

case class Bowling(acc: Seq[Int] = Nil):
   def roll(p: Int): Bowling = Bowling(p +: acc)

   def score(): Either[String, Int] =
      @tailrec
      def calc(frame: Double, rs: Seq[Int], total: Int): Either[String, Int] =
         println((frame, rs, total))
         (frame, rs) match
            case (10, bonus) if bonus.forall(_ <= 10)                                => Right(bonus.sum + total)
            case (f, r1 :: r2 :: tail) if r1 + r2 < 10        => calc(f + 1.0, tail, r1 + r2 + total)
            case (f, r1 :: r2 :: r3 :: tail) if r1 + r2 == 10 => calc(f + 1.0, r3 :: tail, r1 + r2 + r3 + total)
            case (f, 10 :: r2 :: r3 :: tail)                  => calc(f + 1.0, r2 :: r3 :: tail, 10 + r2 + r3 + total)
            case (_, rs) if rs.exists(r => r > 10 || r < 0)   => Left("Error")
      calc(1, acc.reverse, 0)
