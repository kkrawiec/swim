package swim

/* Program is a finite executable structure. However, it's execution is delegated
 * to the Domain classes, so it's not present in the Program trait. 
 */
trait Program {
  def size: Int 
}

