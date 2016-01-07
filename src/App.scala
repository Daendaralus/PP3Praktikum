

// Case Klasse f shopping items:
case class Item ( name : String, price : Double) 

// App mit Main Methode
object App {
    def main (args : Array[String] ) : Unit = {
      println ("Moin" )
      
      
      // Liste mit Items
      val itemList = List (
                            new Item ("Chocolate" , 0.69) , 
                            new Item ("Jeans" ,  69.95) ,
                            new Item ("Cupcakes" , 2.99),
                            new Item ("Seinfeld DVDs", 8.99),
                            new Item ("Orange Juice", 1.99),
                            new Item ("Ground Coffee", 5.99), 
                            new Item ("Coffee Machine" ,  99.95) 
                        ) 
      
      // Shopping Cart mit Items:                  
      val cart = new ShoppingCart (itemList) 
      
      
      // BEISPIELE 
      
      // Ausgabe aller Items
      println (cart.items)
      
      // Ausgabe aller Items, die gstiger als 4 Euro sind
      println (cart.filterItems( x => x.price < 4 ) )
      
      // Preise einzelner Items ausgaben:
      println( cart.apply("Chocolate") )
      println( cart("Jeans")  )
      
      
      // Alles  1 Euro teurer werden lassen:
      val expensiveItems = cart.priceUpdate( item => new Item (item.name , item.price + 1 )  )
    println(expensiveItems.items)
      println (cart.items)

      // TODO: Alles 19 % teurer werden lassen
      val expensiveItems2 = expensiveItems.priceUpdate( item => new Item (item.name , item.price*1.19 )  )
      println(expensiveItems2.items);
      // TODO: Alle Items, die teuerer als 50 Euro sind, erhalten 25% Rabatt
      val expensiveItems3 = expensiveItems2.priceUpdate2(d => if(d<=50) d else d*0.75)
      println(expensiveItems3.items);
      // TODO: Nacheinander ausfren: erst alles um 2 Euro erhhen, dann 10% Rabatt gewhren falls Item Eer 3 Euro, dann 19% aufschlagen
      val expensiveItemsComplete = cart.priceUpdate2(d => d+2).priceUpdate2(d=>if(d>3) d*0.9 else d).priceUpdate2(d=>d*1.19)
      println(expensiveItemsComplete.items)
      
     
      // Beispiel: Von jedem Item ein weiteres einpacken, wobei das zweite Item 50% gstiger ist
      val secondItem50Off = cart.multiplySomeItems { item => item::new Item(item.name, item.price*0.5)::Nil }
      println(secondItem50Off.items)
      // TODO: Zweites Item einpacken, falls gstiger als 10 Euro
      val secondItemLesser10 = cart.multiplySomeItems { item => { if(item.price<10) item::item::Nil  else item::Nil  } }
      println(secondItemLesser10.items)

      // TODO: Von jedem Item so viele einpacken, wie man f 100 Euro bekommt
      val manyItems = cart.multiplySomeItems { item => {
        var c = new ShoppingCart(Nil)
        while(c.totalSum()+item.price<=100) {
          c = new ShoppingCart(c.items:+item)}
        c.items
      } }
      println(manyItems.items)
      val manyItems2 = cart.multiplySomeItems { item => List.fill(math.floor(100/item.price).toInt)(item) }
      println(manyItems2.items)

      // TODO: Von jedem Item ein zustzliches, kostenloses Item einpack falls das Item in der specialOffer Liste enthalten ist
      val specialOffer = List ("Milk","Eggs", "Chocolate", "Cupcakes" , "Flour", "Soap" )
      val second4Free = cart.multiplySomeItems { item => if(specialOffer.contains(item.name)) item::new Item(item.name, 0)::Nil else item::Nil}
      println(second4Free.items)
      
     }
    

                      
}



class ShoppingCart (val l : List [Item] ) {
   
    // Items liefert die Liste l
    def items = l
  
    /**
     * Liefert den Preis von itemName
     */
    def apply(itemName : String ) = "Item: "  +  filterItems((item => item.name == itemName))(0).price
    
    /**
     * Liefert eine neue Liste mit Items, deren Preis gem
     * angepasst wurde
     * Als Defaultwert werden die items dieses Shopping Carts als Inputliste genommen
     */
    def priceUpdate (  f : Item => Item , l : List[Item] = items) : ShoppingCart = new ShoppingCart(l.mapConserve(f)  )
    def priceUpdate2 (  f : Double => Double , l : List[Item] = items) : ShoppingCart = new ShoppingCart(l.mapConserve((x=>new Item(x.name, f(x.price)))))
    
     /**
     * Liefert eine Liste, in der bestimme Items mehrfach vorkommen,
     * wobei f festlegt, wie hufig ein Item in der Liste vorkommt und welchen
     * Preis diese Items haben
     * Als Defaultwert werden die items dieses Shopping Carts als Inputliste genommen
     */
     private def multiply(f:Item => List[Item], l : List[Item] = items) : List[Item] = l match{
      case Nil => l
      case head::tail => f(head) ::: multiply(f, tail)
     }


    def multiplySomeItems( f: Item => List [Item] , l : List[Item] = items ) : ShoppingCart = new ShoppingCart(multiply(f, l))
    
    /**
     * Liefert eine Liste, in der nur die Preise der Items sind
     */
    def justPrices (l : List[Item] = items) : List [Double] = l.map(x=>x.price)
    
    /**
     * Beispielimplementierung (rekursiv)
     * Liefert nur die Items, f�E die die in f definierte Bedingung gilt
     * Als Defaultwert werden die items dieses Shopping Carts als Inputliste genommen
     */
    def filterItems ( f : Item => Boolean ,  l : List[Item] = items ) : List [Item] = l match {
      case Nil => l
      case head::tail => if  ( f ( head )  )  head  :: filterItems ( f , tail) else filterItems (f , tail) 
    }

    
    /**
     * Gesamtsumme aller Items in diesem Shopping Cart
     */
    def totalSum (l : List[Item] = items ) : Double = justPrices(l).sum
    
    /**
    *  Liefert das gstigste Item des Shopping Carts
    */
    def cheapestItem () : Item = items.sortWith(_.price <= _.price)(0) 
  
    
    
}