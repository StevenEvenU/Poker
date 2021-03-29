type suit = Spades | Hearts | Diamonds | Clubs

type value = Two 
| Three 
| Four 
| Five 
| Six 
| Seven 
| Eight 
| Nine 
| Ten 
| Jack 
| Queen 
| King 
| Ace

type card ={suit : suit; value : value} 

type deck = card option array 