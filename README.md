let rec filter_deck (deck : card list)= match deck with 
                                        x::xs -> if x.typee ="MINION" then x ::(filter_deck xs)
                                                    else filter_deck xs;;

let rec acc (c1 : card)( c2 :card) = if ((c1.health - c2.attack= c1.health)&& (c1.health < 0)) || ((c2.health - c1.attack=c2.health) &&(c2.health < 0)) 
                                          then att c1.health c2.health
                                      else acc c1 c2 
 
let att = if a > 0 && b < 0 then p1.life=p1.life && p2.life <- p2.life + c2.health && print_card_death c2
               elif a < 0 && b > 0 then p2.life=p2.life && p1.life <- p1.life + c1.health  && print_card_death c1
               else p1.life <- p1.life + c1.health && p2.life <- p2.life + c2.health && print_card_death c1 && print_card_death c2
                                  
let fight (deck1 : deck) (deck2 : deck) : player * player * int =
    let p1 = { name ="P1"; life = 30; deck = filter_deck deck1 }    // dummy players
    let p2 = { name ="P2"; life = 30; deck = filter_deck deck2 }
    let mutable turn = 1
    let mutable quit = false
    while not quit && p1.life > 0 && p2.life > 0 do
     print_turn_begin turn
    let mana = if turn > 10 then 10 else turn 
    let c1 = draw_card mana p1
    let c2 = draw_card mana p2
    match c1, c2 with
           [],_-> p1.life <- p1.life-c2.attack
          |_,[]-> p2.life <- p2.life-c1.attack
          |_,_-> if c1.attack > c2.health && c2.attack > c1.health 
                        then p1.life <- p1.life - (c2.attack - c1.health) && p2.life<- p2.life - (c1.attack - c2.health)
                  elif c1.attack > c2.health && c2.attack < c1.health 
                        then p1.life=p1.life && p2.life<- p2.life - (c1.attack - c2.health)
                  elif c1.attack < c2.health && c2.attack > c1.health 
                        then p2.life=p2.life && p1.life<- p1.life - (c2.attack - c1.health)
                 elif c1.attack < c2.health && c2.attack < c1.health 
                        then acc c1 c2 
    match c1, c2 with
           [],[]->print_turn_no_cards p1 p2 &&  print_turn_end p1 p2
          |[],_->print_turn_1card p1 c2 &&  print_turn_end p1 p2
          |_,[]->print_turn_1card p2 c1 &&  print_turn_end p1 p2
          |_,_->print_turn_2cards c1 c2 &&  print_turn_end p1 p2       
       
    p1, p2, turn+1
