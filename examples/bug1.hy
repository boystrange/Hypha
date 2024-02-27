
  *buffer?(in_0,out_2).{
   in_0?in_1.
   new out_3 in out_2!out_3.
   buffer!(in_1, out_3)
  }
|
  buffer!(w_0,r_2) | user!(r_2,w_0)
|
  *user?(r_2,w_0).
   new w_1 in w_0!w_1.
   new w_2 in w_1!w_2.
   r_2?r_3.
   r_3?r_4.
   user!(r_4,w_2)
   


-- quindi non si riesce a modellare un buffer grande 2 ?


  *buffer?(in_0,out_1).{
   in_0?in_2.
   new out_3 in out_1!out_3. -- deve poter consegnare il messaggio prima di accettare il successivo
   buffer!(in_2, out_3)
  }
|
  buffer!(w_0,r_1) | user!(r_1,w_0)
|
  *user?(r_1,w_0).
   new w_2 in w_0!w_2.
   r_1?r_3.
   new w_4 in w_2!w_4.
   r_3?r_5.
   user!(r_5,w_4)
   

