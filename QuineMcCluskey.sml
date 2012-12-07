(*Tomas Pllaha*)
(*December 2011*)

(*this function takes two lists as arguments and returns true if they're equal*)
fun equal([],[]) = true
| equal([], _) = false
| equal (_, []) = false
| equal (h::l1, h2::l2) = if h = h2 then equal(l1,l2) else false;

(*this function takes a list and a list list and checks if the list is in the list list :P *)
fun exists(l1, []) = false
| exists(l1, l2::lss) = if equal(l1,l2) then true else exists(l1,lss);

(*This function removes duplicates from a list list. *)
fun remDup ([]) = []
| remDup(h::ls) = if exists(h,ls) then remDup(ls) else h::remDup(ls);

(*helper function for the "different" function below*)
fun different1([],[],i, ls,n) = (i,ls)
| different1 (h1::l1, h2::l2,i,ls,n) =  
	if (h1 <> h2) 
	then different1(l1,l2, i+1, ls@[n],n+1)
	else
		different1(l1,l2, i, ls, n+1);

(*this function takes two lists and returns a pair int * int list containing the number of elements
that are different and their respective position *)
fun different(l1, l2) = different1(l1,l2, 0, [], 0);
	
(*helper function for the R function *)	
fun R_helper(ls,i,j,ret) = 
if j >= List.length(ls) then R_helper(ls,i+1,i+1,ret)
else if i >= List.length(ls) - 1 then ret 
else

if 
	List.length(List.nth(ls,i)) = List.length(List.nth(ls,j))
then
	if #1 (different(List.nth(ls,i), List.nth(ls,j))) = 1
	then
		let 
			val n = hd(#2 (different(List.nth(ls,i), List.nth(ls,j))))
		in
			R_helper(ls,i,j+1, (List.take(List.nth(ls,i),n) @ [2] @ List.drop(List.nth(ls,i),n+1))::ret)
		end
	else R_helper(ls,i,j+1, ret)
else
	R_helper(ls,i,j+1, ret);

(*R(), R_hat(), M() and P() are defined as in the lecture notes. 
  Course: GenCSI, Jacobs University Bremen
  Prof.Dr. Michael Kohlhase
  page 97/slide 166
  URL: http://kwarc.info/teaching/GenCS1/notes.pdf 	*)
fun R(ls) = R_helper(ls, 0,0,[]);

(*Helper function for R_hat*)
fun R_hat_helper(ls,i,j,ret) = 
if j >= List.length(ls) then R_hat_helper(ls,i+1,0,ret)
else if i >= List.length(ls) then ret 
else

if 
	List.length(List.nth(ls,i)) = List.length(List.nth(ls,j))
then
	if #1(different(List.nth(ls,i), List.nth(ls,j))) = 1
	then
			if (not(exists(List.nth(ls,i),ret)))
			then
				R_hat_helper(ls,i,j+1, (List.nth(ls,i))::ret)
			else
				R_hat_helper(ls,i,j+1, ret)
	else R_hat_helper(ls,i,j+1, ret)
else
	R_hat_helper(ls,i,j+1, ret);
	



fun R_hat ls = R_hat_helper(ls, 0, 0, []);


(*The function above takes a truth table as an argument and returns the lists that represent the variable
assignments that yield true (1) *)
fun getMons([]) = []
| getMons (ls:(int list * int) list) = if #2(hd(ls)) = 1 
	then 
		#1(hd(ls))::getMons(tl(ls))
	else
		getMons(tl(ls));


fun M(0,ls) = getMons(ls)
| M(i,ls) = R(M(i-1,ls)); 		

(*the helper function for P *)
fun P_helper(j,m,n) = 
if n = nil 
then 
	nil
else

	if exists(hd(n), R_hat(m))
	then 
		P_helper(j,m,tl(n))
	else
		hd(n)::P_helper(j,m,tl(n));



fun P(j,ls) = P_helper(j,M(j-1,ls), M(j-1,ls));


(*Helper function for the qmc function that follows*)
fun qmcHelper(ls, i, cont) =
	if M(i,ls) = nil
	then
		remDup(List.concat(cont))
	else
		qmcHelper(ls,i+1, P(i+1,ls)::cont);


(*the function above takes a truth table as an argument and returns the "prime implicants" as a list 
of variable assignments *)
fun qmc(ls) = qmcHelper(ls,0, []);

(*helper function for the MonToStr function defined below*)		
fun MonToStr1(nil,_) = ""
| MonToStr1(h::ls,i) = 
if h = 0
then "(-x"^Int.toString(i)^")"^MonToStr1(ls,i+1)
else
	if h = 1
	then
		"x"^Int.toString(i)^MonToStr1(ls,i+1)
	else
		MonToStr1(ls,i+1);

(*the function above takes a variable assignment and returns a monomial represented by that
assignment in string form *)
fun MonToStr(ls) = MonToStr1(ls, 1);


(*the function above takes a list of variable assignments and returns the sum of the monomials as 
a string *)
fun toExp([]) = ""
| toExp([h]) = MonToStr(h)
| toExp(h::ls) = MonToStr(h) ^ " + " ^ toExp(ls);

(*the Function that implements the Quine McCluskey algorithm.
  It takes a truth table as an argument (in the same format as the example below)
  and returns the shortest polynomial (boolean expression) that is equivalent
  to that Truth table. *)

fun QuineMcCluskey(ls) = toExp(qmc(ls));


(*TEST CASE *)
val ls =[([0,0,0,0],1),([0,0,0,1],1),([0,0,1,0],1),([0,0,1,1],1),([0,1,0,0],1),([0,1,0,1],1), ([0,1,1,0],1), ([0,1,1,1],0),([1,0,0,0],1),([1,0,0,1],0),([1,0,1,0],1),([1,0,1,1],0), ([1,1,0,0],1), ([1,1,0,1],0),([1,1,1,0],1),([1,1,1,1],0)];
