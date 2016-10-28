/* ==================================================================================== */
/* ========		*Επεξεργασία Φυσικής Γλώσσας* 									======= */
/* ========		ΕΡΓΑΣΙΑ  2012 													======= */
/* ========		ΧΡΗΣΤΟΣ ΜΠΑΖΙΩΤΗΣ - Π07085 										======= */
/* ========		ΝΙΚΗ ΤΑΣΟΥΛΑ	  - Π07129 										======= */
/* ==================================================================================== */

/* ==================================================================================== */
/* 								SENTENCE												*/
/* ==================================================================================== */
s(s(s(NP,VP),CONJ,ST),Sem) --> np(NP,X), vp(VP,Y,_,_T), conj(CONJ,_Conj), s(ST,S),	{Sem=..[Y,X,S]}.
s(s(NP,VP),Sem) --> np(NP,X), vp(VP,Y,Z,W,_,_T), 	{Sem=..[Y,X,Z,W]}.
s(s(NP,VP),Sem) --> np(NP,X), vp(VP,Y,Z,_,_T), 		{Sem=..[Y,X,Z]}.
s(s(NP,VP),Sem) --> np(NP,X), vp(VP,Y,_,_T), 		{Sem=..[Y,X]}.

% Ο λόγος για τον οποίο χρησιμοποιούνται 3 διαφορετικοί κανόνες της μορφής
% S --> NP,VP είναι για την εξαγωγή των διαφορετικών νοημάτων(Semantics)
% Από τη μεριά της συντακτικής ανάλυσης δεν έχουν διαφορές

/* ==================================================================================== */
/* 								QUESTION		 										*/
/* ==================================================================================== */

/* ====================			YES - NO		==================== */ 
q(q(AV,NP,ADJ),	 Sem,tf) --> av(AV,Av,_), np(NP,N), adj(ADJ,Adj), 	 		       	{av(Av,1), Sem=..[Adj,N]}.

q(q(AV,NP,V),	 Sem,tf) --> av(AV,Av,T), np(NP,N), iv(V,Verb,2,T), 			   	{av(Av,1), Sem=..[Verb,N]}.
q(q(AV,NP,V),	 Sem,tf) --> av(AV,Av,T), np(NP,N), iv(V,Verb,3,T), 			   	{av(Av,2), Sem=..[Verb,N]}.
q(q(AV,NP,V,ADV),Sem,tf) --> av(AV,Av,T), np(NP,N), iv(V,Verb,2,T), adv(ADV,Adv), 	{av(Av,1), Sem=..[Verb,N,Adv]}.
q(q(AV,NP,V,ADV),Sem,tf) --> av(AV,Av,T), np(NP,N), iv(V,Verb,3,T), adv(ADV,Adv), 	{av(Av,2), Sem=..[Verb,N,Adv]}.

q(q(AV,NP,V,NP2),Sem,tf) --> av(AV,Av,T), np(NP,N), tv(V,Verb,2,T), np(NP2,N2), 	{av(Av,1), Sem=..[Verb,N,N2]}.
q(q(AV,NP,V,NP2),Sem,tf) --> av(AV,Av,T), np(NP,N), tv(V,Verb,3,T), np(NP2,N2), 	{av(Av,2), Sem=..[Verb,N,N2]}.

/* ====================			Wh Facts		==================== */

%ερωτήσεις με νόημα με 3 ορίσματα
q(q(QW,AV,NP1,V,NP2,PREP), X,wh) --> qw(QW,_Qw), av(AV,_Av,T), np(NP1,N1), tv(V,Verb,_,T), np(NP2,N2), prep(PREP,_Prep),	{Sem=..[Verb,N1,N2,X],Sem}.
q(q(QW,AV,NP1,V,NP2,PREP), X,wh) --> qw(QW,_Qw), av(AV,_Av,T), np(NP1,N1), tv(V,Verb,_,T), prep(PREP,_Prep), np(NP2,N2),	{Sem=..[Verb,N1,X,N2],Sem}.
q(q(QW,V,NP1,PREP,NP2),    X,wh) --> qw(QW,_Qw), tv(V,Verb,1,_T), np(NP1,N1), prep(PREP,_Prep), np(NP2,N2), 				{Sem=..[Verb,X,N1,N2],Sem}.


q(q(QW,V,ADV),	 X,wh) 	--> qw(QW,_Qw), iv(V,Verb,1,_), adv(ADV,Adv),					{Sem=..[Verb,X,Adv],Sem}.
q(q(QW,V,NP),	 X,wh) 	--> qw(QW,_Qw), iv(V,Verb,1,_), np(NP,N),						{Sem=..[Verb,X,N],Sem}.
q(q(QW,V,NP),	 X,wh) 	--> qw(QW,_Qw), tv(V,Verb,1,_), np(NP,N),						{Sem=..[Verb,X,N],Sem}.
q(q(QW,V),		 X,wh) 	--> qw(QW,_Qw), iv(V,Verb,1,_), 								{Sem=..[Verb,X],Sem}.

q(q(QW,AV,V),	   X,wh)	--> qw(QW,_Qw), av(AV,Av,T), 	iv(V,Verb,2,T), 				{av(Av,1), Sem=..[Verb,X],Sem}.
q(q(QW,AV,V,ADV),  X,wh)	--> qw(QW,_Qw), av(AV,Av,T), 	iv(V,Verb,2,T), adv(ADV,Adv),	{av(Av,1), Sem=..[Verb,X,Adv],Sem}.
q(q(QW,AV,NP,V),   X,wh) 	--> qw(QW,_Qw), av(AV,Av,T), 	np(NP,N), 	tv(V,Verb,3,T),		{av(Av,2), Sem=..[Verb,N,X],Sem}.
q(q(QW,AV,ADJ),    X,wh)  	--> qw(QW,_Qw), av(AV,Av,_), 	adj(ADJ,Adj),					{av(Av,1), Sem=..[Adj,X],Sem}.
q(q(QW,V,CONJ,ST), X,wh) 	--> qw(QW,_Qw), iv(V,Verb,1,_), conj(CONJ,_Conj), s(ST,S),		{Sem=..[Verb,X,S],Sem}.

q(q(QW,AV,NP,V), X,wh) 	--> qw(QW,how), av(AV,Av,T),	np(NP,N), 	iv(V,Verb,3,T),		{av(Av,2), adverb(X),Sem=..[Verb,N,X],Sem}.
q(q(QW,AV,NP,V), X,wh) 	--> qw(QW,how), av(AV,Av,T),	np(NP,N), 	iv(V,Verb,2,T),		{av(Av,1), adverb(X),Sem=..[Verb,N,X],Sem}.

q(q(QW,AV,NP,V,PREP), X,wh) --> qw(QW,what), av(AV,Av,T), np(NP,N), iv(V,Verb,2,T), prep(PREP,_Prep), {av(Av,1), Sem=..[Verb,N,X],Sem}.
q(q(QW,AV,V,PREP,NP), X,wh) --> qw(QW,who), av(AV,Av,T), iv(V,Verb,2,T), prep(PREP,_Prep), np(NP,N),  {av(Av,1), Sem=..[Verb,X,N],Sem}.

/* ==================================================================================== */
/* 								GRAMMAR RULES		 									*/
/* ==================================================================================== */
np(np(PP,DET,NBAR),X2) 	 --> pp(PP,_X1),  	 det(DET,_Det), nbar(NBAR,X2).	%, {Sem =[X1,X2]}.
np(np(PREP,DET,NBAR),X2) --> prep(PREP,_X1), det(DET,_Det), nbar(NBAR,X2).	%, {Sem =[X1,X2]}.
np(np(DET,NBAR,PP),X1) 	 --> det(DET,_Det),  nbar(NBAR,X1), pp(PP,_X2).		%, {Sem=..[X1,X2]}.


np(np(DET,NBAR),X) 		--> det(DET,_Det), 	nbar(NBAR,X).
np(np(PN),X) 			--> pn(PN,X).
np(np(N),X) 			--> n(N,X).

vp(vp(IV),Iv,C,T)  		  	--> iv(IV,Iv,C,T).
vp(vp(IV,ADV),Iv,Adv,C,T) 	--> iv(IV,Iv,C,T), adv(ADV,Adv).
vp(vp(IV,PREP),Iv,Prep,C,T) --> iv(IV,Iv,C,T), prep(PREP,Prep).
vp(vp(IV,ADJ),Iv,Adj,C,T) 	--> iv(IV,Iv,C,T), adj(ADJ,Adj).
vp(vp(IV,NP),Iv,X,C,T) 	  	--> iv(IV,Iv,C,T), np(NP,X).
vp(vp(TV,NP),Tv,X,C,T) 	  	--> tv(TV,Tv,C,T), np(NP,X).
vp(vp(AV,ADJ),X,_,T) 	  	--> av(AV,Av,T), adj(ADJ,X), {av(Av,1)}.

vp(vp(TV,NP1,PREP,NP2),Tv,X1,X2,C,T) --> tv(TV,Tv,C,T), np(NP1,X1), prep(PREP,_Prep), np(NP2,X2).

vp(vp(AV,IV),Iv,2,T)  	  		--> av(AV,Av,T), iv(IV,Iv,2,T), 				 	  {av(Av,1)}.
vp(vp(AV,IV,ADV),Iv,Adv,2,T)  	--> av(AV,Av,T), iv(IV,Iv,2,T), adv(ADV,Adv), 	 	  {av(Av,1)}.
vp(vp(AV,IV,PREP),Iv,Prep,2,T)  --> av(AV,Av,T), iv(IV,Iv,2,T), prep(PREP,Prep), 	  {av(Av,1)}.
vp(vp(AV,IV,PREP,N),Iv,X,1,T)	--> av(AV,Av,T), iv(IV,Iv,1,ps),prep(PREP,_), n(N,X), {av(Av,1)}.
vp(vp(AV,IV,NP),Iv,X,2,T)   	--> av(AV,Av,T), iv(IV,Iv,2,T), np(NP,X), 		 	  {av(Av,1)}.
vp(vp(AV,TV,NP),Tv,X,2,T)   	--> av(AV,Av,T), tv(TV,Tv,2,T), np(NP,X), 		 	  {av(Av,1)}.

pp(pp(PREP,NP),Sem) 	--> prep(PREP,Prep), np(NP,X), {Sem=..[Prep,X]}.
pp(pp(DET,NBAR),X) 		--> det(DET,_Det), 	nbar(NBAR,X).

nbar(nbar(N),Nx) 		--> n(N,Nx).
nbar(nbar(ADJ,NBAR),Sem) --> adj(ADJ,X1), nbar(NBAR,X2), {Sem=..[X1,X2]}.


/* ==================================================================================== */
/*							LEXICAL RULES			    							  	*/
/* ==================================================================================== */
det(det(X),X)	 -->[X], {determiner(X)}.
conj(conj(X),X)	 -->[X], {conjuction(X)}.
pn(pn(X),X)		 -->[X], {pronoun(X)}.
n(n(X),X)		 -->[X], {noun(X)}.
adj(adj(X),X)	 -->[X], {adjective(X)}.
iv(iv(X),Vx,C,T) -->[X], {iverb(Vx,C,T,[X],[])}.
tv(tv(X),Vx,C,T) -->[X], {tverb(Vx,C,T,[X],[])}.
av(av(X),X,T)	 -->[X], {averb(X,T)}.
qw(qw(X),X)		 -->[X], {qword(X)}.
adv(adv(X),X)	 -->[X], {adverb(X)}.
prep(prep(X),X)	 -->[X], {preparation(X)}.


/* ==================================================================================== */
/*							VOCABULARY	      				      				      	*/
/* ------------------------------------------------------------------------------------ */
/* Στο λεξικό αντί να γίνει χρήση πολλών λιστών, οι οποίες θα περιέχουν από μία λέξη η  */
/* κάθε μία, χρησιμοποιήθηκε το κατηγόρημα member σε συνδυασμό με *μία* λίστα με τις    */
/* λέξεις τις μίας κατηγορίας. Με αυτό τον τρόπο είναι πολύ πιο απλό και εύκολο να      */
/* προσθέσει κανείς μία νέα λέξη στην κατηγορία από το να φτιάξει ένα νέο κανόνα, αφού  */
/* απλά προσθέτει ένα στοιχείο σε μια λίστα, αλλά κάνει και τον κώδικα πιο απλό και     */
/* ευανάγνωστο και συνεπώς είναι πιο έυκολο και το debugging του προγράμματος			*/
/* ==================================================================================== */
determiner(X):- member(X, [a, an, the, one, some]).

conjuction(X):- member(X, [that, and, but]).

pronoun(X):- member(X, [chris, christos, niki, nicky, giorgos, takis, themis, thanos, panos, xaris, kostas,
						maria, dimitra, xristina, xristos, aggelos, mixalis, spyros, giannis, manos, sakis,
						leonidas, apostolis, dimitris, odysseas, axileas, aris, gianna, eleni, antonia]).

noun(X):- member(X, [ant, grasshopper, summer, winter,	food, woman, man, dog, cat, street, 
					 wall, mirror, car, money, river, mongoose, cobra, baby, mouth, market, 
					 waterpot, house, water, pot, remorse, entrance, day, night, moral, girl, 
					 boy, table, train, sun, moon, sea, fish, tv, radio, mobile, cellphone,
					 computer, book, cd, screen, bird, bed, mirror, glass, bottle, program]).
					 
adjective(X):- member(X, [fat, thin, smart, stupid, good, bad, lazy, happy, sad, dead, mad, ugly, tall,
						  bloody, big, small, faithful, hasteful, furious, alive, beautiful, handsome,
						  pretty, drunk, black, blue, white, green, red, orange, purple, yellow, brown]).
							
qword(X):- member(X, [who,what,how]).

adverb(X):- member(X, [fast, slow, loud, quite, hard, suddenly, dead, cheerfully]).

preparation(X):- member(X, [in, on, of, at, to, behind, before, after, until, back, front, nearby, with, without]).

/* ================== VERBS ================== */
/* ========	  INTRANSITIVE	========= */
iverb(lays,1,pr)-->[lays]. 	iverb(lays,2,pr)-->[laying].		iverb(lays,3,pr)-->[lay].
iverb(laid,1,ps)-->[laid]. 	iverb(laid,2,ps) -->[laying].		iverb(laid,3,ps) -->[lay].

iverb(thinks,1,pr)-->[thinks]. 		iverb(thinks,2,pr)-->[thinking].	iverb(thinks,3,pr)-->[think].
iverb(thought,1,ps)-->[thought]. 	iverb(thought,2,ps) -->[thinking].	iverb(thought,3,ps) -->[think].

iverb(runs,1,pr)-->[runs]. 			iverb(runs,2,pr)-->[running].		iverb(runs,3,pr)-->[run].
iverb(run,1,ps)	-->[run]. 			iverb(run,2,ps) -->[running].		iverb(run,3,ps) -->[run].

iverb(cries,1,pr)-->[cries]. 		iverb(cries,2,pr)-->[crying].		iverb(cries,3,pr)-->[cry].
iverb(cried,1,ps)-->[cried]. 		iverb(cried,2,ps) -->[crying].		iverb(cried,3,ps) -->[cry].

iverb(works,1,pr)	-->[works]. 	iverb(works,2,pr)	-->[working].	iverb(works,3,pr)	-->[work].
iverb(worked,1,ps)	-->[worked]. 	iverb(worked,2,ps) -->[working].	iverb(worked,3,ps) -->[work].

iverb(enters,1,pr)	-->[enters]. 	iverb(enters,2,pr)	-->[entering].	iverb(enters,3,pr)	-->[enter].
iverb(entered,1,ps)	-->[entered]. 	iverb(entered,2,ps) -->[entering].	iverb(entered,3,ps) -->[enter].

iverb(returns,1,pr)	-->[returns]. 	iverb(returns,2,pr)	-->[returning].	iverb(enters,3,pr)	-->[return].
iverb(returned,1,ps)-->[returned]. 	iverb(returned,2,ps)-->[returning].	iverb(entered,3,ps) -->[return].

iverb(sees,1,pr)	-->[sees]. 		iverb(sees,2,pr) -->[seeing].		iverb(sees,3,pr) -->[see].
iverb(saw,1,ps)		-->[saw]. 		iverb(saw,2,ps) -->[seeing].		iverb(saw,3,ps) -->[see].

iverb(notices,1,pr)	-->[notices]. 	iverb(notices,2,pr) -->[noticing].	iverb(notices,3,pr) -->[notice].
iverb(noticed,1,ps)	-->[noticed]. 	iverb(noticed,2,ps) -->[noticing].	iverb(noticed,3,ps) -->[notice].

iverb(becomes,1,pr)	-->[becomes]. 	iverb(becomes,2,pr) -->[becoming].	iverb(becomes,3,pr) -->[become].
iverb(became,1,ps)	-->[became]. 	iverb(became,2,ps) -->[becoming].	iverb(became,3,ps) -->[become].

iverb(fills,1,pr)	-->[fills]. 	iverb(fills,2,pr) -->[filling].		iverb(fills,3,pr) -->[fill].
iverb(filled,1,ps)	-->[filled]. 	iverb(filled,2,ps) -->[filling].	iverb(filled,3,ps) -->[fill].

iverb(plays,1,pr)	-->[plays]. 	iverb(plays,2,pr) -->[playing].		iverb(plays,3,pr) -->[play].
iverb(played,1,ps)	-->[played]. 	iverb(played,2,ps) -->[playing].	iverb(played,3,ps) -->[play].

:-dynamic lays/1,thinks/1,runs/1,cries/1,works/1,returns/1,plays/1.
:-dynamic laid/1,thought/1,run/1,cried/1,worked/1,returned/1,played/1.

:-dynamic lays/2,thinks/2,returns/2,enters/2,sees/2,notices/2,becomes/2,fills/2.
:-dynamic laid/2,thought/2,returned/2,entered/2,saw/2,noticed/2,became/2,filled/2.

/* ========	  TRANSITIVE	========= */
tverb(has,1,pr)		-->[has]. 		tverb(has,2,pr) -->[having].		tverb(has,3,pr) -->[have].
tverb(had,1,ps)		-->[had]. 		tverb(had,2,ps) -->[having].		tverb(hade,3,ps) -->[have].

tverb(goes,1,pr)	-->[goes]. 		tverb(goes,2,pr) -->[going].		tverb(goes,3,pr) -->[go].
tverb(went,1,ps)	-->[went]. 		tverb(went,2,ps) -->[going].		tverb(went,3,ps) -->[go].

tverb(leaves,1,pr)	-->[leave]. 	tverb(leaves,2,pr) -->[leaving].	tverb(leaves,3,pr) -->[leave].
tverb(left,1,ps)	-->[left]. 		tverb(left,2,ps) -->[leaving].		tverb(left,3,ps) -->[leave].

tverb(gives,1,pr)	-->[gives]. 	tverb(gives,2,pr) -->[giving].		tverb(gives,3,pr) -->[give].
tverb(gave,1,ps)	-->[gave]. 		tverb(gave,2,ps) -->[giving].		tverb(gave,3,ps) -->[give].

tverb(throws,1,pr)	-->[throws]. 	tverb(throws,2,pr) -->[throwing].	tverb(throws,3,pr) -->[throw].
tverb(threw,1,ps)	-->[threw]. 	tverb(threw,2,ps) -->[throwing].	tverb(threw,3,ps) -->[throw].

tverb(comes,1,pr)	-->[comes]. 	tverb(comes,2,pr) -->[coming].		tverb(comes,3,pr) -->[come].
tverb(came,1,ps)	-->[came]. 		tverb(came,2,ps) -->[coming].		tverb(came,3,ps) -->[come].

tverb(shoots,1,pr)	-->[shoots]. 	tverb(shoots,2,pr) -->[shooting].	tverb(shoots,3,pr) -->[shoot].
tverb(shot,1,ps)	-->[shoots]. 	tverb(shot,2,ps) -->[shooting].		tverb(shot,3,ps) -->[shoot].

tverb(fights,1,pr)	-->[fights]. 	tverb(fights,2,pr) -->[fighting].	tverb(fights,3,pr) -->[fight].
tverb(fought,1,ps)	-->[fought]. 	tverb(fought,2,ps) -->[fighting].	tverb(fought,3,ps) -->[fight].

tverb(kills,1,pr)	-->[kills]. 	tverb(kills,2,pr) -->[killing].		tverb(kills,3,pr) -->[kill].
tverb(killed,1,ps)	-->[killed]. 	tverb(killed,2,ps) -->[killing].	tverb(killed,3,ps) -->[kill].

tverb(feeds,1,pr)	-->[feeds]. 	tverb(feeds,2,pr) -->[feeding].		tverb(feeds,3,pr) -->[feed].
tverb(fed,1,ps)		-->[fed]. 		tverb(fed,2,ps)   -->[feeding].		tverb(fed,3,ps)   -->[feed].

tverb(kicks,1,pr)	-->[kicks]. 	tverb(kicks,2,pr) -->[kicking].		tverb(kicks,3,pr) -->[kick].
tverb(kicked,1,ps)	-->[kicked]. 	tverb(kicked,2,ps) -->[kicking].	tverb(kicked,3,ps) -->[kick].

tverb(gathers,1,pr)	-->[gathers]. 	tverb(gathers,2,pr) -->[gathering].	tverb(gathers,3,pr) -->[gather].
tverb(gathered,1,ps)-->[gathered]. 	tverb(gathered,2,ps)-->[gathering].	tverb(gathered,3,ps) -->[gather].

tverb(loves,1,pr)	-->[loves]. 	tverb(loves,2,pr) -->[loving].		tverb(loves,3,pr) -->[love].
tverb(loved,1,ps)	-->[loved]. 	tverb(loved,2,ps) -->[loving].		tverb(loved,3,ps) -->[love].

tverb(hates,1,pr)	-->[hates]. 	tverb(hates,2,pr) -->[hating].		tverb(hates,3,pr) -->[hate].
tverb(hated,1,ps)	-->[hated]. 	tverb(hated,2,ps) -->[hating].		tverb(hated,3,ps) -->[hate].

tverb(likes,1,pr)	-->[likes]. 	tverb(likes,2,pr) -->[liking].		tverb(likes,3,pr) -->[like].
tverb(liked,1,ps)	-->[liked]. 	tverb(liked,2,ps) -->[liking].		tverb(liked,3,ps) -->[like].

:-dynamic has/2,goes/2,leaves/2,comes/2,shoots/2,fights/2,kills/2,gathers/2,loves/2,hates/2,likes/2, feeds/2.
:-dynamic had/2,went/2,left/2,came/2,shot/2,fought/2,killed/2,gathered/2,loved/2,hated/2,liked/2, fed/2.

:-dynamic gives/3,throws/3.
:-dynamic gave/3,threw/3.

/* ========	  AUXILIARY	========= */
averb(X,pr):- member(X, [is, does]).
averb(X,ps):- member(X, [was, did]).

av(Av,1):- member(Av, [is, was]).
av(Av,2):- member(Av, [does, did]).

/* ==================================================================================== */
/*							TOKENIZER	      				      				      	*/
/* ==================================================================================== */
tokenize([],[]):- !.					% τερματικός κανόνας

tokenize(L,[Word|Out]):-
	L\==[],
	tokenize(L,Rest,WordChs),			% αναγνώριση της 1ης λέξης
	name(Word,WordChs),					% μετατροπή των κωδικών των χαρακτήρων 	της λέξης σε όρο της Prolog 
	tokenize(Rest,Out).					% μεταφορά στους υπόλοιπους κωδικούς χαρακτήρων

tokenize([],[],[]):- !.					% Τέλος της λέξης
	tokenize([46|_],[],[]):- !.			% Τελεία = τέλος πρότασης
	tokenize([63|_],[],[]):- !.			% Ερωτηματικό = τέλος πρότασης
	tokenize([32|T],T,[]):- !.			% Κενό = τέλος λέξης
	tokenize([44|T],T,[]):- !.			% Κόμμα = τέλος λέξης

tokenize([H|T],Rest,[H|List]):-			% Αν δεν τελειώνει μία λέξη τότε γίνεται προσθήκη
	tokenize(T,Rest,List). 				% του κωδικού χαρακτήρα και επανάληψη.
	
	
	
/* ------------------------------------------------------------------------------------ */
/* Οι δύο ακόλουθες μέθοδοι υλοποιούν την γραφική απεικόνιση της συντακτικής ανάλυσης   */
/* που εκτελεί ο parser. Η μόνη διαφορά που έχουν είναι πως η δεύτερη, σχεδιάζει και 	*/
/* γραμμές στα σημεία στα οποία αντιστοιχούν τα κλαδιά του συντακτικού δέντρου, ενώ η 	*/
/* πρώτη όχι. Στο πρόγραμμα επιλέχθηκα να γίνει χρήση της 2ης αλλά συμπεριλαμβάνεται 	*/
/* και η πρώτη ώστε να δοθεί η δυνατότητα για εναλλακτική.							 	*/
/* ------------------------------------------------------------------------------------ */
/* ==================================================================================== */
/*							PRINT TREE	      				      				      	*/
/* ==================================================================================== */
drawtree(Struct):- 
        drawtree(Struct,0).
 
drawtree(Struct,N):-
        Struct =.. [Mother|Daughters],
        write_out(Mother,N),
        NewN is N + 4,
        drawtrees(Daughters,NewN).
 
drawtrees([],_N):- !.
drawtrees([FirstDaughter|MoreDaughters],N):-
        drawtree(FirstDaughter,N),
        drawtrees(MoreDaughters,N).
 
write_out(Symbol,N):-
        nl, tab(N), write(Symbol).

/* ==================================================================================== */
/*							PRETTY PRINT TREE  				      				      	*/
/* ==================================================================================== */
pp(P):-pp([],P).
pp(L,P):-atomic(P),indented_print(L,P).
pp(L,P):-var(P),indented_print(L,P).
pp(L,[H|T]):-indented_print(L,'['), pp(['   |'|L],H),ppa(['   |'|L],T),indented_print(L,']').
pp(L,P):-P=..[F|Arg], pp(L,F),  ppa(['   |'|L], Arg).

ppa(L,[H|T]):-pp(L,H),ppa(L,T).
ppa(_L,[]):-true.

indented_print([H|T], P):-write(H), indented_print(T, P).
indented_print([],P):-write('-'), write(P), nl.


/* ==================================================================================== */
/*								Read Atom	  				      				      	*/
/* ==================================================================================== */
readAtom(A) :-
	readChars(S),
	name(A,S).

readChars([Char|Rest]):-
   get0(Char),
   Char\=10,!,
   readChars(Rest).

readChars([]).

/* ==================================================================================== */
/*								Read File by line	  							      	*/
/* ==================================================================================== */

is_eof(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
	CharCode == -1,
	append(FileAkku, [CurrentLine], FileContent),
	close(FlHndl), !.

is_newline(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
	CharCode == 10,
	append(FileAkku, [CurrentLine], NextFileAkku),
	read_loop(FlHndl, '', NextFileAkku, FileContent).
	
append_char(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
	char_code(Char, CharCode),
	atom_concat(CurrentLine, Char, NextCurrentLine),
         read_loop(FlHndl, NextCurrentLine, FileAkku, FileContent).

read_file(FileName, FileContent) :-
	open(FileName, read, FlHndl),
	read_loop(FlHndl, '', [], FileContent), !.

read_loop(FlHndl, CurrentLine, FileAkku, FileContent) :-
	get_code(FlHndl, CharCode),
	( is_eof(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)
	; is_newline(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)
	; append_char(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)).

	
/* ==================================================================================== */
/*								INFO On/Off			  							      	*/
/* ------------------------------------------------------------------------------------ */
/* Η μέθοδος αυτή απλά Ενεργοποιεί / Απενεργοποιεί την γραφική απεικόνιση.			    */
/* Απο προεπιλογή είναι *απενεργοποιημένη* διότι λόγο της έκτασης ειδικά της δενδροειδής*/
/* απεικόνισης μπορεί να γίνει ενοχλητική. 											    */
/* ΧΡΗΣΗ: απλά δίνοντας την εντολή info αλλάζει η κατάσταση από ON σε OFF και αντίστροφα*/
/* ==================================================================================== */

show_info(Structure) :- info(1),write('Δομή = '),writeln(Structure),pp(Structure).
show_info(_) :- info(0).

:-dynamic info/1.
info(0).

info :- 
		info(0)
		->
		retract(info(0)),asserta(info(1)),write('info ON')
		;
		retract(info(1)),asserta(info(0)),write('info OFF').

/* ==================================================================================== */
/*						USER INTERACTION FUNCTIONS			      				      	*/
/* ------------------------------------------------------------------------------------ */
/* Σε αυτό το σημείο ακολουθούν μερικοί κανόνες οι οποίοι τον τρόπο αλληλεπίδρασης	    */
/* του χρήστη με το πρόγραμμα. Ο στόχος είναι να είναι πιο φυσική η επικοινωνία και     */
/* να διευκολύνεται η εισαγωγή προτάσεων - γνώσης και η εξαγωγή συμπερασμάτων.          */
/* ==================================================================================== */

printLine([]).
printLine([H|T]):- write(H), nl,printLine(T).

import_semantics([]).
import_semantics([H|T]):- name(H,L),tokenize(L,Out),s(_,Sem,Out,[]),write(Sem), nl, asserta(Sem),import_semantics(T).

printStructure([]).
printStructure([H|T]):- write(H), nl, name(H,L),tokenize(L,Out),s(Structure,_,Out,[]), write(Structure), nl,nl, printStructure(T).

printParseTree([]).
printParseTree([H|T]):- write(H), nl, name(H,L),tokenize(L,Out),s(Structure,_,Out,[]), pp(Structure), nl,nl, printParseTree(T).

printAll([]).
printAll([H|T]):- 
				write('Πρόταση:    '), write(H), nl, nl, 
				name(H,L), tokenize(L,Out), s(Structure,Sem,Out,[]), 
				write('Νόημα-Semantic:    '), write(Sem), nl, nl,
				write('Συντακτική - Γραμματική ανάλυση:'), nl,
				nl, write(Structure), nl,
				nl, pp(Structure), nl,
				write('Press enter to continue...'), readAtom(_), 
				write('---------------------------------------------------------------'),  nl,
				printAll(T).

readfile(X) :- read_file(X, Content), write('Εισαγωγή νοημάτων (Semantics):'), nl, nl, import_semantics(Content).
readfileInfo(X) :- read_file(X, Content), printAll(Content),nl.

/* ==============	SAY		==================================================== */
/* Οι 2 τρόποι για την εισαγωγή μίας πρότασης. Αναλύονται στο εγχειρίδιο χρήστη. */
/* ============================================================================= */
say :-
	readAtom(X),
	name(X,L),
	tokenize(L,Out),
	s(Structure,Sem,Out,[]),
	asserta(Sem),
	write('Το γεγονός '),write(Sem),write(' προστέθηκε στη ΒΓ'),nl,
	show_info(Structure).
	
say(X):-
		name(X,L),
		tokenize(L,Out),
		s(Structure,Sem,Out,[]),
		asserta(Sem),
		write('Το γεγονός '),write(Sem),write(' προστέθηκε στη ΒΓ'),nl,
		show_info(Structure).
		

/* ==============	ASK		==================================================== */
/* Οι 2 τρόποι για την εκτέλεση μίας ερώτησης. Αναλύονται στο εγχειρίδιο χρήστη. */
/* ============================================================================= */		
ask :-
	readAtom(X),
	name(X,L),
	tokenize(L,Out),
	query(Out).
		
ask(X):-	
		name(X,L),
		tokenize(L,Out),
		query(Out).
		
query(Out):- q(Structure,Sem,tf,Out,[]), (Sem -> write('Aπάντηση: ΝΑΙ.') ; write('Aπάντηση: ΟΧΙ.')),show_info(Structure).
query(Out):- q(Structure,Fact,wh,Out,[]), write('Aπάντηση: '), write(Fact), nl, show_info(Structure).


/* ===============		ΠΑΡΑΔΕΙΓΜΑΤΑ	==================================================== 	
readfile('Haste makes Waste.txt').

say('chris runs fast').
ask('who runs fast?').
ask('how does chris run?').

say('chris loves a woman').
ask('who loves a woman?').
ask('who does chris love?').

say('chris loves a pretty girl').
ask('who loves a pretty girl?').
ask('who does chris love?').

say('chris gave a program to themis').
ask('what did chris give to themis').
ask('who was chris giving a program to').
ask('who gave a program to themis').
ask('what was chris giving to themis').

Πιο σύνθετο παράδειγμα:
say('chris gave a good program to themis').
ask('what did chris give to themis').
ask('who was chris giving a good program to').
ask('who gave a good program to themis').
*/