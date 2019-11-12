# OCaml-Project-PR2
Bachelor Course: Programmazione 2

L'interprete progettato è un'estensione del linguaggio didattico funzionale che permette di manipolare, come dati primitivi, dizionari: una collezione di coppie (chiave,valore).

## Sintassi
Per esprimere al meglio un dizionario sono stati aggiunti nuovi tipi di espressione:
* _Dict of (ide * exp) list_ : per esprimere la dichiarazione di un dizionario come una lista di coppie				  (identificatore, espressione).
* _MyFunRec of ide * exp_ : per esprimere la dichiarazione di un una funzione ricorsiva utilizzabile in 				  ApplyOver (vedi sotto).
* _Estring of string_ : per esprimere la dichiarazione di una stringa.
* _Select of exp * ide_ : per esprimere l'operazione di selezione di un valore di una chiava appartenente 			  ad un dizionario.
* _Add of exp * ide * exp_ : per esprimere l'operazione di aggiunta di una coppia (chiave, valore) all'interno 			  di un dizionario.
* _Rm of exp * ide_ : per esprimere l'operazione di rimozione di una coppia (chiave, valore) all'interno 			  di un dizionario.
* _Clear of exp_ : per esprimere l'operazione di “pulizia” di un dizionario.
* _ApplyOver of exp * exp_ : per esprimere l'operazione di applicazione di una funzione ad ogni valore di 			  	  un dizionario.

## Tipi esprimibili e type-checker
Il risultato di una valutazione di una dichiarazione di un dizionario sarà un tipo esprimibile _DictVal of (ide * evT) list_ ed è dunque facile intuire che sarà una lista di coppie (identificatore, tipi esprimibili).

Inoltre, affinché possa essere inserita una stringa come valore in una coppia all'interno del dizionario, è stato aggiunto un tipo esprimibile _Estring of string_.

Conseguentemente è stato esteso il type-checker dinamico affinché restituisca _true_ se il tipo esprimibile valutato è una stringa.

## Interprete e funzioni ausiliarie
L'interprete di base fornitoci è stato esteso seguendo la regola di scoping statico ed implementando tutte le operazioni definite nella sintassi astratta sopra descritta.

L'interpretazione dell'espressione di dichiarazione di un dizionario è stata implementata utilizzando una funzione ausiliaria _evalDict_, la quale valuta ricorsivamente il valore di tipo _exp_ di ogni singola coppia e restituisce, come precedentemente detto, un tipo esprimibile _DictVal of (ide * evT) list_.

I dizionari sono entità immutabili ed è proprio questa caratteristica ad aver guidato l'implementazione di ogni operazione di manipolazione su di essi: restituiscono un nuovo dizionario modificato (o valore, nel caso della _Select_), lasciando invariato quello originale.

Per tutte queste operazione viene prima effettuato un controllo affinché sia stato passato un dizionario come parametro e in tal caso viene effettuata la specifica operazione tramite una funzione ausiliaria ad hoc.

Desta di particolare attenzione l'operazione _ApplyOver_ implementata utilizzando una funzione ausiliaria _ap_over_.
In quest'ultima infatti, viene verificato che la chiusura passata come parametro sia corretta (_FunVal_ per le funzioni iterative, _FunRecVal_ per quelle ricorsive) e ricorsivamente applico la funzione ad ogni valore del dizionario seguendo sempre le regole di scoping statico.

Nel caso in cui un valore del dizionario non sia conforme con il tipo richiesto dalla funzione, verrà lanciata un'eccezione a run-time "_Type Error_" .

## Parte opzionale: scoping dinamico
Per poter realizzare l'operatore _rt_eval(exp)_, che prende in ingresso un'espressione e restituisce un valore ottenuto eseguendo _exp_ con la regola di scoping dinamico, sono state effettuate le seguenti operazioni:
* estensione della sintassi: _Rt_eval of exp_ 
* estensione dei tipi esprimibili:  _FunValD of ide * evFunD | RecFunValD of ide * evFunD_   con   _evFunD = ide * exp_

Per quanto riguarda l'interprete è stato aggiunto il costrutto _Rt_eval(e)_, il quale richiama una funzione ausiliaria _evalD_.

Questa funzione non è altro che una mera copia dell'interprete _eval_ con la differenza che la chiusura di una funzione non contiene l'ambiente al momento in cui quest'ultima viene dichiarata e conseguentemente la valutazione di una chiamata di questa avviene in un ambiente globale.

Da notare infatti, che nel tipo esprimibile di una funzione (ovvero la chiusura: _evFunD = ide * exp_ ) è assente il tipo ambiente _evT env_.

## Batteria di test
All'interno del file _ProgettoOCaml.ml_ , nelle ultime righe, sono stati scritti vari test al fine di verificare il corretto funzionamento di ogni operazione.
Ogni singola operazione è stata opportunamente commentata esplicando esaurientemente quale calcolo viene effettuato ed eventualmente il risultato atteso.
