translate([1], .).
translate([1,1,1], -).
translate([1,1], .).
translate([1,1], -).
translate([1,1,1,1|Tail], Result) :- \+(member(0, Tail)), translate([1,1,1], Result).

% 0 means separate . s and - s from each other.
translate([0], dihdahs).
translate([0,0,0], ^).
translate([0,0,0,0,0,0,0], #).
translate([0,0], dihdahs).
translate([0,0], ^).
translate([0,0,0,0], ^).
translate([0,0,0,0,0], ^).
translate([0,0,0,0,0], #).
translate([0,0,0,0,0,0,0,0|Tail], Result) :- \+(member(1, Tail)), translate([0,0,0,0,0,0,0], Result).

separation(Pattern, []) :- translate(Pattern, dihdahs).
separation(Pattern, [^]) :- translate(Pattern, ^).
separation(Pattern, [#]) :- translate(Pattern, #).

signal_morse([], []).
signal_morse(B, [M]) :- translate(B, M).
signal_morse(B, [M]) :- append(Word, Pattern, B), translate(Pattern, dihdahs), translate(Word, M).
signal_morse(B, [M]) :- append(Word, Pattern, B), translate(Pattern, ^), translate(Word, M).
signal_morse(B, [M]) :- append(Word, Pattern, B), translate(Pattern, #), translate(Word, M).
signal_morse(B, M) :- 
	append(Head, Tail, B), separation(Pattern, Sign), append(Pattern, [1|PatternTail], Tail), 
	translate(Head, Trans), Trans \= dihdahs, append([Trans], Sign, TransHead), 
	signal_morse([1|PatternTail], TransTail), append(TransHead, TransTail, M).

morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)

signal_convert_word([], []).
signal_convert_word(M, [Letter]) :- morse(Letter, M).
signal_convert_word(M, [LetterHead|LettersTail]) :- 
	append(MHead, [^|MTail], M), morse(LetterHead, MHead),
	signal_convert_word(MTail, LettersTail).

signal_convert([], []).
signal_convert(Morse, Msg) :- signal_convert_word(Morse, Msg).
signal_convert(Morse, Msg) :- 
	append(Word, [#|WordsTail], Morse), 
	signal_convert_word(Word, WordMsg), 
	signal_convert(WordsTail, WordsMsg), append(WordMsg, [#|WordsMsg], Msg).

remove_error([], []).
remove_error([error|Tail], M) :- remove_error(Tail, M).
remove_error([Head|Tail], [Head|MTail]) :- remove_error(Tail, MTail).

remove_word([], []).
remove_word(String, M) :- append(_, [#,error|Tail], String), remove_word(Tail, M).
remove_word(String, M) :-
	once(append(Head, [error|Tail], String)), once(append(PrevWord, [#|_], Head)),
	remove_word(Tail, TailMsg), append(PrevWord, [#|TailMsg], M).
remove_word(String, M) :- once(append(_, [error|Tail], String)), remove_word(Tail, M).
remove_word(String, String).

signal_message(B, M) :- 
	signal_morse(B, Morse), signal_convert(Morse, RawM), 
	once(remove_word(RawM, FilteredMsg)), once(remove_error(FilteredMsg, M)).

