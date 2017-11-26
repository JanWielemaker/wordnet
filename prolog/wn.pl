:- module(wordnet,
	  [ wn_s/6,			% basic Wordnet relations
	    wn_g/2,
	    wn_hyp/2,
	    wn_ins/2,
	    wn_ent/2,
	    wn_sim/2,
	    wn_mm/2,
	    wn_ms/2,
	    wn_mp/2,
	    wn_der/4,
	    wn_cls/5,
	    wn_cs/2,
	    wn_vgp/4,
	    wn_at/2,
	    wn_ant/4,
	    wn_sa/4,
	    wn_sk/3,
	    wn_syntax/3,
	    wn_ppl/4,
	    wn_per/4,
	    wn_fr/3,

	    load_wordnet/0		% force loading everything
	  ]).

/** <module> Wordnet lexical and semantic database

This module discloses the Wordnet  Prolog   files  is  a more SWI-Prolog
friendly manner. It exploits SWI-Prolog   demand-loading  and SWI-Prolog
Quick Load Files to load `just-in-time' and as quickly as possible.

The system creates Quick Load Files for  each wordnet file needed if the
.qlf file doesn't exist and  the   wordnet  directory  is writeable. For
shared installations it is adviced to   run  load_wordnet/0 as user with
sufficient privileges to create the Quick Load Files.

This library defines a portray/1 rule to explain synset ids.

Some more remarks:

	* SynSet identifiers are large numbers.  Such numbers require
	  significant more space on the stacks but not in clauses and
	  therefore it is not considered worthwhile to strip the
	  type info represented in the most significant digit.

	* On wordnet 2.0, the syntactic category deduced from the
	  synset id is consistent with the 4th argument of s/6, though
	  both adjective and adjective_satellite are represented as
	  3XXXXXXXX

### About Wordnet

Wordnet  is  a  lexical  database   for    the   English  language.  See
http://www.cogsci.princeton.edu/~wn/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/


		 /*******************************
		 *          FIND WORDNET 	*
		 *******************************/

:- multifile user:file_search_path/2.

user:file_search_path(wordnet, WNHOME) :-
	(   getenv('WNHOME', WNHOME)	->  true
	;   current_prolog_flag(windows, true)
	->  WNHOME = 'C:\\Program Files\\WordNet\\2.0'
	;   WNHOME = '/usr/local/WordNet-3.0'
	).
user:file_search_path(prolog_wn, wordnet(prolog)).

%!	wn_op(PredSpec) is nondet.
%
%	Definition of wordnet operator types.

wn_op(ant(synset_id, w_num, synset_id, w_num)).
wn_op(at(synset_id, synset_id)).
wn_op(cls(synset_id, w_num, synset_id, wn_num, class_type)).
wn_op(cs(synset_id, synset_id)).
wn_op(der(synset_id, w_num, synset_id, wn_num)).
wn_op(ent(synset_id, synset_id)).
wn_op(fr(synset_id, w_num, f_num)).
wn_op(g(synset_id, '(gloss)')).
wn_op(hyp(synset_id, synset_id)).
wn_op(ins(synset_id, synset_id)).
wn_op(mm(synset_id, synset_id)).
wn_op(mp(synset_id, synset_id)).
wn_op(ms(synset_id, synset_id)).
wn_op(per(synset_id, w_num, synset_id, w_num)).
wn_op(ppl(synset_id, w_num, synset_id, w_num)).
wn_op(s(synset_id, w_num, 'word', ss_type, sense_number, tag_count)).
wn_op(sa(synset_id, w_num, synset_id, w_num)).
wn_op(sim(synset_id, synset_id)).
wn_op(sk(synset_id, w_num, sense_key)).
wn_op(syntax(synset_id, w_num, syntax)).
wn_op(vgp(synset_id, w_num, synset_id, w_num)).


		 /*******************************
		 *    WORDNET BASIC RELATIONS   *
		 *******************************/

%! wn_ant(Antonym1, Wnum1, Antonym2, WNum2) is nondet.
%  Antonyms: synsets with opposite meanings.
wn_ant(Antonym1, Wnum1, Antonym2, WNum2) :- ant(Antonym1, Wnum1, Antonym2, WNum2).

%! wn_at(Noun, Adjective) is nondet.
%  Attribute relation: adjective pertains to concept represented by noun.
wn_at(Noun, Adjective) :- at(Noun, Adjective).

%! wn_cls(SynSet, W1, Class, W2, ClassType) is nondet.
wn_cls(SynSet, W1, Class, W2, ClassType) :- cls(SynSet, W1, Class, W2, ClassType).

%! wn_cs(SynSet, Causes) is nondet.
%  First kind of event is caused by second.
wn_cs(SynSet, Causes) :- cs(SynSet, Causes).

%! wn_der(SynSet1, W1, SynSet2, W2) is nondet.
wn_der(SynSet1, W1, SynSet2, W2) :- der(SynSet1, W1, SynSet2, W2).

%! wn_ent(SynSet, Entailment) is nondet.
%  First kind of event entails occurrence of second.
wn_ent(SynSet, Entailment) :- ent(SynSet, Entailment).

%! wn_fr(Synset, Wnum, Fnum) is nondet.
wn_fr(Synset, Wnum, Fnum) :- fr(Synset, Wnum, Fnum).

%! wn_g(SynSet, Gloss) is nondet.
wn_g(SynSet, Gloss) :- g(SynSet, Gloss).

%! wn_hyp(Hyponym, HyperNym) is nondet.
%  Subclass-superclass relation between pairs of nouns or verbs
wn_hyp(Hyponym, HyperNym) :- hyp(Hyponym, HyperNym).

%! wn_ins(A,B) is nondet.
wn_ins(A,B) :- ins(A,B).

%! wn_mm(SynSet, MemberMeronym) is nondet.
%  First kind of object is a member of second
wn_mm(SynSet, MemberMeronym) :- mm(SynSet, MemberMeronym).

%! wn_mp(SynSet, PartMeronym) is nondet.
%  First kind of object is a part of second
wn_mp(SynSet, PartMeronym) :- ms(SynSet, PartMeronym).

%! wn_ms(SynSet, SubstanceMeronym) is nondet.
%  First substance is part of second.
wn_ms(SynSet, SubstanceMeronym) :- ms(SynSet, SubstanceMeronym).

%! wn_per(Synset1, WNum1, Synset2, WNum2) is nondet.
wn_per(Synset1, WNum1, Synset2, WNum2) :- per(Synset1, WNum1, Synset2, WNum2).

%! wn_ppl(Synset1, WNum1, Synset2, WNum2) is nondet.
%  Relation between adjectival form and a verb.
wn_ppl(Synset1, WNum1, Synset2, WNum2) :- ppl(Synset1, WNum1, Synset2, WNum2).

%! wn_s(SynSet, WNum, Word, SynSetType, Sense, Tag) is nondet.
wn_s(SynSet, WNum, Word, SynSetType, Sense, Tag) :- s(SynSet, WNum, Word, SynSetType, Sense, Tag).

%! wn_sa(Synset1, WNum1, Synset2, WNum2) is nondet.
%  Relation between a single word verb its phrasal verb variant with a similar meanings.
wn_sa(Synset1, WNum1, Synset2, WNum2) :- sa(Synset1, WNum1, Synset2, WNum2).

%! wn_sim(SynSet, Similar) is nondet.
%  Similar adjectives.
wn_sim(SynSet, Similar) :- sim(SynSet, Similar).

%! wn_sk(A,B,C) is nondet.
wn_sk(A,B,C) :- sk(A,B,C).

%! wn_syntax(A,B,C) is nondet.
wn_syntax(A,B,C) :- syntax(A,B,C).

%! wn_vgp(Verb, W1, Similar, W2) is nondet.
wn_vgp(Verb, W1, Similar, W2) :- vgp(Verb, W1, Similar, W2).


		 /*******************************
		 *	   CODE MAPPINGS	*
		 *******************************/

%!	wn_cat(+SunSet, -SyntacticCategory, -Offset) is det.
%
%	Break the synset id into its   syntactic  category and offset as
%	defined in the manpage prologdb.5

wn_cat(SynSet, Category, Small) :-
	Small is SynSet mod 100000000,
	CatNum is SynSet // 100000000,
	wn_cat(CatNum, Category).

wn_cat(1, noun).
wn_cat(2, verb).
wn_cat(3, adjective).
wn_cat(4, adverb).

%!	ss_type(+Code, -Type) is det.
%!	ss_type(-Code, -Type) is nondet.
%
%	Mapping between readable syntactic category and code.

ss_type(n, noun).
ss_type(v, verb).
ss_type(a, adjective).
ss_type(s, adjective_satellite).
ss_type(r, adverb).


%!	load_wordnet is det.
%
%	Load all of wordnet.  This must be used to create all .QLF
%	files or before creating a stand-alone saved state

load_wordnet :-
	(   wn_op(O),
	    functor(O, Name, _),
	    load_op(Name),
	    fail
	;   true
	).

load_op(Name) :-
	atom_concat('wn_', Name, File),
	absolute_file_name(prolog_wn(File),
			   [ access(read),
			     file_type(prolog)
			   ],
			   PlFile),
	file_name_extension(Base, _Ext, PlFile),
	file_name_extension(Base, qlf, QlfFile),
	(   exists_file(QlfFile),
	    time_file(QlfFile, QlfTime),
	    time_file(PlFile, PlTime),
	    QlfTime >= PlTime
	->  load_files([QlfFile])
	;   access_file(QlfFile, write)
	->  qcompile(PlFile)
	;   load_files(PlFile)
	).


		 /*******************************
		 *     JUST IN TIME LOADING	*
		 *******************************/

:- multifile user:exception/3.

user:exception(undefined_predicate, wordnet:Name/Arity, retry) :-
	functor(Op, Name, Arity),
	wn_op(Op),
	load_op(Name).


		 /*******************************
		 *	      PORTRAY		*
		 *******************************/

:- multifile user:portray/1.

user:portray(SynSet) :-
	integer(SynSet),
	SynSet > 100000000,
	SynSet < 500000000,
	findall(Word, s(SynSet, _, Word, _, _, _), Words),
	Words \== [], !,
	concat_atom(Words, ', ', SS),
	format('~w (wn: ~w)', [SynSet, SS]).

