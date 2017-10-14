(* swank-mlworks.sml -- SWANK server for MLWorks
 *
 * This code has been placed in the Public Domain.
 *)

(* This is an experiment to see how the interfaces/modules would look
 * in a language with a supposedly "good" module system.
 *
 * MLWorks is probably the only SML implementation that tries to
 * support "interactive programming".  Since MLWorks wasn't maintained
 * the last 15 or so years, big chunks of the SML Basis Library are
 * missing or not the way as required by the standard.  That makes it
 * rather hard to do anything; it also shows that MLWorks hasn't been
 * "used in anger" for a long time.
 *)

structure Swank = struct

    structure Util = struct
	fun utf8ToString (v:Word8Vector.vector) : string = Byte.bytesToString v
	fun stringToUtf8 s = Byte.stringToBytes s
      end

    structure Map = struct
	datatype ('a, 'b) map = Alist of {list: ('a * 'b) list ref,
					  eq: ('a * 'a) -> bool}

	fun stringMap () =
	    Alist {list = ref [],
		   eq = (fn (x:string,y:string) => x = y)}


	fun lookup (Alist {list, eq}, key) =
	    let fun search [] = NONE
		  | search ((key', value) :: xs) =
		    if eq (key', key) then SOME value
		    else search xs
	    in search (!list)
	    end

	fun put (Alist {list, eq}, key, value) =
	    let val l = (key, value) :: (!list)
	    in list := l
	    end

      end

    structure CharBuffer = struct
	local
	    structure C = CharArray
	    datatype buffer = B of {array : C.array ref,
				    index: int ref}
	in

	fun new hint = B {array = ref (C.array (hint, #"\000")),
			  index = ref 0}

	fun append (buffer as B {array, index}, char) =
	    let val a = !array
		val i = !index
		val len = C.length a
	    in if i < len then
		   (C.update (a, i, char);
		    index := i + 1;
		    ())
	       else let val aa = C.array (2 * len, #"\000")
			fun copy (src, dst) =
			    let val len = C.length src
				fun loop i =
				    if i = len then ()
				    else (C.update (dst, i, C.sub (src, i));
					  loop (i + 1))
			    in loop 0 end
		    in copy (a, aa);
		       C.update (aa, i, char);
		       array := aa;
		       index := i + 1;
		       ()
		    end
	    end

	fun toString (B {array, index}) =
	    let val a = !array
		val i = !index
	    in CharVector.tabulate (i, fn i => C.sub (a, i)) end

	end

      end


    structure Sexp = struct
	structure Type = struct
	    datatype sexp = Int of int
			  | Str of string
			  | Lst of sexp list
			  | Sym of string
			  | QSym of string * string
			  | T
			  | Nil
			  | Quote
	  end
	open Type

	exception ReadError

	fun fromUtf8 v =
	    let val len = Word8Vector.length v
		val index = ref 0
		fun getc () =
		    case getc' () of
			SOME c => c
		     |  NONE => raise ReadError
		and getc' () =
		    let val i = !index
		    in if i = len then NONE
		       else (index := i + 1;
			     SOME (Byte.byteToChar (Word8Vector.sub (v, i))))
		    end
		and ungetc () = index := !index - 1
		and sexp () : sexp =
		    case getc () of
			#"\"" => string (CharBuffer.new 100)
		      | #"("  => lst ()
		      | #"'" => Lst [Quote, sexp ()]
		      | _ => (ungetc(); token ())
		and string buf : sexp =
		    case getc () of
			#"\"" => Str (CharBuffer.toString buf)
		      | #"\\" => (CharBuffer.append (buf, getc ()); string buf)
		      | c => (CharBuffer.append (buf, c); string buf)
		and lst () =
		    let val x = sexp ()
		    in case getc () of
			   #")" => Lst [x]
			 | #" " => let val Lst y = lst () in Lst (x :: y) end
			 | _ => raise ReadError
		    end
		and token () =
		    let val tok = token' (CharBuffer.new 50)
			val c0 = String.sub (tok, 0)
		    in if Char.isDigit c0 then (case Int.fromString tok of
						    SOME i => Int i
						  | NONE => raise ReadError)
		       else
			   Sym (tok)
		    end
		and token' buf : string =
		    case getc' () of
			NONE => CharBuffer.toString buf
		      | SOME #"\\" => (CharBuffer.append (buf, getc ());
				       token' buf)
		      | SOME #" " => (ungetc (); CharBuffer.toString buf)
		      | SOME #")" => (ungetc (); CharBuffer.toString buf)
		      | SOME c => (CharBuffer.append (buf, c); token' buf)
	    in
		sexp ()
	    end

	fun toString sexp =
	    case sexp of
		(Str s) => "\"" ^ String.toCString s ^ "\""
	      | (Lst []) => "nil"
	      | (Lst xs) => "(" ^ String.concatWith " " (map toString xs) ^ ")"
	      | Sym (name) => name
	      | QSym (pkg, name) => pkg ^ ":" ^ name
	      | Quote => "quote"
	      | T => "t"
	      | Nil => "nil"
	      | Int i => Int.toString i

	fun toUtf8 sexp = Util.stringToUtf8 (toString sexp)
      end

    structure Net = struct
	local
	    structure S = Socket
	    structure I = INetSock
	    structure W = Word8Vector

	    fun createSocket (port) =
		let val sock : S.passive I.stream_sock = I.TCP.socket ()
		    val SOME localhost = NetHostDB.fromString "127.0.0.1"
		in
		    S.Ctl.setREUSEADDR (sock, true);
		    S.bind (sock, I.toAddr (localhost, port));
		    S.listen (sock, 2);
		    sock
		end

	    fun addrToString sockAddr =
		let val (ip, port) = I.fromAddr sockAddr
		in NetHostDB.toString ip ^ ":" ^ Int.toString port
		end

	    exception ShortRead of W.vector
	    exception InvalidHexString of string
	in

	fun acceptConnection port =
	    let val sock = createSocket port
		val addr = S.Ctl.getSockName sock
		val _ = print ("Listening on: " ^ addrToString addr ^ "\n")
		val (peer, addr) = S.accept sock
	    in
		S.close sock;
		print ("Connection from: " ^ addrToString addr ^ "\n");
		peer
	    end

	fun receivePacket socket =
	    let val v = S.recvVec (socket, 6)
		val _ = if W.length v = 6 then ()
			else raise ShortRead v
		val s = Util.utf8ToString v
		val _ = print ("s = " ^ s ^ "\n")
		val len =
		    case StringCvt.scanString (Int.scan StringCvt.HEX) s of
			SOME len => len
		      | NONE => raise InvalidHexString s
		val _ = print ("len = " ^ Int.toString len ^ "\n")
		val payload = S.recvVec (socket, len)
		val plen = W.length payload
		val _ = print ("plen = " ^ Int.toString plen ^ "\n")
		val _ = if plen = len then ()
			else raise ShortRead payload
	    in
		payload
	    end

	fun nibbleToHex i:string = Int.fmt StringCvt.HEX i

	fun loadNibble i pos =
	    Word32.toInt (Word32.andb (Word32.>> (Word32.fromInt i,
						  Word.fromInt (pos * 4)),
				       0wxf))

	fun hexDigit i pos = nibbleToHex (loadNibble i pos)

	fun lenToHex i =
	    concat [hexDigit i 5,
		    hexDigit i 4,
		    hexDigit i 3,
		    hexDigit i 2,
		    hexDigit i 1,
		    hexDigit i 0]

	fun sendPacket (payload:W.vector, socket) =
	    let val len = W.length payload
		val header = Util.stringToUtf8 (lenToHex len)
		val packet = W.concat [header, payload]
	    in  print ("len = " ^ Int.toString len ^ "\n"
		       ^ "header = " ^ lenToHex len ^ "\n"
		       ^ "paylad = " ^ Util.utf8ToString payload ^ "\n");
		S.sendVec (socket, {buf = packet, i = 0, sz = NONE})
	    end

	end
      end

    structure Rpc = struct
	open Sexp.Type

	val funTable : (string, sexp list -> sexp) Map.map
	    = Map.stringMap ()

	fun define name f = Map.put (funTable, name, f)

	exception UnknownFunction of string
	fun call (name, args) =
	    (print ("call: " ^ name ^ "\n");
	     case Map.lookup (funTable, name) of
		 SOME f => f args
	       | NONE => raise UnknownFunction name)


	local fun getpid () =
	    Word32.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ()))
	in
	fun connectionInfo [] =
	    Lst [Sym ":pid", Int (getpid ()),
		 Sym ":lisp-implementation", Lst [Sym ":type", Str "MLWorks",
						  Sym ":name", Str "mlworks",
						  Sym ":version", Str "2.x"],
		 Sym ":machine", Lst [Sym ":instance", Str "",
				      Sym ":type", Str "",
				      Sym ":version", Str ""],
		 Sym ":features", Nil,
		 Sym ":package", Lst [Sym ":name", Str "root",
				      Sym ":prompt", Str "-"]]
	end

	fun nyi _ = Nil

	local structure D = Shell.Dynamic
	in
	fun interactiveEval [Str string] =
	    let val x = D.eval string
	    in Str (concat [D.printValue x, " : ", D.printType (D.getType x)])
	    end
	end

	val _ =
	    (define "swank:connection-info" connectionInfo;
	     define "swank:swank-require" nyi;
	     define "swank:interactive-eval" interactiveEval;
	     ())
      end

    structure EventLoop = struct
	open Sexp.Type

	fun execute (sexp, pkg) =
	    (print ("sexp = " ^ (Sexp.toString sexp) ^ "\n");
	     case sexp of
		 Lst (Sym name :: args) => Rpc.call (name, args))

	fun emacsRex (sexp, pkg, id as Int _, sock) =
	    let val result = (Lst [Sym (":ok"), execute (sexp, pkg)]
			      handle exn => (Lst [Sym ":abort",
						  Str (exnName exn ^ ": "
						       ^ exnMessage exn)]))
		val reply = Lst [Sym ":return", result, id]
	    in Net.sendPacket (Sexp.toUtf8 reply, sock)
	    end

	fun dispatch (Lst ((Sym key) :: args), sock) =
	    case key of
		":emacs-rex" => let val  [sexp, pkg, _, id] = args
				in emacsRex (sexp, pkg, id, sock)
				end

	fun processRequests socket:unit =
	    let val sexp = Sexp.fromUtf8 (Net.receivePacket socket)
	    in print ("request: "
		      ^ Util.utf8ToString (Sexp.toUtf8 sexp)
		      ^ "\n");
	       dispatch (sexp, socket);
	       processRequests socket
	    end

      end

    (* val _ = EventLoop.processRequests (Net.acceptConnection 4005) *)
    val _ = ()
  end

(* (Swank.EventLoop.processRequests (Swank.Net.acceptConnection 4005)) *)
