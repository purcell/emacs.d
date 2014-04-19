%%% *************************************************************
%%% Copyright (C) 2009-2013 Torsten Anders (www.torsten-anders.de) 
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License
%%% as published by the Free Software Foundation; either version 2
%%% of the License, or (at your option) any later version.
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%% *************************************************************

%%
%% This code implements the Oz-side of the Org-babel Oz interface. It
%% creates a socket server (to which org-babel-oz.el then
%% connects). Any input to this socket must be an Oz expression. The
%% input is fed to the OPI oz compiler, and the results are send back
%% via the socket.
%%


declare

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Accessing the OPI compiler
%%

MyCompiler = Emacs.condSend.compiler


/* % testing

%% Feed an expression (result browsed)
{MyCompiler enqueue(setSwitch(expression true))}
{Browse
 {MyCompiler enqueue(feedVirtualString("1 + 2" return(result: $)))}}
{MyCompiler enqueue(setSwitch(expression false))}

%% It is really the OPI: I can use declare!
{MyCompiler enqueue(setSwitch(expression false))}
{MyCompiler enqueue(feedVirtualString("declare X=3\n{Browse X*X}"))}

%% Note: expressions starting with keyword declare need keyword in 
{MyCompiler enqueue(setSwitch(expression true))}
{Browse
 {MyCompiler enqueue(feedVirtualString("declare X=3\nin X*X" return(result: $)))}}
{MyCompiler enqueue(setSwitch(expression false))}

%% Alternatively you use a session with multiple feeds: first declare (statement), and then feed an expression
{MyCompiler enqueue(setSwitch(expression false))}
{MyCompiler enqueue(feedVirtualString("declare X=7" return))}
{MyCompiler enqueue(setSwitch(expression true))}
{Browse
 {MyCompiler enqueue(feedVirtualString("X*X" return(result: $)))}}
{MyCompiler enqueue(setSwitch(expression false))}

%% !!?? does not work?
%% return nil in case of any error (division by 0)
{MyCompiler enqueue(setSwitch(expression true))}
{Browse
 {MyCompiler enqueue(feedVirtualString(
			{Accum ["try\n"
% 				       "skip\n" % do something in any case..
				       "1 div 0" % my code
% 				       "1" % my code
				       "\ncatch E then {Error.printException E}\n"
				       "error\n" % always return nil
				       "end\n"]
			 List.append}
			return(result: $)))}}
{MyCompiler enqueue(setSwitch(expression false))}


%% !! catching some exceptions does not work??

%% exception is not catched
try {Bla} catch E then {Error.printException E} {Browse nil} end

%% exception is catched
try {Browse 1 div 0} catch E then {Error.printException E} {Browse nil} end
{Browse ok}


*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Socket interface
%%


%%
%% Create socket
%%

MyPort = 6001

/** %% Creates a TCP socket server. Expects a Host (e.g., 'localhost') and a PortNo and returns a server plus its corresponding client. This client is an instance of Open.socket, and is the interface for reading and writing into the socket.
%% MakeServer blocks until the server listens. However, waiting until a connection has been accepted happens in its own thread (i.e. MakeServer does only block until the server listens).
%% NB: A port can be used only once, so assign it carefully. In case this postnnumber was shortly used before, you may need to wait a bit before reusing it.
%% */
%% !! Alternatively, let it assign automatically and output the port number..
%%
%% NOTE: for supporting multiple connections see http://www.mozart-oz.org/documentation/op/node13.html#section.sockets.accept
proc {MakeServer Host PortNo ?MyServer ?MyClient}
   proc {Accept MyClient}
      thread H in % P
	 %% suspends until a connection has been accepted
	 {MyServer accept(host:H
			  acceptClass:Open.socket  
			  accepted:?MyClient)} 
%	    {Myserver accept(host:H port:P)} % suspends until a connection has been accepted
	 %% !!?? port number of client is usually created randomly..
	 {System.showInfo "% connection accepted from host "#H}
      end
      %% !!??? 
      %% If Accept is called recursively, then server accepts multiple connections. These share the same compiler instance (e.g. variable bindings are shared). For multiple independent compiler instances call the OzServer application multiple times.
      %% However, how shall the output for multiple connections be sorted?? Would using the different client sockets created with the Server accept method work?
      %% NB: The number of clients accepted concurrently must be limited to the number set by {MyServer listen}
	 % {Accept}
   end
in
   MyServer = {New Open.socket init}
   %% To avoid problems with portnumbers, the port could be assigned automatically and then output..
   %%{MyServer bind(port:PortNo)}
   {MyServer bind(host:Host takePort:PortNo)}
   {MyServer listen}
   {System.showInfo "% OzServer started at host "#Host#" and port "#PortNo}
   MyClient = {Accept}
end
%%
MySocket = {MakeServer localhost MyPort _/*MyServer*/}


%%
%% Read socket input 
%%

declare
%% Copied from OzServer/source/Socket.oz
local
   proc {Aux Socket Size Stream}
      In = {Socket read(list:$
			size:Size)}
   in
      {Wait In}
      %% !! Is this the right way to stop the processing??
      %%
      %% abort condition when client stream ended (i.e. nothing was sent)
      if In == nil
      then {System.showInfo "socket stream ended"}
	 Stream = nil
      else Stream = In | {Aux Socket Size}
      end
   end
in
   /** %% The socket Server returns a stream of the strings it receives. The Server always waits until someone writes something into the socket, then the input is immediately written to a stream and the Server waits again.
   %% */
   proc {ReadToStream Socket Size Xs}
      thread {Aux Socket Size Xs} end
   end
end

/* % test

MyStream = {ReadToStream MySocket 1024}

*/

/* % test

%% writing
{MySocket write(vs:"this is a test")}

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Send socket input to compiler and send results back to socket
%%

%% NOTE: Input code must be expression
thread 
   {ForAll {ReadToStream MySocket 1024}
    proc {$ Code}
       Result
       %% Catch any exception (so the will not cause blocking) and return nil in that case
       FullCode = {Accum ["try\n"
% 				 "skip\n" % do something in any case..
				 Code
				 "\ncatch E then {Error.printException E}\n"
				 "error\n" % in case of an error, return 'error' 
				 "end\n"]
		   List.append}
    in
       %% ?? Should I make setting switches etc atomic?
       {MyCompiler enqueue(setSwitch(expression true))}
       {MyCompiler enqueue(feedVirtualString(FullCode return(result: ?Result)))}
       {MyCompiler enqueue(setSwitch(expression false))}
       %%
       {Wait Result}
       {MySocket write(vs: if {VirtualString.is Result}
			   then Result
			   else {Value.toVirtualString Result 1000 1000}
			   end)}
       {Show 'Org-babel result: '#Result}
    end}
end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Aux defs
%%

/** %% Binds the accumulation of the binary function Fn on all neighbors in Xs to Y. E.g., Accum returns the sum in Xs if Fn is Number.'+'.
%% */
proc {Accum Xs Fn Y}
   {List.foldL Xs.2 Fn Xs.1 Y}
end





   

